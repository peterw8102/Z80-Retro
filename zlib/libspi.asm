import ../zlib/defs.asm

          public SPI_BEG, SPI_END, SPI_INIT, SPI_ACMD, SPI_TXB
          public SPI_RXB, SPI_RBF, SPI_CM1, SPI_CMN, SPI_BRD, SPI_BWR

          CSEG

SPI_INIT: ; Set clock and CS LOW (inactive)
          LD    A,SPICS_DS
          OUT   (SPICS_P),A    ; card NOT selected
          XOR   A
          OUT   (SPIPORT_L),A  ; Data:0 CLK: 0

          ; We want SPI mode 0 so clk low when we take CS active. Activate CS.
          LD    A,SPICS_EN
          OUT   (SPICS_P),A

          ; Send ~80 clock transitions
          LD    B,10
_nidle:   CALL  SPI_RXB
          DJNZ  _nidle
          RET

; ---- SPI_BEG - Assert CS and then send 2x dummy bytes, discarding the results.
; A: Not maintined
SPI_BEG:  LD    A,SPICS_EN     ; Enable CS card
          OUT   (SPICS_P),A
          CALL  SPI_RXB        ; +2x dummy clock bytes
_bsy:     CALL  SPI_RXB
          INC   A              ; Expect 0xFF
          JR    NZ,_bsy
          RET

; ---- SPI_END - End of a command. Clock LOW then remove CS
SPI_END:  LD    A,0
          OUT   (SPIPORT_L),A
          LD    A,SPICS_DS     ; Then take CS off as well.
          OUT   (SPICS_P),A
          RET



; ---- SPI_CMN - Send a command with null parameter (zeros)
SPI_CMN: LD         HL,0
         LD         DE,0
          ; AND drop through to SPI_CMN

; ---- SPI_CM1 - Send a specific command. Note this command does NOT
; send a valid CRC so can't be used where commands need that. This function
; returns the first response byte. It does NOT end the command so this can
; be used to start a block command.
; HL:  Upper 16 bits of arg
; DE:  Lower 16 bits of arg
; B:   CRC
; C:   COMMAND
SPI_CM1:  CALL       SPI_BEG
          LD         A,C
          CALL       SPI_TXB
          LD         A,H
          CALL       SPI_TXB
          LD         A,L
          CALL       SPI_TXB
          LD         A,D
          CALL       SPI_TXB
          LD         A,E
          CALL       SPI_TXB
          LD         A,B
          CALL       SPI_TXB
          ; Now read bytes until we get a non-FF response
          LD         B,8
_r1w:     CALL       SPI_RXB
          INC        A
          JR         NZ,_r1wd
          DJNZ       _r1w
          ; Passed or timeout
_r1wd:    DEC        A
          RET

; ---- Send an application command
SPI_ACMD: LD         BC,00040h+55
          CALL       SPI_CMN
          LD         B,A
          JR         SPI_END

; In many cases we don't care about the received byte this optimised routine sends a byte and ignores the response.
SPI_TXB:   EXX
           LD     D,A        ; A contains the byte to be send. Save in H

           ; H: Byte to be sent
           SPI_SBO
           SPI_SBO
           SPI_SBO
           SPI_SBO

           SPI_SBO
           SPI_SBO
           SPI_SBO
           SPI_SBO

           EXX
           RET

           ; In many cases we just need to send FF and receive a byte
SPI_RXB:   EXX         ; L holds the byte we're receiving

           LD     H,1 ; Send bit 0 from H and read the response
           ; H: Byte to be sent
           ; L: Byte being received
           SPI_SBI
           SPI_SBI
           SPI_SBI
           SPI_SBI

           SPI_SBI
           SPI_SBI
           SPI_SBI
           SPI_SBI

           LD     A,L
           EXX
           RET

SPI_RBF:  ; L holds the byte we're receiving - HL NOT SAVED

          LD     H,1 ; Send bit 0 from H and read the response (i.e. send 8x1)
          ; H: Constant value to write out to write a '1'
          ; L: Byte being received
          SPI_SBI
          SPI_SBI
          SPI_SBI
          SPI_SBI

          SPI_SBI
          SPI_SBI
          SPI_SBI
          SPI_SBI

          LD     A,L
          RET

; -- SPI_BRD
; Block read. Highly optimised block read function.
; Inputs:
;   DE: Address of buffer into which to store the read data.
;   B:  Number of 16 bit words to tansfers. Note this means only
;       EVEN number of bytes can be transferred. Max. 512 bytes (B=0)
; Outputs:
;   DE: Points one byte after the last received bytes.
;   A:  Not saved
SPI_BRD:  PUSH     HL

          LD       H,1        ; Bit value to send. L holds the received byte.
_brn:     SPI_SBI
          SPI_SBI
          SPI_SBI
          SPI_SBI

          SPI_SBI
          SPI_SBI
          SPI_SBI
          SPI_SBI

          LD         A,L
          LD         (DE),A
          INC        DE

          SPI_SBI
          SPI_SBI
          SPI_SBI
          SPI_SBI

          SPI_SBI
          SPI_SBI
          SPI_SBI
          SPI_SBI

          LD         A,L
          LD         (DE),A
          INC        DE

          DJNZ       _brn


          POP      HL
          RET

; -- SPI_BWR
; Block write. Highly optimised block write function.
; Inputs:
;   HL: Address of buffer from which to read data to send to SPI
;   B:  Number of 16 bit words to tansfers. Note this means only
;       EVEN number of bytes can be transferred. Max. 512 bytes (B=0)
; Outputs:
;   DE: Points one byte after the last sent bytes.
;   A:  Not saved
SPI_BWR:  PUSH     DE            ; scratch pad

_bwn:     LD       D,(HL)        ; Bit value to send. L holds the received byte.
          INC      HL
          SPI_SBO
          SPI_SBO
          SPI_SBO
          SPI_SBO

          SPI_SBO
          SPI_SBO
          SPI_SBO
          SPI_SBO

          LD       D,(HL)
          INC      HL

          SPI_SBO
          SPI_SBO
          SPI_SBO
          SPI_SBO

          SPI_SBO
          SPI_SBO
          SPI_SBO
          SPI_SBO

          DJNZ       _bwn


          POP      DE
          RET
.END
