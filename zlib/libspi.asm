import ../zlib/defs.asm

          extrn  ADD8T16

          public SPI_SEL,SPI_BEG,SPI_STRT,SPI_END,SPI_FIN,SPI_INIT,SPI_ACMD,SPI_TXB
          public SPI_RXB,SPI_RBF,SPI_CM1,SPI_CMN,SPI_BRD,SPI_BWR

; SPI_SBI
; Read a single bit into the LSB of L. H contains the value to be written to the OP port (bit 1)
SPI_SBI   MACRO
    LD     A,H
    OUT    (SPIPORT_L),A      ; Set Data with clock LOW
    OUT    (SPIPORT_H),A      ; Make clock high. SD card *should* have shifted a bit out that we can sample.
    IN     A,(SPIIN)          ; Read in the next bit.
    RRA                       ; LSB into C...
    RL     L                  ; ...and from C to LSB of L
    ENDM

; A contains SPICMD on entry
; SPI_SBO
; Send the MSBit of D to the slave, leaving D shifted by one position.
SPI_SBO   MACRO
    local _spi_tx
    XOR    A
    SLA    D                ; MSB into Carry
    RLA                     ; Move that bit into the accumulator
_spi_tx:
    OUT    (SPIPORT_L),A    ; Output data and clock LOW
    OUT    (SPIPORT_H),A    ; Clock high. SD car *should* have shifted out it's first bit that we can sample.
    ENDM

          CSEG

; 5 SPI devices are available. These are numbered 0-4. This table identifies
; which chip select line applies to each of these devices.
_CS        DB    SPICS0_E       ; SDCard 0
           DB    SPICS1_E       ; SDCard 1
           DB    SPICS2_E       ; SPI Device 2
           DB    SPICS3_E       ; SPI Device 3
           DB    SPICS4_E       ; SPI Device 4


; ---- SPI_SEL
; Select an SPI device. Device ID in A must be between 0 and 4. MUST call
; this method BEFORE making data requests to SPI. If the device is NOT selected
; then access fails.
SPI_SEL:  CP    5
          JR    NC,_baddev     ; Out of range.
          PUSH  HL
          LD    HL,_CS         ; Translate device number into chip select
          CALL  ADD8T16
          LD    A,(HL)
          POP   HL
          LD    (DEVSEL),A

          RET

_baddev:  LD    A,SPICS_DS      ; Clear the device select number so requests aren't sent to wrong device.
          LD    (DEVSEL),A
          RET

SPI_STRT: LD    A,SPICS_DS
          OUT   (SPICS_P),A    ; Deselect ALL devices
          XOR   A
          OUT   (SPIPORT_L),A  ; Data:0 CLK: 0

          ; We want SPI mode 0 so clk low when we take CS active. Activate CS.
          LD    A,(DEVSEL)     ; Get mask for selected device
          OUT   (SPICS_P),A
          RET

SPI_INIT: ; Set clock and CS LOW (inactive)
          CALL  SPI_STRT

          ; Send ~80 clock transitions
          LD    B,10
_nidle:   CALL  SPI_RXB
          DJNZ  _nidle
          RET

; ---- SPI_BEG - Assert CS and then send 2x dummy bytes, discarding the results.
; A: Not maintined
SPI_BEG:  LD    A,(DEVSEL)     ; Get mask for selected device
          OUT   (SPICS_P),A
          CALL  SPI_RXB        ; +2x dummy clock bytes
_bsy:     CALL  SPI_RXB
          INC   A              ; Expect 0xFF
          JR    NZ,_bsy
          RET

; ---- SPI_END - End of a command. Clock LOW then remove CS
SPI_END:  LD    A,0
          OUT   (SPIPORT_L),A
SPI_FIN:  LD    A,SPICS_DS     ; Then take CS off as well.
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
          LD         B,128
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

          DSEG
DEVSEL:   DB       SPICS_DS      ; Used to store which SPI device to access.

.END
