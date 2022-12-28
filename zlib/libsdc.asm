import ../zlib/defs.asm

; Functions for reading and writing to/from an SDCard. Currently only cards up
; to 2GB are supported. The SDCards is connected/accessed via the SPI interface.
;
; Note that this library only currently supports the first SDCard. Version 2 of
; the board added hardware support for a second SDCard however this is not yet
; supported in this library.
;
; Exported Functions
; SD_INIT: Initialise the SPI bus and the SDCard. MUST be called before read/write ops
; SD_WBLK:
; SD_RBLK:
;
; SDCard access is not trivial. The following references have been particularly useful
; in reverse engineering the SDCard command interface:
;
;    http://elm-chan.org/docs/mmc/mmc_e.html
;    https://www.microchip.com/forums/m530149.aspx


          extrn  SPI_BEG, SPI_END, SPI_INIT, SPI_ACMD, SPI_TXB
          extrn  SPI_RXB, SPI_RBF, SPI_CM1, SPI_CMN, SPI_BRD, SPI_BWR

          ; Operational exports
          public SD_INIT, SD_WBLK, SD_RBLK, SD_PRES

          ; Debug symbols
          public _RSET, _SCND, _OCR, _AINIT, _SBLK, _CMDBUF
          CSEG

; ------ SD_INIT
; Initialise the SDCard ready to be used. Initialises the SPI interface
; and then sets the correct operational mode for the SDCard.
;
; This function MUST be called before the read/write block operations. This
; function can be called multiple times. There are no parameters.
;
; The SDCard is set to have a block size of 512 bytes,
SD_INIT:  IN        A,(SPIIN)
          AND       SDC0_PR
          RET       Z
          CALL     SPI_INIT        ; Initialise the SPI interface
          CALL     SPI_END

          ; SD Card initialisation sequence
          CALL      _RSET
          CALL      _SCND
          CALL      _OCR
          CALL      _AINIT
          CALL      _OCR

          ; Set block size to 512 bytes
          CALL      _SBLK
          XOR       A
          INC       A
          RET

; ------ SD_PRES
; Report the presence for the two available SD card slots.
; Return
;  A - bit 0: SDCard 0, bit 1: SDCard 1
SD_PRES:  IN         A,(SPIIN)
          AND        SDC0_PR | SDC1_PR
          RRCA
          RRCA
          RRCA
          AND        3
          RET

; _RSET: Send a card reset command.
_RSET:    LD         BC,9540h    ; CMD0, CRC 95
          CALL       SPI_CMN
          PUSH       AF
          CALL       SPI_END
          POP        AF
          DEC        A
          JR         NZ,_RSET    ; Start again
          RET

; ----------- SEND_IF_COND --------
; CMD 08
; Response is stored in the _CMDBUF structure
_SCND:    LD         BC,8740h + 8
          LD         HL,0
          LD         DE,01AAh
          CALL       SPI_CM1

          LD         DE,_CMDBUF
          LD         (DE),A
          INC        DE

          ; A contains the first R1 response byte
          ; Next 4 bytes are the status. Read 4 bytes (2 words)
          LD         B,2
          CALL       SPI_BRD
          CALL       SPI_END

          LD         HL,_CMDBUF
          RET

; ----------- CMD58 --------
_OCR:     LD         BC,0FF40h+58
          CALL       SPI_CMN

          LD         DE,_CMDBUF
          LD         (DE),A
          INC        DE

          ; And the next 4 response bytes
          LD         B,2
          CALL       SPI_BRD
          CALL       SPI_END

          LD         HL,_CMDBUF
          RET

;
_retry:   CALL       _PAUSE
_AINIT:   CALL       SPI_ACMD       ; Prefix command

          ; If we didn't get success at this point then wait and retry
          LD         A,0FEh
          AND        B              ; Result from the ACMD command
          LD         A,0
          RET        NZ

          LD         BC,0ff40h+41
          CALL       SPI_CMN

          ; Wait for zero byte
          OR         A
          CALL       SPI_END
          JR         NZ,_retry
          LD         A,1
          RET

; ----------- SET BLOCK SIZE --------
; CMD 16 - Set 512 BYTE BLOCK SIZE
_SBLK:    LD         BC,0FF40h+16
          LD         HL,0
          LD         DE,512      ; block size of 512 bytes
          CALL       SPI_CM1
          LD         D,A
          CALL       SPI_END
          RET

_PAUSE:  PUSH  HL
         LD    HL,0FFFh
         JR    del_2
         PUSH  HL
         LD    H,001H
del_1:   LD    L,0FFH
del_2:   DEC   L
         JR    NZ,del_2
         DEC   H
         JR    NZ,del_1
         POP   HL
         RET


; ----------- WRITE BLOCK -----------
; SDCard address is a 512byte sector number which needs to be scaled up to
; a byte address. Eventually the scaling will depend on the SDCard formats
; supported. Right now we only support the original cards which have a
; byte addressing scheme.
;
; ALL WRITES ARE 512 BYTES!!!
;
; HL  - SD address upper word...
; DE  - SD address lower word...
; BC  - Points to the buffer containing the data to write.
SD_WBLK:  IN        A,(SPIIN)
          AND       SDC0_PR
          RET       Z
          CALL       SCL_AD
          PUSH       BC             ; Save buffer address
          ; Send command...
          LD         BC,0FF40h + 24
          CALL       SPI_CM1

          OR         A
          JR         NZ,_abrt_wr

          ; Wait for ready (read zero byte from SD card)
_a:       CALL       SPI_RXB
          INC        A
          JR         NZ,_a

          ; Send the start token
          LD         A,0FEh
          CALL       SPI_TXB

          ; Write out the block...
          LD         B,00h          ; 256x2 bytes
          POP        HL             ; Get the block address back but into DE where we can use it.
          CALL       SPI_BWR

          ; 2 byte CRC (NOT USED)
          CALL       SPI_RXB
          CALL       SPI_RXB

          ; Next inbound byte is the response code
          CALL       SPI_RXB
          AND        1Fh
          CP         5
          LD         H,A
          ; End of command
_abrt_wr: CALL       SPI_END
          LD         A,H
          ; Returning Z flag set if we got the expected result.
          RET

; ----------- READ BLOCK -----------
; SDCard address is a 512byte sector number which needs to be scaled up to
; a byte address. Eventually the scaling will depend on the SDCard formats
; supported. Right now we only support the original cards which have a
; byte addressing scheme.
; HL  - SD address upper word...
; DE  - SD address lower word...
; BC  - Target buffer
SD_RBLK:  IN        A,(SPIIN)
          AND       SDC0_PR
          RET       Z

          CALL       SCL_AD
          PUSH       BC                 ; Address of buffer to receive data
          LD         BC,0FF40h + 17     ; Command code
          CALL       SPI_CM1

          LD         B,0FEh
          CMP        B
          JR         Z,_gtok

_tok1:    CALL       SPI_RBF           ; Don't care about HL
          CMP        B
          JR         NZ,_tok1

_gtok:    POP        DE                 ; Where to write the data

          ; Block read 512byte block
_nrb:     LD         B,0                ; Number of 16 bit words to read. Read 256x2 bytes
          CALL       SPI_BRD

          CALL       SPI_END
          RET

; -------------- SCALE BLOCK ADDRESS ------------
; This library currently only supports the original byte addressed SDCards with a maximum
; size of 4GB. The address accepted by the API is a 512 byte sector address so need to
; left shift 9 bits.
SCL_AD:  LD    H,L     ; Multiple by 256
         LD    L,D
         LD    D,E
         LD    E,0

         SLA   D       ; And then by 2 to get to 512 block address
         RL    L
         RL    H
         RET

          DSEG

; _CMDBUF - small buffer for short command responses that can be returned to the caller.
_CMDBUF:   DEFS     10      ; For short response data

.END
