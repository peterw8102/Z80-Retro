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



          extrn  SPI_BEG, SPI_END, SPI_INIT, SPI_ACMD, SPI_TXB, SPI_SEL
          extrn  SPI_RXB, SPI_RBF, SPI_CM1, SPI_CMN, SPI_BRD, SPI_BWR

          ; For debug
          ; extrn  WRITE_8, WRITE_16

          ; Operational exports
          public SD_SEL, SD_INIT, SD_RST, SD_WBLK, SD_RBLK, SD_PRES

          ; Debug symbols
          public _RSET, _SCND, _OCR, _AINIT, _SBLK, _CMDBUF
          CSEG

; ------ SD_RST
; Force SDCard re-initialisation. This would be required if the
; SDCard in one of the carriers in replaced (or card removed then
; re-inserted). Without calling SD_RST the card won't be reset and
; placed into the correct operational mode.
SD_RST:  XOR     A
         LD      (SD_STAT+1),A   ; Card 0 initialisation status
         LD      (SD_STAT+4),A   ; Card 1 initialisation status
         RET

; ------ SD_PRES
; The function takes no parameters and returns a two bit result
; in the accumulator (A):
;    Bit 0: Card 0 presense
;    Bit 1: Card 1 presense
; The value of each bit will be either:
;     0: There is an adaptor fitted but there's no card inserted
;     1: Either there's no adaptor OR there is an adaptor and there
;        is a card inserted into the adaptor.
; NOTE:
; The hardware can't distinguish between card present and adaptor
; missing due to the way the adaptor hardware works. As such it's
; recommended that slot 0 at least have an SDCard adaptor even if
; not used.
SD_PRES:  IN         A,(SPIIN)
          AND        SDC0_PR | SDC1_PR
          RRCA
          RRCA
          RRCA
          AND        3
          RET


; ------ SD_TST
; Test whether the currently selected SDCard is installed.
; This function returns the SPI device number for the selected
; SDCard. Card 0 returns 0, card 1 returns 1.
;
; Return:
;     Z flag SET if the card is NOT installed.
;    HL points to SD_STAT block.
;     A is the SPI device ID for the selected SDCard
SD_TST:   OR        A             ; Which drive?
          IN        A,(SPIIN)
          LD        HL,SD_STAT+2
          JR        NZ,_sd1
          JR        _tst1

_sd1:     LD        HL,SD_STAT+5
_tst1:    AND       (HL)          ; Device number
          DEC       HL
          DEC       HL
          LD        A,(HL)
          RET                     ; Do nothing if there's no card

; ------ SD_SEL
; Select one of the SDCards (0 or 1)
; A: the device number. Anything other than 0 or 1 will cause no device to
; be selected.
SD_SEL:   PUSH       BC
          PUSH       HL
          LD         B,A
          LD         A,$FE
          AND        B           ; Should be zero
          JR         NZ,_baddev
          ; Check whether that drive contains a card
          LD         A,B
          CALL       SD_TST
          ; Record the requested card even if it is not present. Previously
          ; this returned without changing anything, so the following
          ; operations went to the previously selected card (a write to an
          ; absent card 1 ended up on card 0).
          PUSH       AF          ; Z = no card
          LD         A,B
          LD         (CUR_DEV),A
          LD         (CUR_SD),HL
          POP        AF
          LD         A,B
          JR         NZ,_sdsel
_baddev:  LD         A,5         ; No card or invalid number: no chip select active
_sdsel:   CALL       SPI_SEL
          POP        HL
          POP        BC
          RET


; ------ SD_INIT
; Initialise the currently selected SDCard (set via SD_SEL)
;
; Initialises the SPI interface and then sets the correct operational
; mode for the SDCard.
;
; This function MUST be called before the read/write block operations. This
; function can be called multiple times and should be called before all
; read/write operations.
;
; The SDCard is set to have a block size of 512 bytes.
;
SD_INIT:  PUSH      HL
          PUSH      DE
          PUSH      BC
          LD        A,(CUR_DEV)     ; Currently installed device
          CALL      SD_TST
          JR        NZ,_havdev      ; If zero then card not installed.

          ; There's no device. HL points to the device control block. Clear the init flag
          ; so that next time, if the card has been inserted, it will be properly initialised.
          XOR       A
          INC       HL
          LD        (HL),A
          JR        _fin

_havdev:  CALL      SPI_SEL         ; SD_TST returns the SPI device number. Select it.

          ; Does this card need to be initialised?
          INC       HL
          LD        A,(HL)
          OR        A
          JR        Z,_doinit       ; Card needs initialisation.
          CALL      SPI_END
          JR        _fin

_doinit:  PUSH      HL              ; HL -> initialisation flag of this card
          CALL      SPI_INIT        ; Initialise the SPI interface
          CALL      SPI_END

          ; SD Card initialisation sequence. Every step now has a retry limit:
          ; previously a card that did not answer (or a socket without an
          ; adapter, which reads as "card present") hung the machine forever.
          CALL      _RSET
          JR        NZ,_ifail       ; Card never entered the idle state
          CALL      _SCND           ; CMD8: is this a v2 card (SDHC possible)?
          CALL      _OCR
          CALL      _AINIT
          OR        A
          JR        Z,_ifail        ; ACMD41 did not complete (timeout or error)
          CALL      _OCR            ; Final OCR: CCS bit = block addressing

          ; Set block size to 512 bytes
          CALL      _SBLK

          ; Flag: 1 = SDSC card (byte addresses), 2 = SDHC/SDXC (block addresses)
          LD        A,(_CMDBUF+1)   ; First OCR byte (bits 31-24)
          AND       40h             ; CCS
          LD        A,1
          JR        Z,_isdsc
          INC       A
_isdsc:   POP       HL
          LD        (HL),A
          OR        A               ; NZ: initialised (as before)
          JR        _fin

_ifail:   POP       HL
          XOR       A               ; Left uninitialised: retried on the next call
          LD        (HL),A
_fin:     POP       BC
          POP       DE
          POP       HL
          RET

; _RSET: Send a card reset command (CMD0) until the card answers 01 (idle).
; Returns Z on success, NZ after 32 failed attempts.
_RSET:    LD         A,32
_rset1:   PUSH       AF
          LD         BC,9540h    ; CMD0, CRC 95
          CALL       SPI_CMN
          PUSH       AF
          CALL       SPI_END
          POP        AF
          JR         C,_rsetto   ; MISO stuck low: no point retrying
          DEC        A
          JR         Z,_rsetok
          POP        AF
          DEC        A
          JR         NZ,_rset1   ; Start again
          INC        A           ; NZ: failed
          RET
_rsetto:  POP        AF
          OR         1           ; NZ: failed
          RET
_rsetok:  POP        AF
          XOR        A           ; Z: success
          RET

; ----------- SEND_IF_COND --------
; CMD 08
; Response is stored in the _CMDBUF structure. If the card answers with the
; pattern (R1 = 01 and echo AA) it is a v2 card and ACMD41 is sent with HCS = 1.
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

          LD         HL,0           ; v1 (or no answer): HCS = 0
          LD         A,(_CMDBUF)
          CP         1
          JR         NZ,_scnd1
          LD         A,(_CMDBUF+4)
          CP         0AAh
          JR         NZ,_scnd1
          LD         H,40h          ; v2: HCS = 1 (bit 30 of the argument)
_scnd1:   LD         (_HCS),HL
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

; ----------- ACMD41 --------
; Repeats CMD55 + ACMD41 until the card leaves the idle state. Previously the
; argument was always 0 (HCS = 0): an SDHC card (over 2 GB) then never leaves
; the idle state, and the loop had no limit. Now HCS is set from the CMD8
; result and the loop gives up after 255 attempts (~2 s at 7.37 MHz).
; Returns A = 1 when the card is ready, A = 0 otherwise.
_AINIT:   LD         A,255
_ain1:    LD         (_ATRY),A
          CALL       SPI_ACMD       ; Prefix command

          ; If we didn't get success at this point then wait and retry
          LD         A,0FEh
          AND        B              ; Result from the ACMD command
          LD         A,0
          RET        NZ

          LD         BC,0ff40h+41
          LD         HL,(_HCS)
          LD         DE,0
          CALL       SPI_CM1

          ; Wait for zero byte
          OR         A
          CALL       SPI_END
          LD         A,1
          RET        Z
          CALL       _PAUSE
          LD         A,(_ATRY)
          DEC        A
          JR         NZ,_ain1
          RET                       ; A = 0: timed out

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
;
; NOTE: The correct drive must have been selected in SD_INIT BEFORE attempting
; to read or write from a drive.
SD_WBLK:  CALL       SD_INIT        ; Writes did not initialise the card: if the first
                                    ; operation after power-up was a write (e.g. WI or
                                    ; SW) the card was still idle, rejected the command
                                    ; and the error was lost. SD_INIT preserves BC, DE
                                    ; and HL and does nothing if already initialised.
          CALL       _PRSNT         ; No card selected: do nothing
          JR         NZ,_wrgo
          DEC        A              ; A = FF, NZ
          RET
_wrgo:    CALL       SCL_AD
          PUSH       BC             ; Save buffer address
          ; Send command...
          LD         BC,0FF40h + 24
          CALL       SPI_CM1

          OR         A
          LD         H,A            ; R1 as the error code if aborted
          JR         NZ,_abrt_wr

          ; Wait for ready (read FF byte from SD card). Previously unbounded.
          LD         DE,8000
_a:       CALL       SPI_RXB
          INC        A
          JR         Z,_wrrdy
          DEC        DE
          LD         A,D
          OR         E
          JR         NZ,_a
          DEC        A              ; A = FF: timed out
          LD         H,A
          JR         _abrt_wr
_wrrdy:
          ; Send the start token
          LD         A,0FEh
          CALL       SPI_TXB

          ; Write out the block...
          LD         B,00h          ; 256x2 bytes
          POP        HL             ; Get the block address back but into HL where we can use it.
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
_ec:      CALL       SPI_END
          LD         A,H
          ; Returning Z flag set if we got the expected result.
          CP         5
          RET

_abrt_wr: POP        BC
          CALL       SPI_END
          LD         A,H
          OR         A              ; NZ: error (A = R1 or FF)
          RET

; ----------- READ BLOCK -----------
; SDCard address is a 512byte sector number which needs to be scaled up to
; a byte address. Eventually the scaling will depend on the SDCard formats
; supported. Right now we only support the original cards which have a
; byte addressing scheme.
; HL  - SD address upper word...
; DE  - SD address lower word...
; BC  - Target buffer
SD_RBLK:  CALL       _PRSNT             ; Presence of the SELECTED card
          JR         NZ,_rdgo           ; (previously always checked card 0)
          DEC        A                  ; No card: A = FF, NZ
          RET
_rdgo:
          CALL       SCL_AD
          PUSH       BC                 ; Address of buffer to receive data
          LD         BC,0FF40h + 17     ; Command code
          CALL       SPI_CM1
          OR         A                  ; R1 not 0: the card rejected the command.
          JR         NZ,_rderr          ; Previously we waited for the token forever.

          ; Wait for the FE data token. While the card prepares the block it
          ; sends FF; a 0000xxxx byte is an error token. Limit ~0.4 s.
          LD         DE,8000
_tok1:    CALL       SPI_RBF           ; Don't care about HL
          CP         0FEh
          JR         Z,_gtok
          CP         0FFh
          JR         NZ,_rderr          ; Error token (returned as is)
          DEC        DE
          LD         A,D
          OR         E
          JR         NZ,_tok1
          DEC        A                  ; A = FF: timed out

_rderr:   POP        DE                 ; Discard the buffer address
          LD         H,A
          CALL       SPI_END
          LD         A,H
          OR         A                  ; NZ and A = error code
          RET

_gtok:    POP        DE                ; Where to write the data

          ; Block read 512byte block
_nrb:     LD         B,0               ; Number of 16 bit words to read. Read 256x2 bytes
          CALL       SPI_BRD
          CALL       SPI_RBF           ; CRC (2 bytes, unused): reading it leaves the
          CALL       SPI_RBF           ; card ready for the next command

          CALL       SPI_END
          XOR        A                 ; Z and A = 0: success
          RET

; _PRSNT: Z if the selected card (CUR_SD) is not present.
; Preserves BC, DE and HL.
_PRSNT:   PUSH       HL
          LD         HL,(CUR_SD)
          INC        HL
          INC        HL
          IN         A,(SPIIN)
          AND        (HL)
          POP        HL
          RET

; -------------- SCALE BLOCK ADDRESS ------------
; This library currently only supports the original byte addressed SDCards with a maximum
; size of 4GB. The address accepted by the API is a 512 byte sector address so need to
; left shift 9 bits.
SCL_AD:  PUSH  HL      ; SDHC/SDXC card (flag 2): the command takes the
         LD    HL,(CUR_SD) ; block number as is, unscaled.
         INC   HL
         LD    A,(HL)
         POP   HL
         CP    2
         RET   Z
         LD    H,L     ; Multiple by 256
         LD    L,D
         LD    D,E
         LD    E,0

         SLA   D       ; And then by 2 to get to 512 block address
         RL    L
         RL    H
         RET

          DSEG

; Maintain two SDCard status blocks. Bytes in ech block are:
;    0: the SPI device ID (0 or 1)
;    1: 0 = not initialised, 1 = SDSC (byte addresses), 2 = SDHC/SDXC (block addresses)
;    2: The presense bit for this device
CUR_SD   DW    SD_STAT   ; Points to current SD_STAT block (card 0 by default)
CUR_DEV  DB    0         ; Current SDCard (default to card zero)
SD_STAT  DB    0         ; SDCard 1 SPI device ID
         DB    0         ; Set to !0 once initialised
         DB    SDC0_PR   ; Bit mask for this card's presense
         DB    1         ; SDCard 2 SPI device ID
         DB    0         ; Set to !0 once initialised
         DB    SDC1_PR   ; Bit mask for this card's presense

; _CMDBUF - small buffer for short command responses that can be returned to the caller.
_CMDBUF:   DEFS     10      ; For short response data
_HCS:      DW       0       ; High word of the ACMD41 argument (4000h = HCS)
_ATRY:     DB       0       ; ACMD41 attempts left

.END
