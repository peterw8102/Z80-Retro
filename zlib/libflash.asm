import defs.asm

  ; Utility functions for writing to onboard flash storage.
  ;
  public FL_ID,FL_CSECT,FL_CPAGE,FL_WSECT,FL_WPAGE

POFFSET   EQU 0100000000000000b

; Translate a 15 bit command address to a CPU address mode and write
CMD_CODE MACRO addr,data
    LD     HL,addr+POFFSET
    LD     (HL),data
    ENDM

; Command address is in DE. Add the offset and make the write. The
; data is a literal.
CMD_DE MACRO data
    LD     HL,POFFSET
    ADD    HL,DE
    LD     (HL),data
    ENDM

          CSEG

; ------------------- FLSH_ID - Read Flash Info
; No parameters. Read the two ID bytes from the flash device.
; OUTPUT:  H - first byte
;          L - second bytes
FL_ID:    PUSH  DE
          CALL  cmdmode

          LD    A,90h
          CALL  FLSH_CMD

          LD    HL,4000h
          LD    D,(HL)
          INC   HL
          LD    E,(HL)
          EX    DE,HL
          LD    A,0F0h
          CALL  FLSH_CMD
          POP   DE
          RET

; ----------------------------------- FL_CSECT
; Clear a 4K flash sector.
; INPUT:  A - Flash page number (0-1f)
;        HL - Offset to START of sector.
; HL will be rounded down to a 4K boundary.
FL_CSECT: PUSH    AF
          LD      A,$3E             ; Move address down to a 4K boundary in bottom 16K
          AND     H
          OR      $40               ; And move so it's in bank 1
          LD      H,A
          PUSH    HL                ; Save the offset into bank 1

          CALL    _blink

          CALL    cmdmode           ; Set to command pages

          LD       A,80h            ; Activate erase mode
          CALL     FLSH_CMD

          ; Now the specific page erase
          CMD_CODE $5555,$aa        ; Command prefix
          CMD_CODE $2AAA,$55

          POP      HL               ; Flash sector number
          POP      AF               ; And the page number
          BANK     1                ; Into bank 1
          LD       (HL),30h         ; Erase sector
          BANK     1,0              ; And back to command page

          ; Toggle bit *should* be sufficient, but wait anyway
          CALL     _flsh_poll
          ; CALL     _wait

          CALL    _ledoff
          RET

; ----------------------------------- FL_CPAGE
; Clear an entire 16K ZIOS page. That's four Flash device sectors.
; INPUT:  A - Flash page number (0-1f)
FL_CPAGE: LD      B,4               ; Page counter
          LD      HL,0              ; Start offset into page
_nxtsect: PUSH    AF                ; Save page number
          PUSH    BC
          PUSH    HL
          CALL    FL_CSECT          ; First 4K sector
          POP     HL
          LD      DE,1000h          ; Offset to start of next sector
          ADD     HL,DE
          POP     BC
          POP     AF
          DJNZ    _nxtsect
          RET



; ----------------------------------- FL_WSECT
; Write a 4K flash sector.
; INPUT:  B -  From page number
;        DE -  Fromm address offset into page (0-3fffh)
;         C -  To page number
;        HL -  To address offset (0-3fffh)
; Page size is ALWAYS 4K
FL_WSECT: LD    (TO_PG),BC     ; Record the write addresses

          CALL  _blink

          ; Adjust the offsets such that they access the
          ; correct address when the page number is mapped
          ; to bank 1. So add 4000h to each address.
          LD    A,3fh            ; Mask out everything except the sector number
          AND   D
          OR    40h              ; And make sure we reference second bank
          LD    D,A
          LD    (FROM_ADD),DE

          ; For the destination, it MUST align with the start of a sector
          LD    A,30h            ; Mask out everything except the sector number
          AND   H
          OR    40h              ; And make sure we reference second bank
          LD    H,A
          LD    L,0
          LD    (TO_ADD),HL

          CALL  cmdmode        ; Standard flash page application

          ; The length is always a single page ($1000 or 4K)
          LD    BC,1000h
_nextb:   PUSH  BC
          LD    BC,(TO_PG)     ; Page numbers



          ; Read in the next byte
          BANK  1,B
          LD    HL,(FROM_ADD)

          LD    A,(HL)
          INC   HL
          LD    (FROM_ADD),HL    ; Ready for the next byte... A is the value we want
          LD    B,A              ; Store byte to write in B. Don't need the source page now
          BANK  1,0              ; Back to flash for addressing

          ; B now contains the byte to write to the next address.
          LD    HL,(TO_ADD)      ; Where to write it (C has the page number)
          CALL  _flsh_bt         ; Write the byte to C:HL
          INC   HL
          LD    (TO_ADD),HL      ; And save for the next byte.

          POP   BC
          DEC   BC
          LD    A,C
          OR    B
          JR    NZ,_nextb

          CALL    _ledoff
          RET


; ------------------- FL_WPAGE
; Write any 16K page into a specific 16K page in flash.
; INPUT:   B  - From page
;          C  - To page
FL_WPAGE:  LD    A,4        ; Number of flash sectors to write
           LD    DE,0       ; From offset
           LD    HL,0       ; To offset
_wpnxt:    PUSH  AF         ; Save sector count
           PUSH  BC
           PUSH  DE
           PUSH  HL
           CALL  FL_WSECT

           POP   HL         ; Step to address in HL on by 4K (1000h)
           LD    A,H
           ADD   10h
           LD    H,A

           POP   DE
           LD    A,D
           ADD   10h        ; Step from address in DE on by 4K (1000h)
           LD    D,A

           POP   BC
           POP   AF
           DEC   A
           JR    NZ,_wpnxt
           RET

; ----------------------------------- FLSH_CMD
; Write a command prefix sequence to FLASH. This is the sequence of three
; writes that all commands start with. There are no parameters.
;
; NOTE: flash pages must have been mapped into cmd mode.
FLSH_CMD:  PUSH     HL
           CMD_CODE $5555,$AA
           CMD_CODE $2AAA,$55
           CMD_CODE $5555,A
           POP      HL
           RET

; ----------------------------------- _flsh_bt
; Write a single byte to flash.
; INPUT:  C - target page number
;        HL - address
;         C - target page for write
;         B - data
_flsh_bt:  LD    A,$A0
           CALL  FLSH_CMD    ; Flash CMD

           BANK  1,C         ; Map in target page
           LD    (HL),B      ; Write the byte
           BANK  1,0         ; Reset
           CALL  _flsh_poll  ; Wait for gthe write to finish
           RET


; -- cmdmode
; Map FLASH pages 0 and 1 to banks 1 and 2 (mid 32K)
cmdmode:   BANK 1,0
           BANK 2,1
           RET

; ----------------------------------- _flsh_poll
; Poll the toggle bit until we get consecutive reads that DON'T
; togger bit 6.
_flsh_poll: PUSH  HL
            PUSH  DE
            PUSH  AF
            LD    HL,4000h
            LD    D,(HL)        ; Save start value
_flsh_lp1:  LD    E,(HL)        ; New value
            LD    A,D           ; Last value
            XOR   E             ; Compare the two
            AND   40h           ; Look at bit 6 (should have changed)
            LD    D,E           ; Save new value for next time
            JR    NZ,_flsh_lp1  ; toggled so not complete
            POP   AF
            POP   DE
            POP   HL
            RET

; ----------------------------------- _wait
; Short delay
_wait:    PUSH  HL
          PUSH  BC
          LD    B,10h
_wl2:     LD    H,0
_wl1:     LD    L,0
_wl0:     DEC   L
          JR    NZ,_wl0
          DEC   H
          JR    NZ,_wl1
          DJNZ  _wl2
          POP   BC
          POP   HL
          RET

; -----------------------------------_blink
; Blink LED
_blink:   PUSH     AF
          LD       A,(LEDVAL)
          XOR      30h
          AND      30h
          OR       01h
          LD       (LEDVAL),A
          OUT      (LEDS),A
          POP      AF
          RET

; -----------------------------------_ledoff
; LED OFF
_ledoff:  PUSH     AF
          LD       A,01h
          OUT      (LEDS),A
          POP      AF
          RET


          DSEG

LEDVAL     DEFB    10h
TO_PG      DEFB    0
FROM_PG    DEFB    0
TO_ADD     DEFW    0
FROM_ADD   DEFW    0
