import ../zlib/defs.asm
import config.asm

; DEPRECATED - NOT LINKED INTO THE MONITOR.
; This code WAS used to copy the running monitor into page zero of the
; onboard flash device. Used before the author had access to a flash
; programmer. Now obsolete and dangerous (can corrupt the monitor!).
;
; Included here because the code is interesting and took quite a while
; to work out and _MAY_ be useful later.

          extrn  PRINT,PRINT_LN,GET_LINE,SKIPSPC,WASTESPC,BUFCHR,WRITE_8,WRITE_16,INHEX_2,INHEX_4,MAPCASE
          extrn  BRK_HK
          extrn  CMD_B
          extrn  DISASS, SETOFF
          extrn  GET_HEX,GET_DEC,INPTR,INBUF,INITSIO,MAPCASE,RXA,TXA,CKINCHAR,SERINT

          ; Include SPI/SDCard library
          extrn  SD_INIT, SD_RBLK, SD_WBLK

          ; And the RTC/i2c library
          extrn  RTC_INI, RTC_MRD, RTC_MWR

          extrn  PGMAPX,main,BADPS,NL,OPMODE,PGMAPX

          public FLASH_OP

; -- _map_flsh
; Map FLASH pages 0 and 1 to banks 2 and 3 (top 32K)
_map_flsh: XOR  A
           OUT  (PG_PORT0+2),A
           INC  A            ; And flash page 1
           OUT  (PG_PORT0+3),A
           RET

; ------ ROUTINES TO COPY MONITOR TO FLASH --------
FLASH_OP: CALL  SKIPSPC
          JR    Z,flsh_id
          CP    'M'
          JR    Z,flsh_m
          CP    'I'
          JR    Z,flsh_id
          CP    'P'
          JR    Z,flsh_prg
          CP    'Z'
          JR    Z,flsh_clr

; ------------------- Read Flash Info
flsh_id:  CALL  NL
          CALL  _map_flsh
          ; --
          LD    A,90h
          CALL  FLSH_CMD
          ;--
          LD    HL,8000h
          LD    A,(HL)
          CALL  WRITE_8
          WRITE_CHR '-'
          LD    HL,8001h
          LD    A,(HL)
          CALL  WRITE_8
          CALL  NL
          ; -- EXIT
          LD    A,0F0h
          CALL  FLSH_CMD
          JR    main

flsh_m:   CALL  _map_flsh
          JR    main
; ------------------- flsh_clr
flsh_clr: CALL  _flsh_clr
          JR    main


; Programme first 8K into flash.
flsh_prg:  LD    HL,_FLSH_PG
           CALL  PRINT_LN
           CALL  _map_flsh  ; FLASH page 0 and 1 in top 32K
           LD    DE,4000h   ; From address
           LD    HL,8000h   ; To address
           CALL  _flsh_prg
           CALL  _wait
           LD    DE,5000h   ; From address
           LD    HL,9000h   ; To address
           CALL  _flsh_prg
           LD    DE,6000h   ; From address
           LD    HL,0A000h   ; To address
           CALL  _flsh_prg
           LD    DE,7000h   ; From address
           LD    HL,0B000h   ; To address
           CALL  _flsh_prg
           JR    main

; ----------------------------------- flsh_prg
; Programme a 4K page into Flash
; DE: Fromm address (RAM, 4000h or 5000h)
; HL: To address (Flash, 8000h or 9000h)
; Page size is ALWAYS 4K
_flsh_prg: LD    (FL_TO_ADDR),HL  ; TO ADDRESS saved
           ; The length is always a single page ($1000 or 4K)
           LD    BC,$1000
           LD    HL,_prg_msg      ; "FROM ADDRESS: "
           CALL  PRINT
           LD    H,D
           LD    L,E              ; From address from DE
           CALL  WRITE_16
           LD    HL,_prg_msgto
           CALL  PRINT
           LD    HL,(FL_TO_ADDR)   ; Get the TO address to display
           CALL  WRITE_16
           LD    HL,_prg_msglen
           CALL  PRINT
           LD    H,B               ; Length - always 4K
           LD    L,C
           CALL  WRITE_16
           CALL  NL
           ; Delete page 0 in flash - Not done here because page erase polling never works
           ; XOR   A
           ; CALL  _flsh_clr
           ; Write data
           LD    HL,(FL_TO_ADDR)   ; Get the TO address to display
loopp:     LD    A,(DE)            ; Get a source byte
           CALL  _flsh_bt          ; And programme it into address (HL)
           INC   HL
           INC   DE
           DEC   BC
           LD    A,B
           OR    C
           JR    NZ,loopp          ; Keep going until there are no bytes to send
           RET;
; ----------------------------------- _flsh_clr
; Erase page 0 and 1 (mapped to 8000 - 4K page x 2)
_flsh_clr: CALL _map_flsh
           LD   A,80h
           CALL FLSH_CMD
           LD   A,30h
           LD   DE,8000h         ; First 4K page to erase
           CALL _flsh_cmd
           CALL _flsh_poll
           CALL _wait
           LD   A,80h
           CALL FLSH_CMD
           LD   A,30h
           LD   DE,9000h         ; Second 4K page to erase
           CALL _flsh_cmd
           CALL _flsh_poll
           ; wait a while to be sure
           CALL _wait
           LD   A,80h
           CALL FLSH_CMD
           LD   A,30h
           LD   DE,0A000h         ; Third 4K page to erase
           CALL _flsh_cmd
           CALL _flsh_poll
           ; wait a while to be sure
           CALL _wait
           LD   A,80h
           CALL FLSH_CMD
           LD   A,30h
           LD   DE,0B000h         ; Last 4K page to erase (16K total)
           CALL _flsh_cmd
           CALL _flsh_poll
           ; wait a while to be sure
           CALL _wait
           RET

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
; ----------------------------------- _flsh_poll
; Poll for write operation complete
_flsh_poll:PUSH  HL
           LD    HL,8000h
_flsh_lp1: LD    A,(HL)
           XOR   (HL)
           RLCA
           RLCA
           JR    C,_flsh_lp1
           POP   HL
           RET

; ----------------------------------- _flsh_bt
; Write a single byte to flash. HL: Address, A: data
_flsh_bt:  PUSH  AF
           LD    A,0A0h
           CALL  FLSH_CMD  ; Flash CMD
           POP   AF
           LD    (HL),A
           CALL  _flsh_poll
           RET


FLSH_CMD:  PUSH  DE
           LD    DE,0D555h
           CALL  _flsh_cmd
           POP   DE
           RET
; ----------------------------------- _flsh_cmd
; Write a command sequence to FLASH. The flash page must be preselected in page 2. The command
; byte to be written is in the accumulator. No registers overwritten except A.
_flsh_cmd: PUSH  HL
           ; --
           LD    HL, 0D555h
           LD   (HL),0AAh
           ; --
           LD    HL, 0AAAAh
           LD   (HL),55h
           ; --
           LD    H, D
           LD    L, E
           LD   (HL),A
           POP   HL
           RET

_FLSH_PG: DEFB "Flash: ",0

_prg_msg:    DEFB "FROM ADDRESS: ",NULL
_prg_msgto:  DEFB ", TO: ",NULL
_prg_msglen: DEFB ", LEN: ",NULL

          DSEG

FL_TO_ADDR DEFW    0
