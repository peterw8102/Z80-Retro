; **********************************************
; Implements the following status commands:
;
;    M [addr]     ; Modify application memory
;    SM [offset]  ; Modify the loaded SDCard sector
; **********************************************
; Copyright Peter Wilson 2022
; https://github.com/peterw8102/Z80-Retro
; **********************************************
import defs.asm
import config.asm
import zapi.asm

import pcb_def.asm
import zlib.asm
import zios.asm
import zload.asm

  ; extrn  SETDMA
  extrn  E_BADPS
  extrn  main
  extrn  SDPAGE

  public SDMOD,MODIFY

; ------------------- SDMOD
; Modify data in the SDCard buffer. The parameter is the offset into the SDCard buffer
; with a base of zero.
;
;   MS offset  - Offset is required in this case.
SDMOD:   CALL  WASTESPC
         CALL  GET_HEX          ; Offset into the SDCard buffer
         JR    Z, E_BADPS

         ; Sector size is 512 buyes so anthing greater than 0x200 is out of range.
         LD    A,$FE
         AND   H
         JR    NZ,E_BADPS

         ; Set up the addresses to display and use
         LD    DE,SDPAGE        ; Calculate the address in the buffer (SDPAGE+offset)
         EX    DE,HL
         ADD   HL,DE            ; HL now actual address, DE is display offset
         JR    _nextln

;
; ------------------- MODIFY
; Modify application space memory. Optional address, which can
; be '.' to signify current value of the programme counter.
MODIFY:  CALL  WASTESPC
         JR    Z,E_BADPS
         CP    '.'
         JR    NZ,_geth
         LD    HL,(R_PC)
         JR    _gotaddr
_geth:   CALL  GET_HEX          ; Start address (in application space)
         JR    Z, E_BADPS

         ; Make a copy in DE
_gotaddr:LD    D,H
         LD    E,L
         CALL  P_MAPX     ; Translate HL into an offset and a page number and map application pages into memory

         ; HL: Mapped address to write to
         ; DE: The original address that we should display
_nextln: EX    DE,HL
         CALL  WRITE_16   ; Display the application space address
         EX    DE,HL      ; Back to mapped address
         WRITE_CHR ':'
         WRITE_CHR ' '
         CALL  GET_LINE
         LD    B,0
_nexthx: CALL  WASTESPC
_skhx:   JR    Z, _eoln
         CP    '$'         ; Ignore $ and ','
         JR    Z,_skdol
         CP    ','
         JR    Z,_skdol
         CALL  INHEX_2
         JR    C, _eoln    ; Carry: error, no hex digits
         ; Have a value to write...
         LD    (HL),A      ; Storing in mapped address location
         INC   HL
         INC   DE          ; Keep raw address in sync
         INC   B
         JR    _nexthx
_eoln:   CALL  NL
         LD    A,B
         OR    A       ; Did we get any bytes?
         JR    NZ,_nextln
         JR    main

_skdol:  CALL  BUFCHR
         JR    _nexthx
