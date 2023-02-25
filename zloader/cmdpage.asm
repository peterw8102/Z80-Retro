; **********************************************
; Implements the following status commands:
;
;    P       ; Display current application pages
;    P b=p   ; Map page 'p' into bank 'b'
; There are 4 banks so 'b' can be 0-3
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


  extrn  main,E_BADPS

  public PAGE

; ------------ PAGE
; Page operations. Syntax:
;   P - Display or change page map
PAGE:     ; Display current application pages or change a page number
          ; Changing is in the form blknum=pagenum. Eg 3=21
          CALL  SKIPSPC
          JR    Z,_shpg
          SUB   '0'
          JR    C,_shpg
          CP    4
          JR    NC,E_BADPS
          ; Calculate PAGE_MP position
          LD    HL,PAGE_MP
          ADD   L
          LD    L,A
          CALL  SKIPSPC
          CP    '='
          JR    NZ,_shpg
          CALL  INHEX_2
          JR    C,_shpg

          LD    (HL),A
          ; Drop through and display current settings.

_shpg:    LD    HL, _APPPG
          CALL  PRINT
          LD    HL,PAGE_MP
          LD    B,4
_npd:     LD    A,(HL)
          CALL  WRITE_8
          WRITE_CHR ' '
          INC   HL
          DJNZ  _npd
          CALL  NL
          JR    main

_APPPG:      DEFB "App pages: ", NULL
