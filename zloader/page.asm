import defs.asm
import config.asm
import zapi.asm

import pcb_def.asm
import zlib.asm
import zios.asm
import zload.asm


  ; Disassemmber
  extrn  DISASS, SETOFF

  ; From dump
  extrn  DECINST

  ; From regs
  extrn  DO_RGS

  ; From core
  extrn  BADPS,PRTERR


  extrn  main,MORE,E_ERROR

  public PAGE

; ------------ PAGE
; Page operations. Syntax:
;   PM - Display or change page map
;   PC - Copy one pge to another
PAGE:     ; Display current application pages or change a page number
          ; Changing is in the form blknum=pagenum. Eg 3=21
          CALL  SKIPSPC
          JR    Z,_shpg
          SUB   '0'
          JR    C,_shpg
          CP    4
          JR    NC,E_ERROR
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
