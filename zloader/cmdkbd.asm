; **********************************************
; Implements: 'KBD' (Change keyboard mapping)
; Syntax:
;    KBD [VT100 | WS]
;    KBD [state offset char]
;
; Select between one of the two standard keyboard maps
; for the Omega keyboard.
;
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

  extrn  E_BADPS,E_PRTERR
  extrn  main

  public KEYBOARD


; ------------------- fill
KEYBOARD: CALL  WASTESPC
          ; Must be one of the known modes (vt100 or wordstar)
          LD    HL,(INPTR)
          LD    DE,_vt100
          CALL  STRCMP
          LD    A,0
          JR    Z,.setmode
          LD    HL,(INPTR)
          LD    DE,_wordstr
          CALL  STRCMP
          LD    A,1
          JR    Z,.setmode
          LD    HL,_badmode
          JR    E_PRTERR

.setmode: LD    C,A_KBDMAP
          LD    B,0
          LD    D,A
          RST   30h
          JR    main

_vt100:      DEFB "vt100",NULL
_wordstr:    DEFB "wordstar",NULL
_badmode:    DEFB "Allowed maps are 'vt100' or 'wordstar'",NULL
