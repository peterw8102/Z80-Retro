; **********************************************
; Implements: '.' (Show command history)
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


  extrn  main

  public SHWHIST


; ----- SHWHIST
; Display history with most recent at the bottom of the list.
SHWHIST:  LD    B,$11       ; Display AT MOST 16 rows
_nhist:   DEC   B
          CALL  GETHIST
          JR    Z,_gnxt      ; Nothing in that slot
          ; Got a history line, pointer in HL.
          CALL  PRINT_LN
_gnxt:    LD    A,B
          OR    A
          JR    NZ,_nhist
          JR    main
