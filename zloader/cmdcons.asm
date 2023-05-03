; **********************************************
; Implements the following status commands:
;
; Syntax:
;    CONS n     ; Read port pp and display value
;
; Switch to console: 0 is serial and 1 is keyboard/vdu
;
; With no parameter it does nothing.
;
; **********************************************
; Copyright Peter Wilson 2023
; https://github.com/peterw8102/Z80-Retro
; **********************************************
import defs.asm
import config.asm
import zapi.asm

import pcb_def.asm
import zlib.asm
import zios.asm
import zload.asm

  extrn  E_BADPS
  extrn  main

  public CFGCONS,RSTCONS,SELCONS

; ------------------- Port output
CFGCONS: CALL  WASTESPC
         CALL  GET_DEC
         JR    C,E_BADPS

         ; Must be value 0 or 1
         LD    A,L
         CP    2
         JR    NC,E_BADPS

         ; Switch
         LD    (CONSOLE),A
         JR    main

RSTCONS: LD    A,(CONSOLE)
         JR    CNS_SET

SELCONS: LD    (CONSOLE),A
         JR    CNS_SET

         DSEG

; Console. If 0 then use the SIO, otherwise (assuming hardware is present) use the keyboard/SIO
CONSOLE    DEFB    0       ; Default to serial
