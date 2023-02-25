; **********************************************
; Implements the following status commands:
;
; Syntax:
;    IN pp      ; Read port pp and display value
;    OUT pp=vv  ; Output value 'vv' to port 'pp'
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

  extrn  SETDMA
  extrn  E_BADPS
  extrn  main

  public INPUT,OUTPUT

; ------------------- Port output
OUTPUT:  CALL  WASTESPC
         CALL  INHEX_2
         JR    C,E_BADPS
         LD    C,A
         PUSH  BC
         CALL  WASTESPC
         CALL  INHEX_2
         JR    C,E_BADPS
         PUSH  AF
         LD    HL,_OUTMSG
         CALL  PRINT
         LD    A,C
         CALL  WRITE_8
         WRITE_CHR '='
         POP   AF
         PUSH  AF
         CALL  WRITE_8
         POP   AF
         POP   BC
         OUT   (C),A
         CALL  NL
         JR    main

; ------------------- Port input
INPUT:   CALL  WASTESPC
         CALL  INHEX_2
         JR    C,E_BADPS
         LD    C,A        ; Port number
         LD    HL,_INMSG
         CALL  PRINT
         LD    A,C
         CALL  WRITE_8
         WRITE_CHR '='
         IN    A,(C)
         CALL  WRITE_8
         CALL  NL
         JR    main

;
; ---- TEXT MESSAGES ----
_OUTMSG:  DEFB CR,LF,"Out: ",NULL
_INMSG:   DEFB CR,LF,"In: ",NULL
