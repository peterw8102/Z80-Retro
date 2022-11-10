; import config.asm
import defs.asm

; Useful/random functions...
;
; PAUSE:    Wait for a short period (block).
;
          ; Input functions
          public PAUSE

          CSEG

PAUSE:      PUSH  HL
            LD    H,001H
del_1:      LD    L,0FFH
del_2:      DEC   L
            JR    NZ,del_2
            DEC   H
            JR    NZ,del_1
            POP   HL
            RET
