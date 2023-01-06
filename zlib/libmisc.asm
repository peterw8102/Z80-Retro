; import config.asm
import defs.asm

; Miscelaneous hardware
;
; SW_CFG:    Wait for a short period (block).
;
          ; Input functions
          public SW_CFG

          CSEG

; ---- SW_CFG
; Read the DIP config switches and return the result to the caller.
; Result returned in A
; All registers EXCEPT A are preserved.
SW_CFG:     IN    A,(PG_PORT0)
            AND   $E0
            RLCA
            RLCA
            RLCA
            RET
