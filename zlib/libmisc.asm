; import config.asm
import defs.asm

; Miscelaneous hardware
;
; SW_CFG:    Wait for a short period (block).
;
          ; Input functions
          public SW_CFG,HASVDU,HASPIO,MAPMPG

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

; ---- HASVDU
; Check for the existence of the character based VDU card. Return:
;    Z: true (zero) if the card exists, false otherwise
;
; Only register not saved is A
HASVDU:     LD       A,$FF
            OUT      (PG_PORT0+1), A  ; Map VDU memory into CPU space

            ; Try to write to memory
            PUSH     HL
            PUSH     DE
            LD       HL,$55AA
            LD       ($7FFE),HL
            LD       DE,($7FFE)
            LD       A,E
            CP       $AA
            RET      NZ
            LD       A,D
            CP       $55
            POP      DE
            POP      HL
            RET

; ---- HASPIO
; PIO Card Presense
; Returns the presense or absense of the PIO Card. Haven't worked out to do that. Currently
; this is a NOP until the PIO has been tested.
; `Z` flag. `Z` is true if there's a video card installed.
HASPIO:     XOR      A
            INC      A
            RET


;---- MAPMPG
; Map a specified memory page into one of the banks.
; D: Bank number (0-3)
; E: Page number (0-255)
MAPMPG:    PUSH      BC
           LD        A,PG_PORT0
           ADD       D
           LD        C,A
           LD        A,E
           OUT       (C),A
           POP       BC
           RET
