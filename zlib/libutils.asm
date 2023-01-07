; import config.asm
import defs.asm

; Useful/random functions...
;
; PAUSE:    Wait for a short period (block).
;
          ; Input functions
          public PAUSE,STRCMP,ADD8T16,TOUPPER

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

; ---- STRCMP
; Compare the string in HL with the test string in DE. This is a limited test as follows:
;  + Both strings are zero terminated.
;  + The strings are considered to be the same if HL points to a string at least as long as DE
;    AND that all characters pointed to by HL match those pointed to by DE
;
; HL: String to test
; DE: Required test string
; A:      Zero if both strings match, Non-zero if the strings do NOT match
; Z flag: Matches state of 'A'
STRCMP:     LD    A,(DE)
            INC   DE
            OR    A
            RET   Z         ; At the end of DE and no failures
            CP    (HL)      ; Match the test string?
            RET   NZ        ; String pointed to doesn't match
            LD    A,(HL)
            OR    A         ; End of HL string?
            JR    Z,_sfail
            INC   HL
            JR    STRCMP

_sfail:     INC   A
            RET



; ------ ADD8T16
; Add A to HL and correctly deal with carry from L to H. HL and A changed. No
; other registers used or changed.
ADD8T16:  ADD   L
          LD    L,A
          RET   NC
          INC   H
          RET

; ------ TOUPPER
; Take the character in A. If it's a lower case letter then promote to uppercase.
TOUPPER:  CP    'a'                ; Lower case -> upper case
          RET    C
          CP    'z'+1
          RET   NC
          ADD   A,'A'-'a'
          RET
