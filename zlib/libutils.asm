; import config.asm
import defs.asm

; Useful/random functions...
;
; PAUSE:    Wait for a short period (block).
;
          ; Input functions
          public PAUSE,STRCMP,ADD8T16,TOUPPER
          public DEC2BIN,HEX2BIN,BIN2HEX

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
ADD8T16:  OR    A
          RET   Z       ; Optimise for adding zero...
          ADD   L
          LD    L,A
          RET   NC
          INC   H
          RET

; ------ TOUPPER
; Take the character in A. If it's a lower case letter then promote to uppercase.
; INPUT:  A - character
; OUTPUT: A - character converted to uppercase
TOUPPER:  CP    'a'                ; Lower case -> upper case
          RET    C
          CP    'z'+1
          RET   NC
          ADD   A,'A'-'a'
          RET

; -------- DEC2BIN
; Convert the (ASCII) character in A to a binary number. A contains ASCII '0' to '9' inclusive
; Carry flag set if A contains an invalid character (out of range). Result in A.
; INPUT:  A - ASCII character between 0-9 to be valid
; OUTPUT: C - Carry flag. Set on error (character not a valid decimal digit).
;         A  -If Carry clear then a binary number between 0-9
; All registers EXCEPT A preserved
DEC2BIN:    SUB   '0'         ; Minimum value
            JR    C,inv
            CP    10
            JR    NC,inv
            OR    A           ; Clear carry
            RET

; -------- HEX2BIN
; Convert hex character in A to a binary number in A
; INPUT:  A - valid character (0-9, a-f, A-F)
; OUTPUT: C - Carry, set on error (invalid hex character on input)
;         A - if Carry flag clear then binary value 0-15
HEX2BIN:    CALL  TOUPPER     ; Only deal with upper case letters!
            CP    '0'
            JR    C, inv      ; Less than zero so invalid
            CP    'F'+1
            JR    NC, chklwr  ; > 'F' so ignore
            CP    '9'+1
            JR    NC, letter_hc
            SUB   '0'
            AND   0fh
            RET
letter_hc:  CP    'A'
            JR    C, inv
            SUB   'A'-10
_usech:     OR    A
            RET
inv:        SCF
            RET

chklwr:     CP    'f'+1        ; Check if it's lower a-f
            JR    NC, inv
            CP    'a'
            JR    C,inv        ; Less than 'a' so invalid
            SUB   'a'-10
            JR    _usech


; ------------- BIN2HEX
; IN  - A:  Number to convert to HEX
; OUT - HL: Two character converted value - H MSB
; HL and A NOT preserved
BIN2HEX:    PUSH  DE
            PUSH  AF
            LD    HL, _HEX_CHRS
            PUSH  HL
            ; LSB first
            AND   0Fh
            ADD   A,L
            LD    L,A
            JR    NC,_hs1
            INC   H
_hs1:       LD    E,(HL)
            POP   HL
            ; MSB
            POP   AF
            RRA
            RRA
            RRA
            RRA
            AND   $0F
            ADD   A,L
            LD    L,A
            JR    NC,_hs2
            INC   H
_hs2:       LD    D,(HL)
            EX    DE,HL
            POP   DE
            RET


; Read only data definitions that go in the code section
_HEX_CHRS: DEFB  "0123456789ABCDEF"
