; import config.asm
import defs.asm

          public PRINT,PRINT_LN,GET_LINE,SKIPSPC,WASTESPC,BUFCHR,WRITE_8,WRITE_16,HEX_FROM_A,INHEX_2,INHEX_4,GET_HEX,INPTR,INBUF,END_PROG

          CSEG
; Layer of code above the the basic driver that provides line input control and output formatting.
GET_CHR:  RST   10H                ; Read character
          CP    'a'                ; Lower case -> upper case
          RET    C
          SUB   'a'
          ADD   A,'A'
          RET

; ------------- HEX_FROM_A
; IN  - A:  Number to convert to HEX
; OUT - HL: Two character converted value - H MSB
; HL and A NOT preserved
HEX_FROM_A: PUSH  DE
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

; ----------------------------------- Ouput the 16 bit value in HL
WRITE_16:  PUSH AF
           LD   A,H
           CALL WRITE_8
           LD   A,L
           CALL WRITE_8
           POP  AF
           RET

WRITE_8:   PUSH HL
           CALL HEX_FROM_A
           LD   A,H
           RST  08H
           LD   A,L
           RST  08H
           POP  HL
           RET

; ------------------- GET_HEX - read in up to 4 hex digits, returned in HL
GET_HEX:  PUSH  BC
          LD    B,A  ; Save A
          XOR   A
          LD    H,A
          LD    L,A
          LD    C,A
          CALL  SKIPSPC
          ; End of input?
next_hc:  OR    A
          JR    NZ, cont_hc
fin:      LD    A,C
          OR    A
          LD    A,B
          POP   BC
          RET
cont_hc:  CALL  HEX_TO_BIN
          JR    C,fin
          ; Is it between 0 and 9?
add_chr:  ADD   HL, HL
          ADD   HL, HL
          ADD   HL, HL
          ADD   HL, HL
          ADD   A, L
          LD    L, A
          INC   C
          CALL  BUFCHR
          JR    next_hc

; -------- INHEX_2 - Return 2 hex digit value in A. Set C flag on error
INHEX_2:  PUSH  HL
          LD    L,0     ; Value being built
          CALL  BUFCHR  ; A -> character
          CALL  HEX_TO_BIN
          JR    C,errhex2
          LD    L,A     ; First byte
          CALL  BUFCHR  ; A -> character
          CALL  HEX_TO_BIN
          JR    C,errhex2
          LD    H,A     ; Tmp store
          LD    A,L     ; Current val
          ADD   A,A
          ADD   A,A
          ADD   A,A
          ADD   A,A
          ADD   A,H     ; Which will leave carry clear
          POP   HL
          OR    A
          RET

errhex2:  POP   HL
          SCF
          RET

INHEX_4:  CALL  INHEX_2
          RET   C
          LD    H,A
          CALL  INHEX_2
          LD    L,A
          RET

; -------- HEX_TO_BIN Char in A - 0-15. 255 if not valid char
HEX_TO_BIN: CP    '0'
            JR    C, inv      ; Less than zero so invalid
            CP    'F'+1
            JR    NC, inv     ; > 'F' so ignore
            CP    '9'+1
            JR    NC, letter_hc
            SUB   '0'
            AND   0fh
            RET
letter_hc:  CP    'A'
            JR    C, inv
            SUB   'A'-10
            OR    A
            RET
inv:        SCF
            RET

; --------------------- GET_LINE
GET_LINE: PUSH     HL
          PUSH     BC
          LD       HL, INBUF
          LD      (INPTR), HL
          LD       BC, 0

getc:     CALL     GET_CHR
          CP       CR
          JR       Z, eol
          CP       BS
          JR       Z, bspc

          ; Store in buffer
          LD      (HL), A

          ; At end of buffer?
          LD      C,A           ; Save last character
          LD      A,80
          CP      B
          JR      Z, getc       ; buffer full
          INC     HL
          INC     B
          LD      A,C
echo:     RST     08H           ; Echo character
          JR      getc

eol:      XOR     A
          LD      (HL),A
          LD      A,B
          POP     BC
          POP     HL
          OR      A     ; Z flag set if no characters entered in line
          RET

bspc:     XOR      A
          CP       B
          JR       Z, getc

          ; Delete character
          DEC      HL
          LD      (HL), A
          DEC      B
          LD       A,BS
          JR       echo

BUFCHR:   PUSH     HL
          LD       HL, (INPTR)
          LD       A, (HL)
          OR       A
          JR       Z, eb1
          INC      HL
          LD       (INPTR), HL
eb1:      POP      HL
          RET

; -------- SKIPSPC
; Step over spaces and return first non-space character.
SKIPSPC:  PUSH     HL
          LD       HL, (INPTR)
skip      LD       A, (HL)
          OR       A
          JR       Z, eb
          INC      HL
          CP       SPC
          JR       Z,skip
          CP       TAB
          JR       Z,skip
          LD       (INPTR), HL
eb:       POP      HL
          RET

WASTESPC: PUSH     HL
          LD       HL, (INPTR)
skip2:    LD       A, (HL)
          OR       A
          JR       Z, eb
          INC      HL
          CP       SPC
          JR       Z,skip2
          CP       TAB
          JR       Z,skip2
          DEC      HL
          LD       (INPTR), HL
          JR       eb


; --------- GET_HEX_2 - read 2 byte HEX value from input line to A
; Z flag set on OK

; --------------------- PRINT - write a string to the terminal
; HL: The address of the string to print (NOT SAVED)
; A and HL not saved
PRINT:    LD       A,(HL)          ; Get character
          OR       A               ; Is it $00 ?
          RET      Z               ; Then RETurn on terminator
          RST      08H             ; Print it
          INC      HL              ; Next Character
          JR       PRINT           ; Continue until $00
PRINT_LN: CALL     PRINT
          LD       A,CR
          RST      08H
          LD       A,LF
          RST      08H
          RET
; Read only data definitions that go in the code section
_HEX_CHRS: DEFB  "0123456789ABCDEF"
END_PROG:  .DB 0

; Data defintions for this module
         dseg

INPTR:      DEFW  INBUF
INBUF:      .DS   80
END_INBUF:  DEFB    0
