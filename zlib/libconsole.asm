; import config.asm
import defs.asm

          public PRINT,PRINT_LN,GET_LINE,SKIPSPC,WASTESPC,BUFCHR,WRITE_8,WRITE_16,HEX_FROM_A
          public INHEX,INHEX_2,INHEX_4,GET_HEX,GET_DEC,INPTR,INBUF,MAPCASE,_ENDPRG,UPPERCASE
          public SETHIST,GETHIST

          ; Utilities
          public DEC2BIN

          CSEG
; Layer of code above the the basic driver that provides line input control and output formatting.
GET_CHR:  LD    A,(UPPERCASE)
          OR    A
          JR    Z,_rawchr
          RST   10H                ; Read character
          CP    'a'                ; Lower case -> upper case
          RET    C
          CP    'z'+1
          RET   NC
          ADD   A,'A'-'a'
          RET
_rawchr:  RST   10H
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

; ----------------------------------- Output the 16 bit value in HL
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
          LD    C,A           ;
          CALL  SKIPSPC
next_hc:  OR    A             ; End of input?
          JR    NZ, cont_hc
fin:      LD    A,C
          OR    A
          LD    A,B
          POP   BC
          RET
cont_hc:  CALL  HEX_TO_BIN
          CALL  C,UNGET
          JR    C,fin
          ADD   HL, HL
          ADD   HL, HL
          ADD   HL, HL
          ADD   HL, HL
          ADD   A, L
          LD    L, A
          INC   C
          CALL  BUFCHR
          JR    next_hc

;
; -------- GET_DEC - Read a (16 bit) decimal number into HL with C flag on error
GET_DEC:  PUSH  BC
          LD    B,A           ; Save A
          XOR   A
          LD    H,A           ; Working value in HL, set to zero
          LD    L,A
          LD    C,A           ; Count number of characters
          CALL  SKIPSPC
next_dc:  OR    A             ; End of input?
          JR    Z, fin        ; Same end caseas GET_HEX
cont_dc:  CALL  DEC2BIN    ; Decimal character?
          CALL  C,UNGET
          JR    C,fin
          PUSH  DE            ; Muliple HL by 10
          ADD   HL, HL        ; x2
          LD    E,L
          LD    D,H
          ADD   HL, HL        ; x4
          ADD   HL, HL        ; x8
          ADD   HL,DE         ; x10
          POP   DE
          ADD   A, L          ; And add in the new digit.
          LD    L, A
          LD    A,0
          ADC   H
          LD    H,A
          INC   C
          CALL  BUFCHR
          JR    next_dc


; -------- _shft32 - Shift HLDE 4 bits left (mult. 16)
_shft32:  PUSH  BC
          LD    B,4
_shn:     SLA   E
          RL    D
          RL    L
          RL    H
          DJNZ  _shn
          POP   BC
          RET

; -------- INHEX - Read a 2 byte (16 bit) hex representation
; number. Keeps reading characters until a non-hex digit is read.
;
; Output in HLDE
; Accumulator NOT preserved
; Carry flag set on error (no hex digits)
INHEX:    XOR   A
          LD    H,A
          LD    L,A
          LD    D,A
          LD    E,A
          CALL  BUFCHR  ; A -> character
          CALL  HEX_TO_BIN
          JR    C,errhexx ; Must be at least ON hex digit!

          ; Multiple HL by 16
nxtchr:   CALL _shft32

          ; Map in the new digit
          OR    E
          LD    E,A

          CALL  BUFCHR  ; A -> character
          JR    Z, eonum
          CALL  HEX_TO_BIN
          JR    NC, nxtchr

          ; Read a character that's NOT a hex char (or end of line) - unget it
          CALL  UNGET

eonum:    OR    A ; Clear carry
          RET

errhexx:  SCF
          RET

INHEX_4:  PUSH  DE
          CALL  INHEX
          EX    DE,HL    ; Want LS two bytes in HL
          POP   DE
          RET


; -------- INHEX_2 - Return 2 hex digit value in A. Set C flag on error
INHEX_2:  PUSH  HL
          CALL  INHEX
          LD    A,L
          POP   HL
          RET

; -------- DEC2BIN Char in A - 0-9. Carry flag set if invalid.
DEC2BIN:    SUB   '0'         ; Minimum value
            JR    C,inv
            CP    10
            JR    NC,inv
            OR    A           ; Clear carry
            RET

; -------- HEX_TO_BIN Char in A - 0-15. 255 if not valid char
HEX_TO_BIN: CP    '0'
            JR    C, inv      ; Less than zero so invalid
            CP    'F'+1
            JR    NC, chklwr     ; > 'F' so ignore
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


; --------------------- MAPCASE
; If A is 1 then lower case is mapped to upper case (default). If 0
; then no case mapping happens
MAPCASE:  LD      (UPPERCASE),A
          RET
; --------------------- GET_LINE
GET_LINE: PUSH     HL
          PUSH     BC
          CALL     INITMLN
          LD       HL, INBUF     ; Reset the line pointer
          LD      (INPTR), HL
          LD      B,LINELEN
          XOR     A
_znxt:    LD      (HL),A
          INC     HL
          DJNZ    _znxt
          LD       HL, INBUF     ; Reset the line pointer
          LD      (CURSOR), HL   ; Where we're currently entering content.
          LD      (HL),0         ; Flag end of line here
          LD       BC, 0         ; B: Cursor pos, C: last character

getc:     CALL     GET_CHR
          LD       C,A           ; Save last character
          CP       ESC
          JR       Z, _esc
          CP       CR
          JR       Z, eol
          CP       BS
          JR       Z, bspc
          CP       DEL
          JR       Z, bspc
          CP       VT
          JR       Z, deol

          ; Store in buffer, insert space if necessary
_ext:     LD      A,LINELEN
          CP      B
          JR      Z, getc       ; buffer full so ignore this character
          LD      A,(HL)
          OR      A
          LD      A,B
          CALL    NZ,INSERT     ; Insert if current pos is not empty
          LD      (HL), C       ; Store the character

          ; Move cursor forward
          INC     HL
          LD      (CURSOR),HL
          INC     B             ; Cursor offset
          LD      A,C
          RST     08H           ; Echo character
          JR      getc

eol:      LD      A,(INBUF)
          OR      A
          CALL    NZ,ADD_HIST   ; Add current line to history buffer
          POP     BC
          POP     HL
          LD      A,(INBUF)
          OR      A             ; Z flag set if no characters entered in line
          RET

_esc:     LD      A,(MLINE)
          OR      A
          LD      A,C
          JR      Z,_ext
          ; Escapes are being processed. If it's one we know then do something otherwise ignore.
          CALL    GET_CHR
          LD      C,A
          CP      '['           ; The only form I understand for arrow characters
          JR      NZ,_ext
          CALL    GET_CHR       ; It's a ESC-[ - net character must be A,B,C or D
          LD      C,A
          CP      'A'
          JR      Z,_pline
          CP      'B'
          JR      Z,_nline
          CP      'C'
          JR      Z,_nchr
          CP      'D'
          JR      Z,_pchr
          CP      '3'
          JR      NZ,getc
          CALL    GET_CHR
          CP      $7E
          JR      NZ,getc

          ; Delete the current character
          ; WRITE_CHR '}'
          CALL    DELETE
          JR      getc         ; Not a recognised escape sequence.

; -- _pchr
; Move cursor one space to the left.
_pchr:    LD      A,B
          OR      A
          JR      Z,getc       ; At start of line
          DEC     B
          DEC     HL
          WRITE_CHR ESC
          WRITE_CHR '['
          WRITE_CHR '1'
          WRITE_CHR 'D'
          JR      getc

; -- _nchr
; Move cursor one space to the right.
_nchr:    LD      A,(HL)
          OR      A
          JR      Z,getc       ; At end of line (null terminator)
          INC     B
          INC     HL
          WRITE_CHR ESC
          WRITE_CHR '['
          WRITE_CHR '1'
          WRITE_CHR 'C'
          JR      getc

; -- _hprev
; Move back one history line from (HL)
_hprev:   CALL    DECB      ; Keep going back until OD or FF
          CP      0Dh
          JR      Z,_hfnd
          INC     A
          JR      Z,_hfnd
          JR      _hprev    ; Step back

_hfnd:    CALL    INCB      ; Step in front of the line marker
          XOR     A
          INC     A         ; NZ for found
          RET


; -- _pline
; Discard the current line and move to the previous one
_pline:   PUSH    HL
          LD      HL,(HPOS) ; Move backwards in this history buffer until the previous-but-one OD/FF
          CALL    DECB      ; Points to the previous OD
_pl1:     CALL    DECB      ; Keep going back until OD or FF
          CP      0Dh
          JR      Z,_pl1_st
          INC     A
          JR      NZ,_pl1

          ; Found an FFh which means there's we've got back to the head of the
          ; history queue. Step forward 1 character. If that's FF then the queue
          ; is empty
          CALL    INCB
          LD      A,(HL)
          INC     A
          JR      NZ,_more
_nmore:   POP     HL
          JR      getc

_nline:   PUSH    HL
          LD      HL,(HPOS)
          LD      A,(HL)
          INC     A
          JR      Z,_nmore
          ; Step forward to the end of this line (OD)
_nl1:     CALL    INCB
          CP      0Dh
          JR      NZ,_nl1

_pl1_st:  CALL    INCB
          INC     A
          JR      Z,_nmoren    ; End of history
_more:    LD      (HPOS),HL
          POP     HL
          JR      _RESLN

_nmoren:  LD      A,ESC    ; restore cursor
          RST     08H
          LD      A,'8'
          RST     08H
          LD      A,ESC    ; clear to end of line
          RST     08H
          LD      A,'['
          RST     08H
          LD      A,'K'
          RST     08H
          LD      HL,INBUF ; Reset to start of line
          LD      (HL),0   ; Terminator
          JR      getc

; ------ _RESLN
; Copy the current history line to the input buffer and update display.
_RESLN:   PUSH    DE

          ; Restore cursor
          LD      A,ESC
          RST     08H
          LD      A,'8'
          RST     08H
          LD      HL,(HPOS)
          LD      DE,INBUF
          LD      B,0
_nch3:    LD      A,(HL)
          CP      0Dh
          JR      Z,_eohl
          LD      (DE),A
          RST     08H
          INC     B        ; Character count
          INC     DE
          CALL    INCB
          JR      _nch3
_eohl:    EX      DE,HL
          LD      (HL),0
          POP     DE
          ; Clear to end of line
          LD      A,ESC
          RST     08H
          LD      A,'['
          RST     08H
          LD      A,'K'
          RST     08H
          JR      getc


; ----- deol
; Delete to end of line (Ctrl-K)
deol:     XOR      A
          LD       (HL),A
          WRITE_CHR ESC
          WRITE_CHR '['
          WRITE_CHR 'K'
          JR       getc


bspc:     XOR      A
          CP       B
          JR       Z, getc

          ; Delete character
          DEC      HL
          DEC      B
          WRITE_CHR ESC
          WRITE_CHR '['
          WRITE_CHR 'D'
          CALL     DELETE
          JR       getc

; ----- INITMLN
; Initialise multiline
INITMLN:  PUSH     HL
          LD       HL,(LPOS)
          LD       (HPOS),HL
          ; Store the cursor position
          LD       A,ESC
          RST      08H
          LD       A,'7'
          RST      08H
          POP      HL

          LD       A,(INIT)
          OR       A
          RET      NZ
          ; Initialise the history buffer
          PUSH     BC
          PUSH     DE
          PUSH     HL
          LD       BC,MLSZ
          LD       HL,MLBUF
          LD       (LPOS),HL
          LD       (HPOS),HL
          XOR      A
          DEC      A
          LD       (INIT),A    ; Set the initialised flag to non-zero
_nb:      LD       (HL),A
          LD       D,H
          LD       E,L
          INC      DE
          DEC      BC
          LDIR                 ; Entire buffer set to FF
          DEC      A

          POP      HL
          POP      DE
          POP      BC
          RET



; ----- DEDUP
; Check current line against the last line in the history buffer. Return Z if
; the lines are the same.
DEDUP:    PUSH    HL
          PUSH    DE
          LD      HL,(LPOS)    ; Move backwards in this history buffer until the previous-but-one OD/FF
          CALL    DECB         ; Points to the previous OD
_dd1:     CALL    DECB         ; Keep going back until OD or FF
          CP      0Dh
          JR      Z,_dd2
          INC     A            ; If FF then there's no more content
          JR      Z,_nodup
          JR      _dd1
_dd2:     LD      DE,INBUF
_dd3:     CALL    INCB         ; HL points to start of line
          LD      A,(DE)
          OR      A            ; Got to end of buffer without finding any difference
          JR      Z,_dupret
          CP      (HL)
          JR      NZ,_dupret   ; Character is different so return NZ
          INC     DE
          JR      _dd3

_nodup:   INC     A            ; Force non-zero
_dupret:  POP     DE
          POP     HL
          RET

; ----- GETHIST
; Return the 'n'th history row.
; A: requested history index
;    index 0 is the most recent history item
; Return: HL: Points to the start of the line, teminated with an OD
;             Return Z:  No history item
;                    NZ: Returned item pointed to by HL
GETHIST:  PUSH     BC
          PUSH     DE
          LD       B,A
          INC      B           ; Index counter
          LD       HL,(LPOS)
_gh1:     CALL     DECB        ; Points to the previous OD (or FF)
          INC      A
          JR       Z,_gh0      ; Wasn't an 0D so no line
          CALL     _hprev      ; Step back - found if NZ
          ; JR       Z,_gh0
          ; HL points to start of line - need to look back further?
          DJNZ     _gh1

          ; Got the line wanted, copy to the line buffer
          LD       DE,INBUF
_gh3:     LD       A,(HL)
          CP       $0D
          JR       Z,_gh2
          LD       (DE),A
          INC      DE
          CALL     INCB
          JR       _gh3

_gh2:     LD       HL,INBUF
          XOR      A
          LD       (DE),A
          INC      A

_gh0:     POP      DE
          POP      BC
          RET

; ----- ADD_HIST
; Add a new line to the history buffer
ADD_HIST: LD       A,(MLINE)
          OR       A
          LD       A,B        ; Count
          RET      Z          ; History switched off
          CALL     DEDUP
          RET      Z          ; No difference from previous history line
          PUSH     HL
          PUSH     DE
          PUSH     BC
          ; Copy line into buffer
          LD       HL,(LPOS)
          ; Copy characters from the line (DE) into the history buffer (HL) until a zero is seen
          LD       DE,INBUF
_nchr2:   LD       A,(DE)
          OR       A
          JR       Z,_ehln
          LD       (HL),A
          INC      DE
          CALL     INCB
          JR       _nchr2
          ; Add an end of line...
_ehln:    LD       (HL),0Dh
          CALL     INCB
          LD       (LPOS),HL
          LD       C,0FFh
          LD       (HL),C
_nchr3:   ; Fill all characters until the next 0D or FF with FF flags
          CALL     INCB
          LD       (HL),C
          CP       0Dh         ; The character that WAS (HL)
          JR       Z,_dn       ; End of line
          INC      A           ; Got all the way to the begging (FF)?
          JR       NZ,_nchr3
_dn:      POP      BC
          POP      DE
          POP      HL
          RET

; ---- INSERT
; Shift characters in the input buffer to the right. A contains the
; current position in the buffer (cursor position).
INSERT:   PUSH     HL
          PUSH     DE
          PUSH     BC
          LD       HL,INBUF+LINELEN-1     ; Last character
          LD       D,H
          LD       E,L
          DEC      HL
          LD       C,A                    ; Store cursor position
          LD       A,LINELEN-1
          SUB      C
          JR       Z,_nosh
          JR       C,_nosh
          LD       C,A
          LD       B,0
          LDDR
          ; And insert space at cursor on display
          WRITE_CHR ESC
          WRITE_CHR '['
          WRITE_CHR '1'
          WRITE_CHR '@'
_nosh:    POP      BC
          POP      DE
          POP      HL
          RET

DELETE:   PUSH     HL
          PUSH     DE
          ; HL points into the buffer at the current location (cursor)
          LD       D,H
          LD       E,L
          INC      HL
_dnxt:    LD       A,(HL)
          LD       (DE),A
          INC      DE
          INC      HL
          OR       A
          JR       NZ,_dnxt
          ; And delete one character at cursor on display
          WRITE_CHR ESC
          WRITE_CHR '['
          WRITE_CHR 'P'
          POP      DE
          POP      HL
          RET

; ---- INCB
; Increment HL and keep it in the bounds of the history buffer
INCB:     INC      HL
          INC      HL

; ---- DECB
; Increment HL and keep it in the bounds of the history buffer
DECB:     DEC      HL
          ; Keep in the 1K history buffer
_scope:   LD       A,3
          AND      H
          OR       HIGH MLBUF
          LD       H,A
          LD       A,(HL)
          RET


BUFCHR:   PUSH     HL
          LD       HL, (INPTR)
          LD       A, (HL)
          OR       A
          JR       Z, eb1
          INC      HL
          LD       (INPTR), HL
eb1:      POP      HL
          RET

; ------ UNGET
; Wind back ONE chatacter unless at start of buffer
UNGET:    PUSH     AF
          PUSH     HL
          PUSH     DE
          LD       HL, INBUF    ; Reselt the line pointer
          LD       DE, (INPTR)
          LD       A,L
          CP       E
          JR       NZ,_unok
          LD       A,H
          CP       D
          JR       Z,_unbad      ; INPTR at start of buffer
_unok:    DEC      DE
          LD       (INPTR),DE
_unbad:   POP      DE
          POP      HL
          POP      AF
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

; Step over all characters that are NOT spaces and set the
; INPTR to the first non-space character.
; Returns:
;  flag Z: set if whitespace or end of input
;       A: first non-space character or 0 for end of string
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

; ----------- SETHIST
; Switch history on or off. A: Boolean: 0: OFF, 1: ON
SETHIST:  LD       (MLINE),A
          RET

; Read only data definitions that go in the code section
_HEX_CHRS: DEFB  "0123456789ABCDEF"
_ENDPRG:

; Data defintions for this module
            dseg
LINELEN:    EQU   128
INPTR:      DEFW  INBUF    ; Position in current line (READ)
CURSOR:     DEFW  INBUF    ; Position in current line (INPUT)
INBUF:      DS    LINELEN
INIT:       DEFB  0        ; Initialised to 'false'. Set true once hist bug initialised.
UPPERCASE:  DEFB  1        ; If true then map lower case to upper case letters. Set at start of getline
MLINE:      DEFB  1        ; If true then implement a line buffer/line editor
LPOS:       DEFW  MLBUF    ; Pointer to where the next history line will be added
HPOS:       DEFW  MLBUF    ; Pointer to place in hist buffer when using up/down arrow keys

; Storage for the previous 'n' lines. Keeping it simple - there's a 1K buffer of variable
; length lines. THIS BLOCK MUST BE ALIGNED ON A 1K BOUNDARY.
            ASEG
            ORG 3800h
MLSZ:       EQU   1024
MLBUF:      DS    MLSZ
