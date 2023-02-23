; import config.asm
import defs.asm

; Large set of low to mid-level functions used to provide console input
; and output:
;
; Utilies:
; DEC2BIN:
;
; Input functions:
; GET_LINE: Read in a complete line, including history butter
; SET_LINE: Set the input line buffer to contain this string (HL)
; EDT_LINE: Edit the content of the input bufffer. Usually set by
;           first calling SET_LINE
; SETHIST:  Switch on or off cursor controls/history in GET_LINE
; GETHIST:  Return a specific line from the history buffer
;
; Processing line returned by GET_LINE:
; BUFCHR:   Consume/return one character from the input buffer
; BUFCHUP:  Consume/return one character from the input buffer, mapping lowercase to uppercase.
; SKIPSPC:  Return the next non-space character.
; WASTESPC: Step over spaces and leave input ptr at first non-space character (without reading it)
; INHEX:    Return a 32 bit number (hex) from the input buffer -> HLDE
; INHEX_4   Return a 16 bit number (hex) from the input buffer -> HL
; INHEX_2:  Return an 8 bit number (hex) in the accumulator
; GET_HEX:  Read a 16 bit hexidecimal number
; GET_DEC:  Read in a 16 bit number in decimal
;
; Data values exported
; INPTR:    Points to the current position in the input line being processed
; INBUF:    Points to the start of the input line (returned by GET_LINE)
; MAP_CASE: (B) Set to 1
;
          ; Utilities
          extrn  ADD8T16,TOUPPER

          ; Input functions
          public GET_LINE,SET_LINE,EDT_LINE
          public SKIPSPC,WASTESPC,BUFCHR,BUFCHUP
          public SETHIST,GETHIST
          public INHEX,INHEX_2,INHEX_4,GET_HEX,GET_DEC

          ; Data values
          public INPTR,INBUF,_ENDPRG
          public RESINP

          ; Utilities
          extrn DEC2BIN,HEX2BIN

          CSEG

; ------------------- GET_HEX
; Read in hex characters to generate a 16 bit number, returned in HL. The 'Z' flag
; is set if the first character processed is NOT a valid hex character. If there are
; MORE than 4 hex characters then the last 4 are used (most sig. ignored). Eg:
; 01A2B3C4D => returns 3C4D
;
; Output:
;    HL: entered value OR zero if no valid characters
;    Z:  flag set if there we NO valid hex characters
;
; AF not preserved
GET_HEX:  PUSH  BC
          LD    B,A  ; Save A
          XOR   A
          LD    H,A
          LD    L,A
          LD    C,A           ;
          CALL  SKIPSPC
next_hc:  OR    A             ; End of input?
          JR    Z, _fin
cont_hc:  CALL  HEX2BIN
          CALL  C,UNGET
          JR    C,_fin
          ADD   HL, HL
          ADD   HL, HL
          ADD   HL, HL
          ADD   HL, HL
          ADD   A, L
          LD    L, A
          INC   C
          CALL  BUFCHR
          JR    next_hc

; -------- GET_DEC - Read a (16 bit) decimal number into HL with C flag on error
; If there is no valid decimal number then the:
;    HL returned as zero
;    C  flag SET
; otherwise the 16 bit number is returned in HL
GET_DEC:  PUSH  BC
          LD    B,A           ; Save A
          XOR   A
          LD    H,A           ; Working value in HL, set to zero
          LD    L,A
          LD    C,A           ; Count number of characters
          CALL  SKIPSPC
next_dc:  JR    Z, _fin       ; Same end caseas GET_HEX
cont_dc:  CALL  DEC2BIN       ; Decimal character?
          CALL  C,UNGET
          JR    C,_fin
          PUSH  DE            ; Muliply HL by 10
          ADD   HL, HL        ; x2
          LD    E,L
          LD    D,H
          ADD   HL, HL        ; x4
          ADD   HL, HL        ; x8
          ADD   HL,DE         ; x10
          POP   DE
          CALL  ADD8T16
          INC   C
          CALL  BUFCHR
          JR    next_dc

_fin:     LD    A,C
          OR    A
          LD    A,B
          POP   BC
          RET

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

; -------- INHEX - Read a 4 byte (32 bit) hex representation
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
          CALL  HEX2BIN
          JR    C,errhexx ; Must be at least ONE hex digit!

          ; Multiple HL by 16
nxtchr:   CALL _shft32

          ; Map in the new digit
          OR    E
          LD    E,A

          CALL  BUFCHR  ; A -> character
          JR    Z, eonum
          CALL  HEX2BIN
          JR    NC, nxtchr

          ; Read a character that's NOT a hex char (or end of line) - unget it
          CALL  UNGET

eonum:    OR    A ; Clear carry
          RET

errhexx:  SCF
          RET

INHEX_4:  CALL  INHEX_2
          RET   C
          LD    H,A
          CALL  INHEX_2
          LD    L,A
          RET


; -------- INHEX_2 - Parse the next TWO characters as an 8bit hex
; number. If the characters are valid hex digits then the converted
; value is returned in the accumulator. If either character is invalid
; then the C flag will be set on exit (clear for a good number)
; INPUTS:  Next two characters from INPTR.
; OUTPUTS: A  - converted hex number if valid (otherwise undefined)
;          C  - Set for an error, clear if number parsesd OK
; Registers NOT preserved: A
INHEX_2:  PUSH  HL
          LD    L,0     ; Value being built
          CALL  BUFCHR  ; A -> character
          CALL  HEX2BIN
          JR    C,errhex2
          LD    L,A     ; First byte
          CALL  BUFCHR  ; A -> character
          CALL  HEX2BIN
          LD    H,A     ; Tmp store
          LD    A,L     ; Current val
          JR    C,onedig
          ADD   A,A
          ADD   A,A
          ADD   A,A
          ADD   A,A
          ADD   A,H     ; Which will leave carry clear
onedig:   POP   HL
          OR    A
          RET
errhex2:  POP   HL
          SCF
          RET



; --------------------- SETHIST
; If A is 1 to enable cursor controls in GET_LINE processing. If on then
; GET_LINE understands up/down/left/right cursors and manages a history
; buffer. Set to 0 to disabled cursor controls.
SETHIST:  LD      (MLINE),A
          RET

; --------------------- SET_LINE
; Write a null terminated string into the input buffer and treat it as though
; it was typed by the user.
; INPUT:  HL  - Pointer to the null terminated string
SET_LINE: PUSH    AF
          PUSH    BC
          PUSH    DE
          PUSH    HL
          CALL    RESBUF      ; Clear buffer
          LD      DE,INBUF
          LD      B,LINELEN   ; Maximum line length allowed
.nextch:  LD      A,(HL)
          LD      (DE),A
          OR      A
          JR      Z,.setpos
          INC     DE
          INC     HL
          DJNZ    .nextch

          ; Store current position
.setpos:  LD      HL,INBUF
          LD      (INPTR),HL
          POP     HL
          POP     DE
          POP     BC
          POP     AF
          RET
; --------------------- GET_LINE
; Block until the user types RETURN. The entered line is in the input buffer (INBUF). The
; line can be processed either using GETCHR/SKIPSPC/WASTESPC etc or the client code can
; access INBUF directly.
;
; The accumulator and flags are NOT preserved. All other registers restored.
;
; On return the Z flag will be set if the entered line is empty, otherwise it
; will be cleared.
GET_LINE: PUSH    HL
          PUSH    BC
          CALL    INITMLN        ; Reset the history pointer
          CALL    RESBUF
          LD      HL,INBUF

          ; Entry point to continue editing the current line. At this point:
          ;    'HL' must point to the current position in the input buffer!
          ;    'B'  must be the current offset into the line.
          ;    'C'  is used to contain the last entered chartacter.
getc:     RST      10H
          LD       C,A           ; Save last character
          CP       ESC           ; Check for significant control characters
          JR       Z, _esc
          CP       CR
          JR       Z, eol
          CP       BS
          JR       Z, bspc
          CP       DEL
          JR       Z, bspc
          CP       VT
          JR       Z, deol

          ; Not a control character so store in buffer, insert space if necessary
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
          INC     B             ; Cursor offset
          LD      A,C
          RST     08H           ; Echo character
          JR      getc

eol:      LD      A,(MLINE)
          OR      A
          JR      Z,nosave      ; History buffer disabled
          LD      A,(INBUF)
          CP      ':'           ; Skip lines starting with ':'
          JR      Z,nosave
          CP      '.'           ; Skip lines starting with '.'
          JR      Z,nosave
          OR      A             ; Is the line empty?
          CALL    NZ,ADD_HIST   ; Add current line to history buffer if line is not empty
nosave:   POP     BC
          POP     HL
          LD      A,(INBUF)
          OR      A             ; Z flag set if no characters entered in line
          RET

_esc:     LD      A,(MLINE)     ; MLINE is true if history buffer (up/down arrows) are enabled
          OR      A
          LD      A,C
          JR      Z,_ext
          ; Escapes are being processed. If it's one we know then do something otherwise ignore.
          RST     10H
          LD      C,A
          CP      '['           ; The only form I understand for arrow characters
          JR      NZ,_ext
          RST     10H       ; It's a ESC-[ - net character must be A,B,C,D or '3'
          LD      C,A
          CP      'A'
          JR      Z,_pline
          CP      'B'
          JR      Z,_nline
          CP      'C'
          JR      Z,_nchr
          CP      'D'
          JR      Z,_pchr
          ; CP      '1'
          ; JR      Z,_stln
          ; CP      '4'
          ; JR      Z,_endl
          CP      '3'           ; It's ESC[3. If the next char is $7E then delete char at current pos.
          JR      NZ,getc
          RST     10H
          CP      $7E
          JR      NZ,getc

          ; Delete the current character
          CALL    DELETE
          JR      getc         ; Not a recognised escape sequence.

; --------- EDT_LINE
; As GET_LINE but use the current content of the INBUF. Cursor positioning
; for this along with getting the correct line display is left to the client code.
;
; Generally clients will call SET_LINE to fill the input buffer with the line
; to be edited then call EDT_LINE for the interractive part.
EDT_LINE: PUSH    HL
          PUSH    BC
          CALL    INITMLN     ; Initialise the multiline history buffer
          LD      HL,INBUF
          LD      B,0         ; Column
.nxt      LD      A,(HL)
          OR      A
          JR      Z,.atend
          RST     08h         ; Print it
          INC     HL          ; Not end of line so move forward
          INC     B
          JR      .nxt

          ; HL points to the null terminator, B is the number of
.atend:   LD      C,0
          JR      getc

; -------------------------
; CURSOR CONTROL FUNCTIONS
; -------------------------

; -- _stln
; Start of line
; TBD

; -- _endl
; End of line
; TBD


; -- _pchr
; Move cursor one space to the left.
_pchr:    LD      A,B
          OR      A
          JR      Z,getc       ; At start of line so can't go back
          DEC     B
          DEC     HL
          WRITE_CHR ESC        ; Shift cursor
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
          INC     A         ; NZ for found
          RET


; -- _pline
; Discard the current line and move to the previous one from the history buffer
_pline:   EX      DE,HL
          LD      HL,(HPOS) ; Move backwards in this history buffer until the previous-but-one OD/FF
          CALL    DECB      ; Points to the previous OD
_pl1:     CALL    DECB      ; Keep going back until OD or FF
          CP      0Dh
          JR      Z,_pl1_st
          INC     A         ; Either an FF or part way through a command
          JR      NZ,_pl1   ; Wasn't an FF so go back further

          ; Found an FFh which means we've got back to the head of the
          ; history queue. Step forward 1 character. If that's FF then
          ; the queue is empty
          CALL    INCB
          INC     A
          JR      NZ,_more
_nmore:   EX      DE,HL     ; No more history entries. Leave everything unchanged
          JR      getc

; -- _nline
; Discard the current line and move to the next one from the history buffer
_nline:   EX      DE,HL
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
          JR      _RESLN

_nmoren:  LD      A,ESC    ; restore cursor to start of line
          RST     08H
          LD      A,'8'
          RST     08H
          LD      A,ESC    ; clear to end of line
          RST     08H
          LD      A,'['
          RST     08H
          LD      A,'K'
          RST     08H
          CALL    RESBUF
          LD      HL,INBUF ; HL points to start of line
          LD      BC,0
          JR      getc

; ------ _RESLN
; Copy the current history line to the input buffer and update display.
_RESLN:   PUSH    DE

          CALL    RESBUF    ; Clear buffer

          ; Restore cursor
          LD      A,ESC
          RST     08H
          LD      A,'8'
          RST     08H

          LD      HL,(HPOS)  ; History buffer line
          LD      DE,INBUF
          LD      B,0
_nch3:    LD      A,(HL)
          CP      0Dh
          JR      Z,_eohl
          LD      (DE),A
          RST     08H        ; Print it
          INC     DE
          INC     B          ; Cursor position (num chars)
          CALL    INCB
          JR      _nch3

_eohl:    EX      DE,HL      ; Current position in buffer must be in HL before jumping to getc
          POP     DE

          ; Clear to end of line
          LD      A,ESC
          RST     08H
          LD      A,'['
          RST     08H
          LD      A,'K'
          RST     08H

          ; Jump back into the get-line loop
          JR      getc

; ----- RESBUF -----
; Clear the buffer, reset the CURSOR and INPTR
RESBUF:   PUSH    HL
          LD      HL,INPTR
          LD      B,LINELEN
          XOR     A
.clrnxt:  LD      (HL),A
          INC     HL
          DJNZ    .clrnxt
          LD      HL,INBUF

          LD      (INPTR),HL
          LD      BC,0
          POP     HL
          RET

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
; Set the history position to the first history line and then check whether the
; multiline buffers have been initialised. If not then initialise now.
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
; INPUTS: B  - requested history index
;    index 0 is the most recent history item
; OUTPUT: HL - Points to the start of the line, teminated with an O
;              Return Z:  No history item
;                    NZ: Returned item pointed to by HL
GETHIST:  PUSH     BC
          PUSH     DE
          LD       DE,INBUF
          INC      B           ; Index counter. want one more than requested.
          LD       HL,(LPOS)
_gh1:     CALL     DECB        ; Points to the previous OD (or FF)
          INC      A           ; If DECB returns 0xFF then no more lines
          JR       Z,_gh0      ; Wasn't an 0D so no line
          CALL     _hprev      ; Step back - found if NZ
          JR       Z,_gh0
          ; HL points to start of line - need to look back further?
          DJNZ     _gh1

          LD       DE,INBUF

          ; Got the line wanted, copy to the line buffer
_gh3:     LD       A,(HL)
          CP       $0D
          JR       Z,_gh2
          LD       (DE),A
          INC      DE
          CALL     INCB
          JR       _gh3

_gh2:     LD       HL,INBUF     ; The return value so the caller can find the string
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
_scope:   LD       A,(HIGH MLSZ)-1
          AND      H
          OR       HIGH MLBUF
          LD       H,A
          LD       A,(HL)
          RET

; ------ BUFCHR
; Consume and return the next character from the input line buffer (GET_LINE).
; If there are no more characters in the buffer then return zero and set
; the Z flag.
; Return:
;   A:  The next character from the buffer or zero if no more characters
;   Z:  Z flag set if at end of buffer.
BUFCHR:   PUSH     HL
          LD       HL, (INPTR)
          LD       A, (HL)
          OR       A
          JR       Z, eb1
          INC      HL
          LD       (INPTR), HL
eb1:      POP      HL
          RET


; ------ BUFCHUP
; As BUFCHR but promote lower case to uppercase
; Return:
;   A:  The next character from the buffer or zero if no more characters
;   Z:  Z flag set if at end of buffer.
BUFCHUP:  CALL      BUFCHR
          CALL      TOUPPER
          OR        A           ; Fix Z flag
          RET


; ------ UNGET
; Wind back ONE character unless at start of buffer. This function is NOT currently exported.
; All registers are preserved.
UNGET::   PUSH     AF
          PUSH     HL
          PUSH     DE
          LD       HL, INBUF    ; Start of buffer
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

; ---- RESINP
; Reset the line input buffer to start of line.
RESINP:   PUSH     HL
          LD       HL, INBUF     ; Reset the line pointer
          LD       (INPTR), HL
          POP      HL
          RET


_ENDPRG:

; Data defintions for this module
            dseg
LINELEN:    EQU   128
INPTR:      DEFW  INBUF    ; Position in current line (READ)
INBUF:      DS    LINELEN
INIT:       DEFB  0        ; Initialised to 'false'. Set true once hist bug initialised.
MLINE:      DEFB  1        ; If true then implement a line buffer/line editor
LPOS:       DEFW  MLBUF    ; Pointer to where the next history line will be added
HPOS:       DEFW  MLBUF    ; Pointer to place in hist buffer when using up/down arrow keys

; Storage for the previous 'n' lines. Keeping it simple - there's a 1K buffer of variable
; length lines. THIS BLOCK MUST BE ALIGNED ON A 1K BOUNDARY.
            ASEG
            ORG   D_HISTB
MLBUF:      DS    MLSZ
