#define WRITE_CHR(c) LD A,c \ RST 08H
#define WRITE_CRLF   LD A,CR \ RST 08H \ LD A,LF \ RST 08H

SIO_A_C   .EQU     80H
SIO_A_D   .EQU     81H
SIO_B_C   .EQU     82H
SIO_B_D   .EQU     83H

CR        .EQU     0DH
LF        .EQU     0AH
BS        .EQU     08H             ; Backspace
TAB       .EQU     09H             ; Tab
DEL       .EQU     7fH             ; Delete
CS        .EQU     0CH             ; Clear screen
SPC       .EQU     20H

          .ORG     0200H

START:    LD    HL, INTRO
          CALL  PRINT

main:     LD    HL, PROMPT
          CALL  PRINT
          CALL  GET_LINE

          ; Echo input line
          LD    HL, INBUF

          WRITE_CHR(CR)
          WRITE_CHR(LF)
          ; CALL  PRINT

          CALL  SKIPSPC

          ; CALL  GET_CHR
          CP    'L'                ; Load
          JR    Z, LOAD
          CP    CR                 ; Load
          JR    Z, REPEAT
          CP    LF                 ; Load
          JR    Z, REPEAT
          CP    'R'
          JR    Z, RUN
          CP    'D'
          JR    Z, DUMP
          CP    'R'
          JR    Z, RUN
          RST   08H                ; Echo character
          JP    err

; ------------------- load
REPEAT:   WRITE_CHR(CR)
          WRITE_CHR(LF)
          JP    main

; ------------------- load
LOAD:     CALL  GET_HEX           ; OFFSET to apply to hex records. Z flag set if 'break' pressed.
          JP    HEX_IMP

; ------------------- dump
RUN:      CALL  GET_HEX           ; OFFSET to apply to hex records. Z flag set if 'break' pressed.
          PUSH  HL
          RET
; ------------------- run
DUMP:     CALL  GET_HEX           ; OFFSET to apply to hex records. Z flag set if 'break' pressed.
          JR    NZ, cont_dump

          ; No value entered so use the stored value as the dump start address
          LD    HL, (DUMP_ADDR)
cont_dump:
          ; Display 5 blocks of 16 characters
          LD    C,5

          ; Dump address (start of line)
dloop2    WRITE_CHR(CR)
          WRITE_CHR(LF)
          CALL  WRITE_16          ; 4 hex digits from HL
          WRITE_CHR(SPC)
          LD    DE, DUMP_CHRS+2
          LD    B,16

dloop:    LD    A,(HL)
          CP    20h
          JR    C, outdot
          CP    7fh
          JR    NC, outdot
          LD    (DE),A
          JR    writeout
outdot:   LD    A,'.'
          LD    (DE), A
          LD    A, (HL)
writeout: INC   DE
          INC   HL
          CALL  WRITE_8
          WRITE_CHR(SPC)
          DJNZ  dloop
          PUSH  HL
          LD    HL, DUMP_CHRS
          CALL  PRINT
          POP   HL
          DEC   C
          JR    NZ,dloop2
          LD    (DUMP_ADDR), HL
          WRITE_CHR(CR)
          WRITE_CHR(LF)
          JP    main

err:      LD    HL, ERROR
          CALL  PRINT
          JP    main

; ----------------------------------- Ouput the 16 bit value in HL
WRITE_16:  PUSH AF
           LD   A,H
           CALL WRITE_8
           LD   A,L
           CALL WRITE_8
           POP  AF
           RET
WRITE_8:   PUSH AF
          PUSH BC
           PUSH DE
           PUSH HL
           LD   DE, HEX_CHRS
           LD   B,A
           SRA  A
           SRA  A
           SRA  A
           SRA  A
           AND  0Fh
           LD   L,A
           XOR  A
           LD   H,A
           ADD  HL,DE
           LD   A,(HL)
           RST  08H
           ; And the second nibble
           LD   A,B
           AND  0Fh
           LD   L,A
           XOR  A
           LD   H,A
           ADD  HL,DE
           LD   A,(HL)
           RST  08H
           POP  HL
           POP  DE
           POP  BC
           POP  AF
           RET


; ----- Load a hex file (from the console input)
_waiting: .TEXT "Waiting..."
          .DB    0
_h1:      .TEXT "Here 1" \ .DB 0
_h2:      .TEXT "Here 2" \ .DB 0
_h3:      .TEXT "Here 3" \ .DB 0
_h4:      .TEXT "Here 4" \ .DB 0

invalid:  LD    HL, _NOSTART
          CALL  PRINT
          JP    main
impeof:   LD    HL,_COMPLETE
          CALL  PRINT
          JP    main
rec_err:  LD    HL,_REC_ERR
          CALL  PRINT
          JP    main
here:     LD    HL,_h1
          CALL  PRINT
          JP    main

; Process all lines starting with a ':'
HEX_IMP:  LD    HL, _waiting
          CALL  PRINT
          WRITE_CRLF
;          :102000000F0E0D0C0B0A0908070605040302010008
;          :102010000A0A0A0A0A0A0A0A0A0A0A0A0A0A0A0A08
;          :00000001FF
nextline: CALL  GET_LINE
          JR    Z, nextline
          CALL  BUFCHR
          CP    ':'
          JP    NZ,invalid
          ; Accept this line. Format: [LEN 1][ADDR 2][REC_TYPE 1][DATA 2]+[CHKSM 1]
          CALL  IN_HEX_2  ; Length -> A
          JR    C, rec_err
          LD    B,A       ; Length
          CALL  IN_HEX_4  ; Address - HL
          JR    C, rec_err
          ; CALL  here
          CALL  IN_HEX_2  ; Command - which should be 00. If not then EOF so end.
          JR    C, rec_err
          OR    A
          JR    NZ, impeof
next_b:   CALL  IN_HEX_2
          JR    C, rec_err
          LD   (HL), A
          INC   HL
          DJNZ  next_b
          ; END of that record (ignoring checksum)
          WRITE_CRLF
          JR    nextline
; ----- Load a hex file (from the console input)
RUN_AT:   LD    HL, DONE
          CALL  PRINT
          JP    main

GET_CHR:  XOR   A
          RST   10H                ; Read character
          CP    'a'                ; Lower case -> upper case
          RET    C
          SUB   'a'
          ADD   A,'A'
          RET

; ------------------- GET_HEX - red in up to 4 hex digits, returned in HL
GET_HEX:  LD    HL, 0
          PUSH  BC
          LD    C,0
          CALL  SKIPSPC
          ; End of input?
next_hc:  OR    A
          JR    NZ, cont_hc
fin:      LD    A,C
          OR    A
          POP   BC
          RET
cont_hc:  CALL  HEX_TO_BIN
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

; -------- IN_HEX_2 - Return 2 hex digit value in A. Set C flag on error
IN_HEX_2: PUSH  HL
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

errhex2:  LD    HL,_HEXERR
          CALL  PRINT
          POP   HL
          RET

IN_HEX_4: CALL  IN_HEX_2
          RET   C
          LD    H,A
          CALL  IN_HEX_2
          LD    L,A
          RET

; -------- HEX_TO_BIN Char in A - 0-15. 255 if not valid char
_hexout:    .TEXT  "\r\nCharacter: [" \ .DB 0
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
          PUSH     DE
          PUSH     BC
          LD       HL, INBUF
          LD      (INPTR), HL
          LD       DE, END_INBUF
          LD       BC, 0

getc:     CALL     GET_CHR
          CP       CR
          JR       Z, eol
          CP       BS
          JR       Z, bspc

          ; Store in buffer
          LD      (HL), A

          ; At end of buffer?
          LD      C,A
          LD      A,80
          CP      B
          JR      Z, getc       ; buffer full
          INC     HL
          INC     B
          LD      A,C
          RST     08H
          JR      getc

eol:      XOR     A
          LD      (HL), A
          LD      A, B
          POP     BC
          POP     DE
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
          WRITE_CHR(BS);
          ; WRITE_CHR(DEL);
          JR       getc

BUFCHR:   PUSH     HL
          LD       HL, (INPTR)
          LD       A, (HL)
          OR       A
          JR       Z, eb
          INC      HL
          LD       (INPTR), HL
          POP      HL
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

; --------- GET_HEX_2 - read 2 byte HEX value from input line to A
; Z flag set on OK

; --------------------- PRINT - write a string to the terminal
PRINT:    LD       A,(HL)          ; Get character
          OR       A               ; Is it $00 ?
          RET      Z               ; Then RETurn on terminator
          RST      08H             ; Print it
          INC      HL              ; Next Character
          JR       PRINT           ; Continue until $00

wait:     PUSH HL
          LD H,0ffH
loop2:    LD L,0ffH
loop3:    DEC L
          JR NZ,loop3
          DEC H
          JR NZ,loop2
          POP HL
          RET

; --------------------- STRINGS
INTRO:    .TEXT "\f\r\nZ80 Hex Loader\r\nReady...\r\n"
          .DB    0
PROMPT:   .TEXT "> "
          .DB    0
DONE:     .TEXT "\r\ndone.\r\n"
          .DB    0
WHERE:    .TEXT "\rADDR? "
          .DB    0
ERROR:    .TEXT "\r\nUnknown command\r\n"
          .DB    0
_NOSTART: .TEXT "\r\nNo record start character ':'\r\n"
          .DB    0
_REC_ERR: .TEXT "\r\nBad record\r\n"
          .DB    0
_COMPLETE:.TEXT "\r\nDownload complete\r\n"
          .DB    0
_HEXERR:  .TEXT "\r\nBad hex character\r\n"
          .DB    0
HEX_CHRS: .TEXT  "0123456789ABCDEF"

; ---------------------- VARIABLES
DUMP_ADDR: .DW    0
DUMP_CHRS: .TEXT  "  "
           .DS   16
           .DB    0
INPTR      .DW  INBUF
INBUF      .DS   80
END_INBUF  .DB    0

.END
