WRITE_CHR  MACRO     chr
    LD A,&chr
    RST 08H
    ENDM
WRITE_CRLF MACRO
    LD A,CR
    RST 08H
    LD A,LF
    RST 08H
    ENDM

; Character constants
NULL       .EQU     00H
CR:        .EQU     0DH
LF:        .EQU     0AH
FF:        .EQU     0CH
BS:        .EQU     08H             ; Backspace
TAB:       .EQU     09H             ; Tab
DEL:       .EQU     7fH             ; Delete
CS:        .EQU     0CH             ; Clear screen
SPC:       .EQU     20H
ESC:       .EQU     1BH
