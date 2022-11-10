; MACROS
; A few utility macros

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
