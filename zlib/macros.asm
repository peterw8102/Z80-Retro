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

; SIO_C
; Write a value to one of the SIO channel control registers
;   chn: A or B - which channel/port
;   val: Date to write to the port
;
; A NOT PRESERVED
SIO_C MACRO chn,val
    LD  A,val
    OUT (SIO`chn`_C),A
    ENDM

; SIO_WR
; Write a value to one of the SIO write registers
;   chn: A or B - which channel/port
;   reg: The register number
;   val: Date to write to the port
;
; A NOT PRESERVED
SIO_WR MACRO  chn,reg,val
    LD    A,reg
    OUT   (SIO`chn`_C),A
    LD    A,val
    OUT   (SIO`chn`_C),A
    ENDM

; BANK
; Map a specific page number (0-255) into one of the 16KB
; hardware Z80 memory banks. Because of the way macros
; work using text substitution, 'pg' can be an indirect
; reference:
;     BANK   1, 72    - map page 72 into bank 1
;     BANK   2,(PAGE) - map the page number stored at PAGE into bank 2
; Parameters
;     bnk: The destination bank into which the page is to be mapped 0-3
;     pg:  The page number (or indirect reference) 0-255
;
; NOTE (1): 'pg' is optional. If ommitted then whatever is already in A
; is taken to be the page number.
;
; A NOT PRESERVED
BANK MACRO bnk,pg
if ! nul &pg
    LD    A,pg
endif
if ! nul &bnk
    OUT   (PG_PORT0+bnk),A
else
    OUT   (C),A
endif
    ENDM

; BANK_I
; As bank except the target bank reference (0-3) indirect in the specified
; register.
;     BANK   E, 72    - map page 72 into bank in register E
;     BANK   C,(PAGE) - map the page number stored at PAGE into bank 2
; Parameters
;     bnk: The indirect register reference to the bank.
;     pg:  The page number (or indirect reference) 0-255
;
; NOTE (1): Neither 'bnk' not 'pg' may be 'A'. The accumulator is used
; as a working sote and immediately overwritten.
;
; NOTE (2): 'pg' is optional. If ommitted then whatever is already in A
; is taken to be the page number.
;
; A and C NOT PRESERVED
BANK_I MACRO bnkref,pg
    LD    A,PG_PORT0
    ADD   bnkref
    LD    C,A
    LD    A,pg
    OUT   (C),A
    ENDM

; EN_PAGE
; Enable page mapping hardware (the simple MMU). No parameters
; A NOT PRESERVED
EN_PAGE MACRO
    LD    A,$03
    OUT   (PG_CTRL),A        ; Switch to page mode
    ENDM


; CTC_VAL
; Write a control value and timer value to one of the SIO write registers
;   chn: 0-3
;   reg: The register number
;   val: Date to write to the port
;
; A NOT PRESERVED
CTC_VAL MACRO  chn,reg,val
    LD    A,reg
    OUT   (CTC_CH`chn`),A
if ! nul &val
    LD    A,val
    OUT   (CTC_CH`chn`),A
endif
    ENDM
