; import config.asm
import defs.asm

; Large set of low to mid-level functions used to provide console input
; and output:
;
; PRINT:    Send a null terminated string to the console
; PRINT_LN: As 'PRINT' but append CR/LF
;
          extrn  BIN2HEX

          ; Output functions
          public PRINT,PRINT_LN,NL,WRITE_8,WRITE_16, WRITE_D

          CSEG

; ------ WRITE_16
; Convert the 16 bit value in HL to 4 ASCII caracters and send to the console.
; All registers preserved.
WRITE_16:  PUSH AF
           LD   A,H
           CALL WRITE_8
           LD   A,L
           CALL WRITE_8
           POP  AF
           RET

; ------ WRITE_8
; Convert the 8 bit number in A into HEX characters in write to the console
; A: number to write (not preserved)
WRITE_8:   PUSH HL
           CALL BIN2HEX
           LD   A,H
           RST  08H
           LD   A,L
           RST  08H
           POP  HL
           RET

; ------ WRITE_D
; Convert a 16 bit number in HL to an ASCII decimal number and print out
WRITE_D:  PUSH   HL
          PUSH   DE
          PUSH   BC
          CALL   _NUM2D
          POP    BC
          POP    DE
          POP    HL
          RET

_NUM2D:   LD     D,' '
          LD     BC,-10000
          CALL   NUM1
          LD     BC,-1000
          CALL   NUM1
          LD     BC,-100
          CALL   NUM1
          LD     C,-10
          CALL   NUM1
          LD     C,B

NUM1      LD     A,'0'-1
NUM2      INC    A
          ADD    HL,BC
          JR     C,NUM2
          SBC    HL,BC
          CP     A,'0'
          JR     NZ,_isnotz
          LD     A,D
          RST    08H
          RET

_isnotz:  LD     D,'0'
          RST    08h
          RET
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
          ; Drop through to new line

; --------------------- NL
; --- Utility to print a carriage return/new line
NL:       LD       A,CR
          RST      08H
          LD       A,LF
          RST      08H
          RET
