#define WRITE_CHR(c) LD A,c \ RST 08H

SIO_A_C   .EQU     80H
SIO_A_D   .EQU     81H
SIO_B_C   .EQU     82H
SIO_B_D   .EQU     83H

CR        .EQU     0DH
LF        .EQU     0AH
CS        .EQU     0CH             ; Clear screen
SPC       .EQU     20H

          .ORG     0290H

START:    LD    HL, INTRO
          CALL  PRINT

main:     LD    HL, PROMPT
          CALL  PRINT

          CALL  GET_CHR
          CP    'L'                ; Load
          JR    Z, LOAD
          CP    CR                 ; Load
          JR    Z, main
          CP    'R'
          JR    Z, RUN
          CP    'D'
          JR    Z, DUMP
          RST   08H                ; Echo character
          JP    err

; ------------------- load
LOAD:     LD    HL, WHERE
          CALL  PRINT
          CALL  GET_HEX           ; OFFSET to apply to hex records. Z flag set if 'break' pressed.
          JR    Z, err
          JP  HEX_IMP

; ------------------- run
RUN:      LD    HL, WHERE
          CALL  PRINT
          CALL  GET_HEX           ; OFFSET to apply to hex records. Z flag set if 'break' pressed.
          JR    Z, err
          JP    RUN_AT

; ------------------- dump
DUMP:     LD    HL, WHERE
          CALL  PRINT
          CALL  GET_HEX           ; OFFSET to apply to hex records. Z flag set if 'break' pressed.
          JR    NZ, cont_dump

          ; No value entered so use the stored value as the dump start address
          LD    HL, (DUMP_ADDR)
cont_dump:
          ; Display 5 blocks of 16 characters
          LD    C,5

          ; Dump address (start of line)
dloop2    LD    A,0dh
          RST   08h
          LD    A,0Ah
          RST   08h
          CALL  WRITE_16          ; 4 hex digits from HL

          WRITE_CHR(SPC)
          LD    B,16

dloop:    LD    A,(HL)
          INC   HL
          CALL  WRITE_8
          WRITE_CHR(SPC)
          DJNZ  dloop
          DEC   C
          JR    NZ,dloop2
          LD    (DUMP_ADDR), HL
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
WRITE_8:   PUSH BC
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
           RET


; ----- Load a hex file (from the console input)
HEX_IMP:  LD    HL, DONE
          CALL  PRINT
          JP    main

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
NEXT_HC:  XOR   A
          CALL  GET_CHR
          ; End of input?
          CP    13
          JR    NZ, cont_hc
          LD    A,C
          OR    A
          POP   BC
          RET
cont_hc:
          ; Is it between 0 and 9?
          CP    '0'
          JR    C, NEXT_HC      ; Less than zero so ignore
          CP    'F'+1
          JR    NC, NEXT_HC     ; > 'F' so ignore
          CP    '9'+1
          JR    NC, letter_hc
          RST   08H
          SUB   '0'
          JR    add_chr
letter_hc:
          CP    'A'
          JR    C, NEXT_HC
          RST   08H
          SUB   'A'
          ADD   A, 10

add_chr:  ADD   HL, HL
          ADD   HL, HL
          ADD   HL, HL
          ADD   HL, HL
          ADD   A, L
          LD    L, A
          INC   C
          JR    NEXT_HC

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
HEX_CHRS: .TEXT  "0123456789ABCDEF"

; ---------------------- VARIABLES
DUMP_ADDR: .DW    0
DUMP_BUFFL .DS  132

TEST:      .TEXT 'The quick brown fox jumped a lot'

;data:
;  DEFB 0xc3, 0x3C, 0xAA, 0x55
;  DEFB 0

.END
