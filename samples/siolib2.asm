;==================================================================================
; Original by Grant Searle
; Modified by
; http://searle.hostei.com/grant/index.html
; eMail: home.micros01@btinternet.com
;
; Modifed by smbaker@smbaker.com for use as general-purpose IO for nixie tube
; clock. Also added support for CTC chip. Switched to SIO implementation instead
; of 68B50. Removed all basic-related stuff.
;
; Interrupts:
;    RST08 - TX the character in A reg on port A
;    RST10 - RX a character from port A
;    RST18 - Check port status on Port A
;    RST20 - TX the character in A reg on port B
;    RST28 - Set baud rate (A is 1=1200, 2=2400, 9=9600, 19=19200, 115=115200)
;    RST38 - Hardware interrupt from SIO
;
;==================================================================================

; Full input buffering with incoming data hardware handshaking
; Handshake shows full before the buffer is totally filled to allow run-on from the sender

SER_BUFSIZE     .EQU     3FH
SER_FULLSIZE    .EQU     30H
SER_EMPTYSIZE   .EQU     5

SIOA_D          .EQU     $81
SIOA_C          .EQU     $80
SIOB_D          .EQU     $83
SIOB_C          .EQU     $82

RTS_HIGH        .EQU    0E8H
RTS_LOW         .EQU    0EAH

TEMPSTACK       .EQU     $7FF0           ; temporary stack somewhere near the
                                         ; end of high mem

CR              .EQU     0DH
LF              .EQU     0AH
CS              .EQU     0CH             ; Clear screen

                .ORG $0000
;------------------------------------------------------------------------------
; Reset

RST00           DI                       ;Disable interrupts
                JP       INIT            ;Initialize Hardware and go

;------------------------------------------------------------------------------
; TX a character over RS232

                .ORG     0008H
RST08            JP      TXA

;------------------------------------------------------------------------------
; RX a character over RS232 Channel, hold here until char ready.
; Reg A = 0 for port A, 1 for port B

                .ORG 0010H
RST10            JP      RXA

;------------------------------------------------------------------------------
; Check serial status
; Reg A = 0 for port A, 1 for port B

                .ORG 0018H
RST18            JP      CKINCHAR

;------------------------------------------------------------------------------
; RST 38 - INTERRUPT VECTOR [ for IM 1 ]

                .ORG     0038H
RST38            JR      serialInt

;------------------------------------------------------------------------------
serialInt:      PUSH     AF
                PUSH     HL

                XOR      A
                OUT      (SIOA_C),A
                IN       A, (SIOA_C)
                RRCA
                JR       NC, rts0

                IN       A,(SIOA_D)
                PUSH     AF
                LD       A,(serBufUsed)
                CP       SER_BUFSIZE     ; If full then ignore
                JR       NZ,notFull
                POP      AF
                JR       rts0

notFull:        INC      A
                LD       (serBufUsed),A
                CP       SER_FULLSIZE
                JR       C,norts
                ; set rts high
                LD       A, $05
                OUT      (SIOA_C),A
                LD       A,RTS_HIGH
                OUT      (SIOA_C),A
norts:          LD       HL,(serInPtr)
                INC      HL
                LD       A,L             ; Only need to check low byte becasuse buffer<256 bytes
                CP       serInMask
                JR       NZ, notWrap
                LD       HL,serBuf
notWrap:        LD       (serInPtr),HL
                POP      AF
                LD       (HL),A

rts0:           POP      HL
                POP      AF
                EI
                RETI

;------------------------------------------------------------------------------
RXA:
waitForChar:    LD       A,(serBufUsed)
                OR       A
                JR       Z, waitForChar
                DI
                PUSH     HL
                LD       HL,(serRdPtr)
                INC      HL
                LD       A,L             ; Only need to check low byte becasuse buffer<256 bytes
                CP       serInMask
                JR       NZ, notRdWrap
                LD       HL,serBuf
notRdWrap:      ; DI
                LD       (serRdPtr),HL
                LD       A,(serBufUsed)
                DEC      A
                LD       (serBufUsed),A
                CP       SER_EMPTYSIZE
                JR       NC,rts1
                ; set rts low
                LD       A, $05
                OUT      (SIOA_C),A
                LD       A,RTS_LOW
                OUT      (SIOA_C),A
rts1:           LD       A,(HL)
                EI
                POP      HL
                RET                      ; Char ready in A

;------------------------------------------------------------------------------
TXA:            PUSH     AF              ; Store character
conout1:        SUB      A
                OUT      (SIOA_C),A
                IN       A,(SIOA_C)
                RRCA
                BIT      1,A             ; Set Zero flag if still transmitting character
                JR       Z,conout1       ; Loop until flag signals ready
                POP      AF              ; Retrieve character
                OUT      (SIOA_D),A      ; Output the character
                RET
;------------------------------------------------------------------------------
CKINCHAR:       LD       A,(serBufUsed)
                OR       A
                RET

;------------------------------------------------------------------------------
INIT:          LD        C, 224          ; Set up bank select register.
               LD        A, 10h          ; RAM page 1 in 32K[2]
               OUT       (C), A
               LD        HL,TEMPSTACK    ; Temp stack
               LD        SP,HL           ; Set up a temporary stack

;       Initialise SIO

                LD      A,$00            ; write 0
                OUT     (SIOA_C),A
                LD      A,$18            ; reset ext/status interrupts
                OUT     (SIOA_C),A

                LD      A,$04            ; write 4
                OUT     (SIOA_C),A
                LD      A,$C4            ; X64, no parity, 1 stop
                OUT     (SIOA_C),A

                LD      A,$01            ; write 1
                OUT     (SIOA_C),A
                LD      A,$18            ; interrupt on all recv
                OUT     (SIOA_C),A

                LD      A,$03            ; write 3
                OUT     (SIOA_C),A
                LD      A,$E1            ; 8 bits, auto enable, rcv enab
                OUT     (SIOA_C),A

                LD      A,$05            ; write 5
                OUT     (SIOA_C),A
                LD      A,RTS_LOW        ; dtr enable, 8 bits, tx enable, rts
                OUT     (SIOA_C),A

                LD      A,$00
                OUT     (SIOB_C),A
                LD      A,$18
                OUT     (SIOB_C),A

                ; LD      A,$04            ; write 4
                ; OUT     (SIOB_C),A
                ; LD      A,$44            ; X16, no parity, 1 stop
                ; OUT     (SIOB_C),A
                ;
                ; LD      A,$01
                ; OUT     (SIOB_C),A
                ; LD      A,$18
                ; OUT     (SIOB_C),A

                LD      A,$02           ; write reg 2
                OUT     (SIOB_C),A
                LD      A,$E0           ; INTERRUPT VECTOR ADDRESS
                OUT     (SIOB_C),A

                ; LD      A,$03
                ; OUT     (SIOB_C),A
                ; LD      A,$E1
                ; OUT     (SIOB_C),A

                ; LD      A,$05
                ; OUT     (SIOB_C),A
                ; LD      A,RTS_LOW
                ; OUT     (SIOB_C),A

               ; initialize first serial port
               LD        HL,serBuf
               LD        (serInPtr),HL
               LD        (serRdPtr),HL
               XOR       A               ;0 to accumulator
               LD        (serBufUsed),A

               ; enable interrupts
               IM        1
               EI

               JP        $160             ; Run the program

               .ORG     $110
serBuf          .DS      SER_BUFSIZE
serInPtr        .DW      serBuf
serRdPtr        .DW      serBuf
serBufUsed      .DB      0
serInMask       .EQU     serInPtr&$FF


.END
