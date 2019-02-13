;==================================================================================
; Original by Grant Searle
; Modified by
; http://searle.hostei.com/grant/index.html
; eMail: home.micros01@btinternet.com
;
; Modifed by smbaker@smbaker.com for use as general-purpose IO for nixie tube
; clock. Also added support for CTC chip. Switched to SIO implementation instead
; of 68B50. Removed all basic-related stuff.
; (https://github.com/sbelectronics/rc2014/blob/master/asm/intsio.asm)
;
; Heavily re-written and stripped down by petew@yellowhawk.co.uk to drive a minimal
; Z80 board using a single SIO. CTC code removed and port B removed. Also added in
; a hacky Ctrl-C tracker with call out in the SIO interrupt routine to allow my
; debugger to break into errant code. Works fine with unmodified RC2014
; Nascom/Microsoft BASIC.
;
; Interrupts:
;    RST08 - TX the character in A reg on port A
;    RST10 - RX a character from port A
;    RST18 - Check port status on Port A
;    RST20 - Initialise the SIO
;    RST38 - Hardware interrupt from SIO
;
;
;==================================================================================
; import config.asm
import defs.asm

                extrn  START
                public INITSIO

; Full input buffering with incoming data hardware handshaking
; Handshake shows full before the buffer is totally filled to allow run-on from the sender

SER_BUFSIZE     .EQU     $F8
SER_FULLSIZE    .EQU     30H
SER_EMPTYSIZE   .EQU     5

SIOA_D          .EQU     $81
SIOA_C          .EQU     $83
SIOB_D          .EQU     $80
SIOB_C          .EQU     $82

RTS_HIGH        .EQU    0E8H
RTS_LOW         .EQU    0EAH

                ASEG
                .ORG $0000
;------------------------------------------------------------------------------
; Reset

RST00           DI                ; Disable interrupts
                JP       START    ; Initialize Hardware and go

                .ORG $0008
;------------------------------------------------------------------------------
; TX a character over RS232

;                .ORG     0008H
RST08            JP      TXA

;------------------------------------------------------------------------------
; RX a character over RS232 Channel, hold here until char ready.
; Reg A = 0 for port A, 1 for port B

                 .ORG 0010H
RST10             JP      RXA
BRK_HANDLER_ADD: .DW     0

;------------------------------------------------------------------------------
; Check serial status
; Reg A = 0 for port A, 1 for port B

                .ORG 0018H
RST18            JP      CKINCHAR
;------------------------------------------------------------------------------
; Initialise the SIO and return
; Reg A = 0 for port A, 1 for port B

                .ORG 0020H
RST20            JP      INITSIO

;------------------------------------------------------------------------------
; RST 38 - INTERRUPT VECTOR [ for IM 1 ]

                .ORG     0038H
RST38
serialInt:      PUSH     HL
                PUSH     AF

                XOR      A
                OUT      (SIOA_C),A
                IN       A, (SIOA_C)
                RRCA
                JR       NC, rts0

                IN       A,(SIOA_D)
                ; Check if this is a break character and if it is has anyone registered a break handler?
                CP       3h
                ; CTRL-C - is there a handler?
                PUSH     AF
                JR       NZ,proc
                LD       A,(BRK_HANDLER_ADD)
                OR       A
                LD       L,A
                LD       A,(BRK_HANDLER_ADD+1)
                LD       H,A
                JR       NZ,brk
                OR       A
                JR       NZ,brk

proc:           LD       A,(serBufUsed)
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
                CP       serInPtr
                JR       NZ, notWrap
                LD       HL,serBuf
notWrap:        LD       (serInPtr),HL
                POP      AF
                LD       (HL),A

rts0:           POP      AF
                POP      HL
                EI
                RETI
                ; There is a break handler. Hack the stack so we return to the
                ; handler
brk:            POP      AF  ; The character code we read (CTRL-C) no longer needed
                POP      AF  ; AF as it was before the call
                ; Next thing on the return stack is the pushed HL register. Swap this with the handler address
                EX      (SP),HL
                EI
                RETI

                CSEG
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
                CP       serInPtr
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
conout1:        XOR      A
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
; Initialise the SIO handlers. Assumed we're running from RAM and called from
; the application runtime and the memory paging is initialised. This library doesn't
; understand how the memory is managed. Alos assumg the stack has been initialised.
INITSIO:      XOR     A                ; write 0
              LD      C,SIOA_C
              OUT     (C),A
              LD      A,$18            ; reset ext/status interrupts
              OUT     (C),A

              LD      A,$04            ; write 4
              OUT     (C),A
              LD      A,$C4            ; X64, no parity, 1 stop
              ;LD      A,$84           ; X32, no parity, 1 stop
              OUT     (C),A

              LD      A,$01            ; write 1
              OUT     (C),A
              LD      A,$18            ; interrupt on all recv
              OUT     (C),A

              LD      A,$03            ; write 3
              OUT     (C),A
              LD      A,$E1            ; 8 bits, auto enable, rcv enab
              OUT     (C),A

              LD      A,$05            ; write 5
              OUT     (C),A
              LD      A,RTS_LOW        ; dtr enable, 8 bits, tx enable, rts
              OUT     (C),A

              LD      C,SIOB_C
              XOR     A
              OUT     (C),A
              LD      A,$18
              OUT     (C),A

              LD      A,$02           ; write reg 2
              OUT     (C),A
              LD      A,$E0           ; INTERRUPT VECTOR ADDRESS
              OUT     (C),A

              ; initialize first serial port
              LD        HL,serBuf
              LD        (serInPtr),HL
              LD        (serRdPtr),HL
              XOR       A               ;0 to accumulator
              LD        (serBufUsed),A

              ; enable interrupts
              IM        1
              EI
              RET


                DSEG
serBuf          .DS      SER_BUFSIZE
serInPtr        .DW      serBuf
serRdPtr        .DW      serBuf
serBufUsed      .DB      0
_testaddr       .DB      0AAh

; serInMask        DEFL    serInPtr & $FF

;.END
