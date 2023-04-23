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

                public INITSIO,SIO_OFF,TXA,RXA,CKINCHAR,_ENDLIBS,SERINT
                extrn  BRK_HK

; Full input buffering with incoming data hardware handshaking
; Handshake shows full before the buffer is totally filled to allow run-on from the sender

; EXPORTED FUNCTIONS:
;    TXA:       Transmit a character on port A. USUALLY INSTALLED AS RST 08h
;    RXA:       Wait for a character on port A. USUALLY INSTALLED AS RST 10h
;    CKINCHAR:  Check to see whether there's a character in the port A buffer. Z if no characters. NZ otherwise
;                                               USUALLY INSTALLED AS RST 18h
;    INITSIO:   Initialise the SIO for both ports A and B - installed on RST 20h but never used in that way!
;    SERINT:    Interrupt handler - needs to be installed at RST 38h as part of INITSIO

SER_BUFSIZE     .EQU     $F0
SER_FULLSIZE    .EQU     SER_BUFSIZE / 2
SER_EMPTYSIZE   .EQU     10H

                CSEG

SERINT:         PUSH     AF
nextchr:        XOR      A
                OUT      (SIOA_C),A
                IN       A, (SIOA_C)      ; Read Reg 0: Bit 0: Character available
                RRCA
                JR       NC, rts0         ; Should be a character but maybe not...

                IN       A,(SIOA_D)       ; Get the character - frees the input FIFO

                PUSH     HL

                ; Check if this is a break character and if it is has anyone registered a break handler?
                CP       3h               ; Character still in A

                ; CTRL-C - is there a handler?
                JR       Z,brk

                ; If the break handler isn't installed continue from here.
proc:           PUSH     AF              ; Make sure we have enough room for this character
                LD       A,(serBufUsed)
                CP       SER_BUFSIZE     ; If full then ignore
                JR       Z,_full

                INC      A
                LD       (serBufUsed),A
                CP       SER_FULLSIZE    ; Check H/W flow control
                JR       NC,setrts

norts:          LD       HL,(serInPtr)
                INC      HL
                LD       A,L             ; Only need to check low byte becasuse buffer<256 bytes
                CP       serInPtr
                JR       NZ, notWrap
                LD       HL,serBuf
notWrap:        LD       (serInPtr),HL
                POP      AF
                LD       (HL),A
                POP      HL
                ; Before leaving, check for another character to save an interrupt.
                JR       nextchr

                ; set rts high
setrts:         SIO_WR   A,5,RTS_HIGH
                JR       norts

_full:          POP      AF
                POP      HL
                ; Drop through to return

                ; No characters left...
rts0:           POP      AF
                RET

                ; There is a break handler. Hack the stack so we return to the
                ; handler
brk:            LD       HL,(BRK_HK)
                LD       A,L            ; Null break handler?
                OR       H
                LD       A,3
                JR       Z,proc

                ; The break handler will have a stack that looks like:
                ;    Ret address from SERINT
                ;    AF at start of ISR
                ;    HL at start of ISR
                ;    AF containing the break character
                ;    <--- SP
                ; Still running in the interrupt routine (needs a RETI at some point)
                JP       (HL)

;------------------------------------------------------------------------------
RXA:
waitForChar:    LD       A,(serBufUsed)
                OR       A
                JR       Z, waitForChar
                PUSH     HL
                LD       HL,(serRdPtr)
                INC      HL
                LD       A,L             ; Only need to check low byte becasuse buffer<256 bytes
                CP       serInPtr
                JR       NZ, notRdWrap
                LD       HL,serBuf
notRdWrap:      LD       (serRdPtr),HL
                DI
                LD       A,(serBufUsed)
                DEC      A
                LD       (serBufUsed),A
                CP       SER_EMPTYSIZE
                JR       NC,rts1

                ; set rts low
                SIO_WR   A,5,RTS_LOW

rts1:           LD       A,(HL)
                EI
                POP      HL
                CP       7fh             ; If it's a DEL character then make it a backspace
                RET      NZ
                LD       A,08h
                RET                      ; Char ready in A

;------------------------------------------------------------------------------
TXA:            PUSH     AF              ; Store character
conout1:        XOR      A
                OUT      (SIOA_C),A
                IN       A,(SIOA_C)
                BIT      2,A             ; Set if still transmitting character
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
; understand how the memory is managed. Also assuming the stack has been initialised.
INITSIO:      DI
              PUSH     AF

              ; initialize input buffer for Port A - Port B is NOT currently buffered
              LD        HL,serBuf
              LD        (serInPtr),HL
              LD        (serRdPtr),HL
              XOR       A
              LD        (serBufUsed),A

              ; Fully disable/reset the SIO channels
              CALL    SIO_OFF

              ; Start programming both channels from scratch.

              ; SET INTERRUPT VECTOR - ONLY USED FOR PORT A but set through port B. Port A is polled.
              SIO_WR  B,2,SIO_INTV   ; INTERRUPT VECTOR ADDRESS

              ; INITIALISE CHANNEL A
              SIO_WR  A,1,00011000b  ; interrupt on all recv
              SIO_WR  A,3,11100001b  ; 8 bits, auto enable, rcv enable
              SIO_WR  A,4,11000100b  ; X64, no parity, 1 stop - 230400 baud with 14MHz sys clock
              SIO_WR  A,5,RTS_LOW    ; dtr enable, 8 bits, tx enable, rts OFF

              ; INITIALISE CHANNEL B
              SIO_WR  B,1,00000100b  ; Uses different vectors for each interrupt. Not really used right now!
              SIO_WR  B,3,11000001b  ; WR3 - Enable receiver, 8 bits per character
              SIO_WR  B,4,10000100b  ; WR4 - 32x Clock - 460800 baud with 14MHz sys clock, 1 stop bit, no parity.
              SIO_WR  B,5,RTS_LOW    ; WR5 - TX 8 bits/char, Enable TX

              POP     AF
              RET

; ------ Close down both channel 1 and 2, prevent further interrupts.
SIO_OFF:      XOR       A
              LD        (serBufUsed),A

              ; Channel A - RESET
              SIO_C   A,00011000b    ; Channel reset
              SIO_C   A,00110000b    ; Error reset
              SIO_WR  A,1,0          ; Disable interrupts

              ; Channel B - RESET
              SIO_C   B,00011000b    ; Channel reset
              SIO_C   B,00110000b    ; Error reset
              SIO_WR  B,1,0          ; Disable interrupts
              RET

_ENDLIBS:
                DSEG
serBuf          .DS      SER_BUFSIZE
serInPtr        .DS      2
serRdPtr        .DS      2
serBufUsed      .DS      1

useisr          .DS      1

;.END
