;==================================================================================
; libkbd
; Input driver for the Omega keyboard via the PIO card. The driver is exposed
; as a keyboard scan routine which *should* be called frequently to check for
; pressed keys. The routine converts scanned keys into their ASCII equivalents
; and, in the case of cursor control/function keys into their VT100/ANSI equivalents.
;
; Decoded keys are placed into an input buffer wgucg simulates the buffer
; used in SIO driver.
;==================================================================================
; import config.asm
import defs.asm

                extrn  BRK_HK,WRITE_8,WRITE_16,NL

                public KBDINIT,KBDSCAN,KBDCHAR,KBDCHK

; Store processed characters. Doesn't need to be any longer than the
; longest VT100 expansion.
KBD_BUFSIZE     .EQU     $8      ; Doesn't need to be very long. Just enough
CODE_BUFSIZE    .EQU     $80      ; Undecoded characters (row/col codes)

                CSEG




; ---- KBDINIT
; Initialise the input buffers
KBDINIT:        PUSH     HL
                LD       HL,codeBuf
                LD       (codeWrPtr),HL
                LD       (codeRdPtr),HL
                LD       HL,kbdBuf
                LD       (kbdWrPtr),HL
                LD       (kbdRdPtr),HL
                XOR      A
                LD       (kbdCnt),A
                LD       (kbdMode),A
                LD       (kbdCaps),A
                LD       A,18h
                LD       (kbdState),A
                POP      HL
                RET


; ---- CLRSCAN
; Clear the previous scan codes. This will effectively enable auto-repeat.
CLRSCAN:        PUSH     HL
                PUSH     BC
                PUSH     AF
                LD       HL,kbdState
                LD       B,18
                XOR      A
.clrnxt:        LD       (HL),A
                INC      HL
                DJNZ     .clrnxt
                POP      AF
                POP      BC
                POP      HL
                RET



; ---- KBDSCAN
; A low level scan reporting all keys pressed. Assumed to be called from an ISR
; and so optimised to return as soon as ppssible on no key pressess.
KBDSCAN:        PUSH     AF
                PUSH     BC
                PUSH     DE
                PUSH     HL

                LD       HL,kbdState

                ; Poll each scan row and return if no keys pressed.
                LD       A,(kbdBase)      ; Base value
                LD       B,9              ; Row count
                LD       C,0              ; Change flag (any keys up or down)
                LD       D,0              ; there are rows with keys pressed

_nxts:          PUSH     AF
                OUT      (KBD_IO),A       ; Select keyboard row
                IN       A,(KBD_IO)       ; Result...
                CPL                       ; Want a '1' for pressed keys
                LD       E,A              ; Store scan row for later
                OR       D                ; Flag for ANY keys pressed
                LD       D,A
                LD       (HL),E           ; Store the new value
                INC      HL
                LD       A,(HL)           ; Have any bits changed?
                XOR      E
                OR       C                ; If bits have changed then A!=0. 'C' is non-zero
                LD       C,A              ; C will be non-zero at the end of there are any changes
                INC      HL               ; Ready for next row
                POP      AF
                INC      A
                DJNZ     _nxts            ; Any more rows to check?

                LD       A,C
                OR       A                ; If any keys change state then reset
                JR       Z,.down          ; No keys pressed

.reset          LD       A,(rpCount)
                OR       A
                LD       A,reRep
                JR       Z,.fast
                LD       A,firstRpt
.fast:          LD       (rpCount),A
                JR       .cdown

.down:          LD       A,(rpCount)       ; Timeout for keyboard repeat?
                DEC      A
                LD       (rpCount),A
                CALL     Z,CLRSCAN
.cdown:         LD       A,C
                OR       A
                JR       NZ,_saveState

                POP      HL
                POP      DE
                POP      BC               ; All done, no keys pressed. Return as quickly as possible
                POP      AF
                RET



                ; At this point the kbdState array contains the results of the old and new scan rows and
                ; we know that there's at least one key that's changed state. Pull out each changed
                ; combination and add them to the event queue.

                ; First, build the meta key status
_saveState::    LD       A,(kbdState+menu2Row*2)
                AND      20h                       ; Ignore everything except MNU
                LD       E,A
                LD       A,(kbdState+fn2Row*2)
                AND      40h                       ; Ignore everything except FN2
                OR       E
                LD       E,A
                LD       A,(kbdState+stateRow*2)
                AND      17h                       ; Mask out non meta keys
                OR       E
                LD       E,A                       ; Save meta key modes

                ; 'E' now contains the meta state for the keyboard for this key scan.
                ;    x 1 1 1 x 1 1 1
                ;      | | |   | | +-----> SHIFT
                ;      | | |   | +-------> CTRL
                ;      | | |   +---------> Option
                ;      | | +-------------> FN1
                ;      | +---------------> Menu
                ;      +-----------------> FN2
                LD       (kbdMode),A               ; Store for later

                ; Copy changed keys into the processing queue (without decoding)
                LD       D,0           ; Current row number
                LD       B,9           ; Number of rows
                LD       HL,kbdState   ; Step back through the table
_pnxtrow:       LD       A,(HL)        ; New state
                LD       C,A
                INC      HL
                XOR      A,(HL)        ; Compare with last poll. 1s mean a changed state
                LD       (HL),C        ; Store the changed state
                AND      C             ; a '1' now means a key has been pressed in this row

                ; At least one key pressed. Output this row for processing,
                ; D:   Row number
                ; A:   Changed key
                ; E:   Meta keys
                CALL     NZ,saverow

                INC      HL            ; Next row
                INC      D
                DJNZ     _pnxtrow

                ; Deal with auto-repeat countdown
                ; All rows processed and queued so OK to return.
.cont           POP      HL
                POP      DE
                POP      BC               ; All done, no keys pressed. Return as quickly as possible
                POP      AF
                RET

; ------ saverow
; HL: Points to the current location in kbdState (the previous poll result)
; D:  Row number
; A:  Key changes
; E:  Meta keys
;
; Each entry in the code buffer is three bytes:
;    [META][ROW][KEYS]
;
; Preserve: HL, BC, DE
saverow:        PUSH     HL
                PUSH     DE
                LD       HL,(codeWrPtr)
                LD       (HL),E            ; Save Meta
                INC      HL
                LD       (HL),D            ; Row number
                INC      HL
                LD       (HL),A            ; Changed keys
                INC      HL

                ; Wrap around?
                LD        A,L
                CP        low (codeBuf + CODE_BUFSIZE * 3)
                JR        NZ,.nowrap

                ; Wrapped
                LD        HL,codeBuf

                ; Is the buffer full? (HL = codeRdPtr)
                ; Buffer is <256 bytes so only compare the low 8 bits
.nowrap:        LD        A,(codeRdPtr)
                CP        L
                JR        Z,.full
                LD        (codeWrPtr),HL
.full:          POP       DE
                POP       HL
                RET

; ------ RXSCAN
; Return the next entry from the scan queue
RXSCAN::        PUSH      HL

                LD        HL,(codeRdPtr)
                LD        A,(codeWrPtr)
                CP        L         ; If the same then there are no scan codes to process
                JR        Z,.empt

                ; Three bytes are: [META][ROW][KEYS]
                LD        D,(HL)       ; Store the meta flags for later
                INC       HL
                LD        E,(HL)       ; Row number
                INC       HL
                LD        C,(HL)       ; The scan code
                INC       HL           ; Pointing to next entry (or overflow)

                LD        A,L
                CP        low (codeBuf + ( CODE_BUFSIZE * 3 ) )
                JR        NZ,.nowrap
                LD        HL,codeBuf
.nowrap:        LD        (codeRdPtr),HL

                POP       HL
                LD        A,1
                OR        A
                RET

.empt:          XOR       A
                POP       HL
                RET



; PRIMEBUF
; Take the next entry in the codeBuf and use it to add characters
; into the kbdBuf. If the scan code results in no characters moving
; to the kbdBuf (because some scan codes do things like change caps lock)
; then process the next code. Keep going until the number of characters
; in kbdBuf is greater than zero.
PRIMEBUF:       PUSH      HL
                PUSH      DE
                PUSH      BC

nxtscan:        LD        HL,(codeRdPtr)
                LD        A,(codeWrPtr)
                CP        L         ; If the same then there are no scan codes to process
                JR        Z,_empt


                ; Three bytes are: [META][ROW][KEYS]
                LD        A,(HL)    ; Meta flags. Need to choose which key table to use
                LD        C,A       ; Store the meta flags for later

                ; Meta state in A for the keyboard for this key scan.
                ;    x 1 1 1 x 1 1 1
                ;      | | |   | | +-----> SHIFT
                ;      | | |   | +-------> CTRL
                ;      | | |   +---------> Option
                ;      | | +-------------> FN1
                ;      | +---------------> Menu
                ;      +-----------------> FN2
                LD        DE,tab_std
                RRA
                JR        NC,no_shift
                LD        DE,tab_shft
                JR        decode

no_shift:       RRA
                JR        NC,no_ctrl
                LD        DE,tab_ctrl
                JR        decode

no_ctrl:        LD        A,(kbdCaps)
                OR        A
                JR        Z,decode
                LD        DE,tab_caps

decode:         LD        (kbdMap),DE   ; Store the selected keyboad map

                INC       HL
                LD        A,(HL)        ; The row number. Translate into an offset
                ADD       A             ; Multiple by 8
                ADD       A
                ADD       A
                ADD       E             ; Add to start of key table
                LD        E,A
                LD        A,0
                ADC       D
                LD        D,A           ; Points to the correct ROW. Need to process the bits.
                INC       HL
                LD        A,(HL)        ; The keys pressed in this row,
                INC       HL

                ; Process each bit in the accunulator
_nxtbit:        SLA      A
                JR       NC,_nochr

                ; Key pressed. Add to queue.
                EX       DE,HL         ; Keymap into HL
                LD       B,(HL)        ; The keycode (C still contains the meta chars)
                EX       DE,HL
                CALL     DOKEY         ; Add character in B to the input character queue

_nochr:         INC      DE            ; Ready for the next bit
                JR       NZ,_nxtbit    ; Check the next bit in this

                ; Finished, store modified codeRdPtr ready for next character
                LD       A,L
                CP       low (codeBuf + CODE_BUFSIZE * 3)
                JR       NZ,.nowrap
                LD       HL,codeBuf
.nowrap:        LD       (codeRdPtr),HL

                ; If the number of characters in the output buffer is zero check for
                ; more scan codes.
                LD       A,(kbdCnt)
                OR       A
                JR       Z,nxtscan

_empt:          POP       BC
                POP       DE
                POP       HL
                RET

; ------ DOKEY
; Add the character in the B register into the input queue, performing any
; necessary VT100 expansions (TBD)
;
; INPUT: B - Character to store.
DOKEY:          PUSH     AF
                LD       A,$F0         ; Don't store any metadata characters
                AND      B
                CP       $F0
                JR       Z,_nostore

                ; Is there room in the buffer?
                LD       A,(kbdCnt)
                CP       KBD_BUFSIZE
                JR       Z,_nostore

                ; Increase character count
                INC      A
                LD       (kbdCnt),A

                ; Store in the circular buffer
                PUSH     HL
                LD       HL,(kbdWrPtr)
                LD       (HL),B         ; Store the character
                INC      HL

                ; Wrap?
                LD       A,L
                CP       low (kbdBuf + KBD_BUFSIZE)
                JR       NZ,.nowrap
                LD       HL,kbdBuf
.nowrap:        LD       (kbdWrPtr),HL
                ; CALL     NL
                ; LD       A,'"'
                ; RST      08h
                ; LD       A,B
                ; RST      08h
                ; LD       A,'"'
                ; RST      08h
                ; LD       A,' '
                ; RST      08h
                ; LD       A,'['
                ; RST      08h
                ; LD       A,B
                ; CALL     WRITE_8
                ; LD       A,','
                ; RST      08h
                ; LD       A,(kbdMode)
                ; CALL     WRITE_8
                ; LD       A,']'
                ; RST      08h
                ; CALL     NL
                POP      HL
_nostore:       POP      AF
                RET




; Return next character from input queue. Either:
; + There are characters in the short kbdBuf OR
; + Prime the kbdBuf from the codeBuf
; Note that one scan code row can generate multiple keyboard codes
KBDCHAR:        LD       A,(kbdCnt)
                OR       A
                JR       NZ,.gotchr

.again:         CALL     PRIMEBUF

.gotchr:        LD       A,(kbdCnt)
                OR       A
                JR       Z,.again                  ; Nothing so wait (blocking)

                DEC      A                         ; There's at least one character
                LD       (kbdCnt),A

                PUSH     HL
                LD       HL,(kbdRdPtr)
                LD       A,(HL)
                PUSH     AF
                INC      HL

                ; Check for wrap
                LD       A,L
                CP       low (kbdBuf + KBD_BUFSIZE)
                JR       NZ,.nowrap
                LD       HL,kbdBuf
.nowrap:        LD       (kbdRdPtr),HL
                POP      AF                        ; Get the key back
                POP      HL
                RET


; ---- KBDCHK
; Return NZ if there is at least ONE character in the input buffer. Z otherwise.
KBDCHK:         LD       A,(kbdCnt)
                OR       A
                RET      NZ                        ; There are characters
                CALL     PRIMEBUF                  ; Process scan codes
                LD       A,(kbdCnt)
                OR       A
                RET


ifdef FRED
                ; At this point the kbdState array contains the results of the old and new scan rows and
                ; we know that there's at least one key that's changed state.
_prock::        PUSH     DE
                LD       A,(kbdState+stateRow*2)   ; Get the state of the shift/ctrl etc keys
                AND      modeBits                  ; The bits that count
                LD       (kbdMode),A               ; Store for later

                ; If the CAPS key has been pressed then toggle the stored caps status
                LD       E,A                       ; Current state
                LD       A,(kbdState+stateRow*2+1) ; Old state
                XOR      E                         ; Changed bits
                AND      lockMask                  ; The LOCK bit
                JR       Z,nocaps                  ; The caps lock hasn't changed
                BIT      3,E                       ; Caps LOCK pressed?
                JR       Z,nocaps

                ; Caps LOCK has changed state and is ON
                LD       A,(kbdCaps)
                CPL
                LD       (kbdCaps),A

                ; Decide which translation table to use
nocaps:         LD       A,E
                LD       HL,tab_std

                RRA
                JR       NC,no_shift
                LD       HL,tab_shft
                JR       decode

no_shift:       RRA
                JR       NC,no_ctrl
                LD       HL,tab_ctrl
                JR       decode

no_ctrl:        LD       A,(kbdCaps)
                OR       A
                JR       Z,decode
                LD       HL,tab_caps

decode:         LD       (kbdMap),HL   ; Store the selected keyboad map

                ; PUSH     HL
                ; PUSH     AF
                ; CALL     WRITE_16
                ; CALL     NL
                ; RST      08h
                ; POP      AF
                ; POP      HL

                ; Step through each of the rows deciding which keys have been pressed (XOR old and new then AND with new)
                EX       DE,HL         ; DE is the keyboard map
                LD       B,9           ; Number of rows
                LD       HL,kbdState   ; Step back through the table
_nxtRowChck:    PUSH     BC
                PUSH     DE            ; Position in map at start of row processing
                LD       A,(HL)        ; New state
                LD       C,A           ; Saved
                INC      HL
                XOR      (HL)          ; With old state 1 for all bits that have changed
                AND      C             ; Only interested in keys that are now pressed
                LD       (HL),C        ; Store new state as old state
                ; PUSH     AF
                ; CALL     WRITE_8
                ; LD       A,'-'
                ; RST      08H
                ; POP      AF
                JR       Z,_noch

                ; At least one key pressed. Each '1' in A represents a key now pressed.

                ; Walk through the bits to work out which key has been pressed.
                ; LD       A,C           ; New scan value
_nxtbit:        SLA      A
                JR       NC,_nochr

                ; Key pressed. Add to queue.
                EX       DE,HL
                LD       B,(HL)
                EX       DE,HL
                CALL     RXCHR         ; Add character in B to the input character queue

_nochr:         INC      DE            ; Ready for the next bit
                JR       NZ,_nxtbit    ; Check the next bit in this

                ; All set bits in that row completed.
_noch:          INC      HL            ; Step to next scan row result.
                POP      DE
                LD       A,8
                ADD      E
                LD       E,A
                LD       A,0
                ADC      D
                LD       D,A           ; Move char pointer to next row.

                POP      BC
                DJNZ     _nxtRowChck
                POP      DE
                POP      HL
                POP      BC               ; All done, no keys pressed. Return as quickly as possible
                POP      AF
                RET

; ------ RXCHR
; Add the character in the B register into the input queue along with the
; keyboard status.
RXCHR:          PUSH     AF
                LD       A,$F0         ; Don't store any metadata characters
                AND      B
                CP       $F0
                JR       Z,_nostore

                PUSH     HL
                LD       HL,(kbdWrPtr)
                LD       (HL),B
                INC      HL

                ; CALL     NL
                ; LD       A,'"'
                ; RST      08h
                LD       A,B
                RST      08h
                ; LD       A,'"'
                ; RST      08h
                ; LD       A,' '
                ; RST      08h
                ; LD       A,'['
                ; RST      08h
                ; LD       A,B
                ; CALL     WRITE_8
                ; LD       A,','
                ; RST      08h
                ; LD       A,(kbdMode)
                ; CALL     WRITE_8
                ; LD       A,']'
                ; RST      08h
                ; CALL     NL

                LD       A,(kbdMode)
                LD       (HL),A
                INC      HL
                LD       (kbdWrPtr),HL
                POP      HL
_nostore:       POP      AF
                RET

endif



; Control characters
BS             EQU        $08          ; `DEL`
TAB            EQU        $09
LF             EQU        $0A          ; Linefeed
FFEED          EQU        $12
DEL            EQU        $7F
SPC            EQU        $20

; Pound is a special (and horrible) case - it's in the extended ASCII set with value A3
POUND          EQU        $A3

F1             EQU        $C1
F2             EQU        $C2
F3             EQU        $C3
F4             EQU        $C4
F5             EQU        $C5
F6             EQU        $C6
F7             EQU        $C7
F8             EQU        $C8
F9             EQU        $C9
F10            EQU        $CA

FC1            EQU        $D1
FC2            EQU        $D2
FC3            EQU        $D3
FC4            EQU        $D4
FC5            EQU        $D5
FC6            EQU        $D6
FC7            EQU        $D7
FC8            EQU        $D8
FC9            EQU        $D9
FC10           EQU        $DA

PGUP           EQU        $E0
PGDN           EQU        $E1
HOME           EQU        $E2
END            EQU        $E3
SOL            EQU        $E4
EOL            EQU        $E5
UP             EQU        $E6
DOWN           EQU        $E7
LEFT           EQU        $E8
RIGHT          EQU        $E9
WRDDEL         EQU        $EA
LOCK           EQU        $EB

; Keys with upper 4 bits set should NOT be recorded as key presses. These
; are the mode keys: CTRL, SHIFT etc
MNU            EQU        $F8
FN1            EQU        $F9
FN2            EQU        $FA
OPT            EQU        $FB

CTRL           EQU        $FD
SHIFT          EQU        $FE
CAPS           EQU        $FF



menu2Row       EQU        2         ; The scan row that includes the CTRL, SHIFT, OPT and CAPS keys
stateRow       EQU        6         ; The scan row that includes the CTRL, SHIFT, OPT and CAPS keys
fn2Row         EQU        7         ; The scan row that includes the CTRL, SHIFT, OPT and CAPS keys

modeBits       EQU        00011111b ; The bits to look at for keyboard state
lockBit        EQU        3         ; Bit in control scan line that controls caps lock
lockMask       EQU        00001000b ; Bit in control scan line that controls caps lock
; Default table
tab_std::       DB       '76543210'
                DB       ';][\=-98'
                DB       "ba",MNU,"/.,`'"    ;'
                DB       'jihgfedc'
                DB       'rqponmlk'
                DB       'zyxwvuts'
                DB       F3,F2,F1,FN1,LOCK,OPT,CTRL,SHIFT
                DB       CR,FN2,DEL,PGDN,TAB,ESC,F5,F4
                DB       RIGHT,DOWN,UP,LEFT,BS,HOME,PGUP,SPC


tab_shft::      DB       '&^%$',POUND,'@!)'
                DB       ':}{|+_(*'
                DB       "BA",MNU,'?><~"'
                DB       'JIHGFEDC'
                DB       'RQPONMLK'
                DB       'ZYXWVUTS'
                DB       F8,F7,F6,FN1,LOCK,OPT,CTRL,SHIFT
                DB       CR,FN2,DEL,PGDN,TAB,ESC,F10,F9
                DB       RIGHT,DOWN,UP,LEFT,BS,HOME,PGUP,SPC


tab_ctrl::      DB       '76543210'
                DB       ';][\=-98'
                DB       $02,$01,MNU,"/.,`'"    ;'
                DB       $0A,$09,$08,$07,$06,$05,$04,$03
                DB       $12,$11,$10,$0F,$0E,$0D,$0C,$0B
                DB       $1A,$19,$18,$17,$16,$15,$14,$13
                DB       FC3,FC2,FC1,FN1,LOCK,OPT,CTRL,SHIFT
                DB       CR,FN2,DEL,PGDN,TAB,ESC,FC3,FC4
                DB       EOL,HOME,END,SOL,WRDDEL,HOME,PGUP,SPC

tab_caps::      DB       '76543210'
                DB       ';][\=-98'
                DB       "BA",MNU,"/.,`'"    ;'
                DB       'JIHGFEDC'
                DB       'RQPONMLK'
                DB       'ZYXWVUTS'
                DB       F8,F7,F6,FN1,LOCK,OPT,CTRL,SHIFT
                DB       CR,FN2,DEL,PGDN,TAB,ESC,F10,F9
                DB       RIGHT,DOWN,UP,LEFT,BS,HOME,PGUP,SPC


tabs::          DW       tab_std
                DW       tab_shft
                DW       tab_ctrl
                DW       tab_caps

                DSEG

firstRpt        EQU       30
reRep           EQU       2


codeWrPtr::     DW       codeBuf
codeRdPtr::     DW       codeBuf
codeBuf::       DS       CODE_BUFSIZE*3
rpCount         DB       firstRpt  ; When this gets to zero, clear the scan codes

kbdWrPtr::      DW       kbdBuf
kbdRdPtr::      DW       kbdBuf
kbdBufUsed      DS       1
kbdBase         DB       0    ; Contains the upper 4 bits (LED control)
kbdCaps         DB       0    ; 1 if caps lock is on
kbdMode         DB       0    ; Keyboard mode bits (Shift CTRL etc)
kbdState::      DS      18    ; Keyboard scan data. Each scan line is 16 bits. The first
                              ; byte is the latest state, the second byte is the last state.
                              ; generally we're looking for state changes.
kbdMap          DW       0    ; Don't forget which keyboard map we're using.
kbdCnt          DB       0    ; Number of characters in the kbdBuf
kbdBuf::        DS       KBD_BUFSIZE
;.END
