;==================================================================================
; libkbd
; Input driver for the Omega keyboard via the PIO card. The driver is exposed
; as a keyboard scan routine which *should* be called frequently to check for
; pressed keys.
;
; Keyboard processing is in two stages:
; 1. The KBDSCAN routiune should be called 'frequently', generally from a timer
;    50 times a second seems to be a good rate. This routine checks for pressed
;    keys and adds the raw scan codes to a queue.
; 2. KBDCHAR is called to rettieve the next key pressed. This routine takes
;    data from the raw scan-code queue and translates to ASCII and, for special
;    keys, into multi-byte ANSI/VT100 sequences.
;
; This split allows the ISR to remain as efficient as possible while deferring
; the somewhat slow translation into the access code.
;
; The KBDCHAR and KBDCHK functions are externally identical to the SIO
; equivalents RXA and CKINCHAR. This allows trivial switching between SIO and
; an internal keyboard.
;
; KBDINIT is the equivalent to INITSIO and must be called before the keyboard
; will correctly operate.
;
;==================================================================================
; import config.asm
import defs.asm

                extrn  BRK_HK,WRITE_8,WRITE_16,NL,ADD8T16

                public KBDINIT,KBDSCAN,KBDCHAR,KBDCHK,KBDSMDE

; Store processed characters. Doesn't need to be any longer than the
; longest VT100 expansion.
KBD_BUFSIZE     EQU     $8      ; Doesn't need to be very long. Just enough
CODE_BUFSIZE    EQU     $80      ; Undecoded characters (row/col codes)
F5_ROW          EQU     7
                CSEG

; ---- KBDINIT
; Initialise the input buffers and pointers.
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
                CALL     CLRSCAN
                POP      HL
                RET



; ------ KBDCHAR
; Return next character from input queue. This is a BLOCKING CALL and will not return
; until a key has been pressed.
;
; OUTPUT: A - the next character for the client to process.
; All registers EXCEPT the accumulator and flags are preserved.
;
; Either:
; + There are characters in the short kbdBuf OR
; + Prime the kbdBuf from the codeBuf (and decode scan codes to ASCII/VT100)
; Note that one scan code row can generate multiple keyboard codes.
KBDCHAR:        LD       A,(kbdCnt)
                OR       A
                JR       NZ,.gotchr

.again:         CALL     PRIMEBUF                  ; Nothing in kbdBuf so prime from the codeBuf

.gotchr:        LD       A,(kbdCnt)                ; If there's still nothing then there are no waiting keys.
                OR       A
                JR       Z,.again                  ; Nothing so wait (blocking)

                DEC      A                         ; There's at least one character
                LD       (kbdCnt),A

                PUSH     HL
                LD       HL,(kbdRdPtr)
                LD       A,(HL)
                PUSH     AF                        ; Save the character to be returned
                INC      HL                        ; Move kbdRdPtr forward but...
                LD       A,L                       ; ...check for wrap at end of buf
                CP       low (kbdBuf + KBD_BUFSIZE)
                JR       NZ,.nowrap                ; kbdBuf is <256 bytes so only need to check low byte of pointer
                LD       HL,kbdBuf
.nowrap:        LD       (kbdRdPtr),HL
                POP      AF                        ; Get the key back
                POP      HL
                RET


; ---- KBDCHK
; Return NZ if there is at least ONE character in the input buffer. Z otherwise. Note that this
; function does NOT return the waiting key.
;
; All registers except AF preserved.
KBDCHK:         LD       A,(kbdCnt)
                OR       A
                RET      NZ                        ; There are characters is the kbdBuf
                CALL     PRIMEBUF                  ; kbdBuf is empty, scheck the scan code queue
                LD       A,(kbdCnt)
                OR       A
                RET

; ---- KBDSCAN
; A low level scan reporting all keys pressed. Assumed to be called from a timer ISR
; and so optimised to return as soon as ppssible on no key pressess.
KBDSCAN:        PUSH     AF
                PUSH     HL
                PUSH     DE
                PUSH     BC

                LD       HL,kbdState

                ; Poll each scan row and return if no keys pressed.
                LD       A,(kbdBase)      ; Base value
                LD       B,9              ; Row count
                LD       C,0              ; Change flag (any keys up or down)

_nxts:          PUSH     AF
                OUT      (KBD_IO),A       ; Select keyboard row
                IN       A,(KBD_IO)       ; Result...
                CPL                       ; Want a '1' for pressed keys
                LD       E,A              ; Store scan row for later
                LD       (HL),E           ; Store the new value
                INC      HL
                LD       A,(HL)           ; Have any bits changed?
                XOR      E
                OR       C                ; If bits have changed then A!=0. 'C' is non-zero
                LD       C,A              ; C will be non-zero at the end if there are any changes
                INC      HL
                INC      HL               ; Ready for next row
                POP      AF
                INC      A
                DJNZ     _nxts            ; Any more rows to check?

                LD       A,C
                OR       A                ; If any keys change state then reset
                JR       Z,.nochng        ; No keys have changed state

                ; At least one key has changed state
                LD       A,(rpCount)
                OR       A
                LD       A,reRep
                JR       Z,.fast
                LD       A,firstRpt
.fast:          LD       (rpCount),A
                JR       .cdown

.nochng:        LD       A,(rpCount)       ; Timeout for keyboard repeat?
                DEC      A
                LD       (rpCount),A
                CALL     Z,CLRSCAN
.cdown:         LD       A,C
                OR       A
                JR       NZ,_saveState

                POP      BC               ; All done, no keys pressed. Return as quickly as possible
                POP      DE
                POP      HL
                POP      AF
                RET

                ; At this point the kbdState array contains the results of the old and new scan rows and
                ; we know that there's at least one key that's changed state. Pull out each changed
                ; combination and add them to the event queue. Also reset the auto-repeat counter if
                ; the only key pressed is a meta key or if there are more than one key pressed.


_saveState::    LD       A,(kbdState+menu2Row*3)
                AND      20h                       ; Ignore everything except MNU
                LD       E,A

                ; Same for the FN2 row
                LD       A,(kbdState+fn2Row*3)
                AND      40h                       ; Ignore everything except FN2
                OR       E
                LD       E,A

                ; And finally the row with all the other control keys in
                LD       A,(kbdState+stateRow*3)
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

                OR       A
                JR       NZ,.nobrk

                ; Check BRK_HK. If non zero then check whether F5 has been pressed.
                LD       HL,(BRK_HK)
                LD       A,L
                OR       H
                JR       Z,.nobrk

                ; Check 'pressed' status for F5
                LD       A,(kbdState+F5_ROW*3)     ; New state
                BIT      1,A                       ; F5
                JR       Z,.nobrk

                ; Break is pressed. Was it pressed on last scan?
                LD       A,(kbdState+F5_ROW*3+1)
                BIT      1,A
                JR       NZ,.nobrk                ; F5 was NOT pressed on last keypress

                ; At this point F5 has been pressed and there's a break handler. Make
                ; the stack the same as it would be from the SIO:
                ;    Ret address from SERINT
                ;    AF at start of ISR
                ;    HL at start of ISR
                ;    AF containing the break character
                POP      BC
                POP      DE
                LD       A,3
                JP       (HL)


                ; Copy changed keys into the processing queue (without decoding) and
                ; count the number of character keys pressed.
.nobrk:         LD       D,0           ; Number of rows with character generating keys pressed, excluding metas
                LD       B,9           ; Number of rows
                LD       HL,kbdState   ; Step back through the table
_pnxtrow:       LD       A,(HL)        ; New state
                LD       C,A           ; Save
                INC      HL
                XOR      A,(HL)        ; Compare with last poll. 1s mean a changed state
                LD       (HL),C        ; Store the changed state
                INC      HL            ; Points to character mask
                AND      C             ; a '1' now means a key has been pressed in this row
                AND      (HL)          ; Ignore meta keys
                JR       Z,.nokey
                ; At least one key pressed. Output this row for processing,
                ; D:   Row number
                ; A:   Changed key
                ; E:   Meta keys
                CALL     saverow
                INC      D             ; Number or rows with pressed character keys
.nokey:         INC      HL            ; Next row
                DJNZ     _pnxtrow

                ; If there were no character generating keys pressed then reset the
                ; auto repeat counter.
                LD       A,D           ; Number of rows with pressed character keys
                OR       A
                JR       NZ,.cont      ; If there are no pressed keys then...
                LD       A,firstRpt    ; ...reset the repeat key counter
                LD       (rpCount),A

                ; All rows processed and queued so OK to return.
.cont           POP      BC
                POP      DE
                POP      HL               ; All done, no keys pressed. Return as quickly as possible
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
                PUSH     AF
                LD       HL,(codeWrPtr)
                LD       (HL),E            ; Save Meta
                INC      HL
                LD       A,9
                SUB      B
                LD       (HL),A            ; Row number
                INC      HL
                POP      AF
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
; Return the next entry from the scan queue.
;
; Returns:
; A: Zero if no scan codes available (reflected in Z flag)
; D: Meta flags in place for this scan code
; E: Row number for this scan code
; C: The scan code (each bit set is a pressed key in that row)
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
                LD        A,C           ; C is the last scan code so can't be zero.
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

decode:         EX        DE,HL         ; DE: Points into code buffer, HL: decode table
                INC       DE
                LD        A,(DE)        ; The row number. Translate into an offset
                ADD       A             ; Multiple by 8
                ADD       A
                ADD       A
                ADD       L             ; Add to start of key table
                LD        L,A
                LD        A,0
                ADC       H
                LD        H,A           ; Points to the correct ROW. Need to process the bits.
                INC       DE
                LD        A,(DE)        ; The keys pressed in this row,
                INC       DE            ; Ready for next row

                ; Process each bit in the accunulator
_nxtbit:        SLA      A
                JR       NC,_nochr

                ; Key pressed. Add to queue.
                LD       B,(HL)        ; The keycode (C still contains the meta chars)
                CALL     DOKEY         ; Add character in B to the input character queue

_nochr:         INC      HL            ; Ready for the next bit
                JR       NZ,_nxtbit    ; Check the next bit in this

                ; Finished, store modified codeRdPtr ready for next character
                LD       A,E
                CP       low (codeBuf + CODE_BUFSIZE * 3)
                JR       NZ,.nowrap
                LD       DE,codeBuf
.nowrap:        LD       (codeRdPtr),DE

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
; necessary VT100 expansions.
;
; INPUT: B - Character to store.
;        C - Meta key state for this key
DOKEY:          PUSH     AF

                LD       A,B
                CP       LOCK
                JR       NZ,.nolock

                ; Caps lock - toggle
                LD       A,(kbdCaps)
                CPL
                LD       (kbdCaps),A
                ; Change caps LED
                OR       A
                LD       A,(kbdBase)
                JR       Z,.capsoff

                ; Caps ON - turn LED on
                AND      not LED_CAPS
                LD       (kbdBase),A
                JR       _nostore

.capsoff:       OR       LED_CAPS
                LD       (kbdBase),A
                JR       _nostore

.nolock:        AND      $F0         ; Don't store any metadata characters
                CP       $F0           ; Meta keys
                JR       Z,_nostore

                ; Is there a translation for this character code? Keys
                ; with the most sig 2 bits set are translated.
                LD       A,$C0
                AND      B
                CP       $C0
                JR       NZ,.noexpand

                ; Expand into VT100 sequence. Index into VTMAP
                PUSH     HL
                LD       A,$3F
                AND      B            ; Just use lower 6 bits (64 chars)
                ADD      A            ; x2
                LD       HL,VTMAP
                ADD      L
                LD       L,A
                LD       A,0
                ADC      H
                LD       H,A          ; HL points to the control sequence
                LD       A,(HL)
                INC      HL
                OR       (HL)
                JR       Z,.noexp2   ; No expansion
                LD       A,(HL)
                DEC      HL
                LD       L,(HL)
                LD       H,A          ; HL now points to the sequence. Replay
.nxtchr:        LD       A,(HL)
                AND      $7F
                CP       '['         ; If it's an '[' then we need an ESC prefix
                JR       NZ,.noesc
                LD       B,ESC
                CALL     DOKEY
.noesc:         LD       B,A
                CALL     DOKEY        ; Recursive put
                LD       A,(HL)
                RLA
                INC      HL
                JR       NC,.nxtchr
                POP      HL
                POP      AF
                RET

.noexp2:        POP      HL

                ; Increase character count
.noexpand:      LD       A,(kbdCnt)     ; Is there room in the buffer?
                CP       KBD_BUFSIZE
                JR       Z,_nostore

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
                POP      HL
_nostore:       POP      AF
                RET



; ---- CLRSCAN
; Clear the previous scan codes. This will effectively enable auto-repeat. Also
; used as part of keyboard initialisation.
CLRSCAN:        PUSH     HL
                PUSH     BC
                PUSH     AF
                LD       HL,kbdState+1
                LD       B,9
                XOR      A
.clrnxt:        LD       (HL),A
                INC      HL
                INC      HL
                INC      HL
                DJNZ     .clrnxt
                POP      AF
                POP      BC
                POP      HL
                RET

; ------ KBDMAP
; Map one of the keys in one of the translation tables to
; emit a different code to standard.
; Parameters:
; L: Table that contains the key to map (0-3)
;    0: Standard keys
;    1: When shift is pressed
;    2: When CTRL is pressed
;    3: When CAPS is locked
; B: Offset into table to be mapped. MAX: 47h
; C: New code to return for this character
KBDMAP::  LD    A,$03
          AND   L             ; Make sure it's in range
          LD    HL,tabset
          JR    Z,.std
          ADD   A,A           ; x2
          CALL  ADD8T16       ; HL points a pointer to the start of the correct txlation tab
.std:     LD    A,(HL)        ; Get the pointer to start of table into HL
          INC   HL
          LD    H,(HL)
          LD    L,A           ; HL points to start of table
          LD    A,B
          CP    NUMKEYS
          RET   NC
          CALL  ADD8T16       ; Add to table base
          LD    (HL),C
          RET

; ------ KBDSMDE
; Switch between two main keyboard operating modes:
; A=0   : VT100 (default). Arrow keys returns VT100 sequences
; A=1   : Wordstar. Arrow keys and some other keys return traditional
;         control keys which a number of CP/M programmes seem to
;         prefer over VT100. In particular, most CCP replacements
;         use these codes.
; Individual keys can be configured using the SETKEY function.
;
; Keyboard modes are changed by overwriting entries in the lookup
; tables. Each patch table is defined in a list of 'patch' codes.
; Each patch code comprises three bytes.
;
; The first selects one of the four key state tables (tab_std,
; tab_shft etc).
;
; The second selects the key offset into that table to be patched.
;
; The last is the key code to be placed in the table.
;
; The end of the table is identified by an 0xFF value in the first byte.
;
; Note that this method does NOT allow any key to return any key
; sequence. The key can be configured to return any byte value. If
; the byte value is >$C0 then that code will be expanded into one
; of the preconfigured escape sequences (VTMAP).
KBDSMDE:: PUSH  HL
          PUSH  DE
          PUSH  BC
          LD    DE,MD_VT100
          DEC   A
          JR    NZ,.overlay
          LD    DE,MD_WSTR
.overlay: LD    A,(DE)           ; Zero means end of table
          INC   DE
          LD    L,A              ; This byte identifes the table, saved in L
          INC   A
          JR    Z,.end

          LD    A,(DE)
          INC   DE
          LD    B,A              ; The character offset into the table

          LD    A,(DE)
          INC   DE               ; Points to start of next entry
          LD    C,A              ; The mapped character
          ; PUSH  DE
          CALL  KBDMAP
          ; POP   DE
          JR    .overlay

.end:     POP   BC
          POP   DE
          POP   HL
          RET


; Control characters
BS             EQU        $08          ; `DEL`
TAB            EQU        $09
LF             EQU        $0A          ; Linefeed
FFEED          EQU        $12
DEL            EQU        $7F
SPC            EQU        $20

; Pound is a special (and horrible) case - it's in the extended ASCII set with value A3
POUND          EQU        $A3


; All values >$C0 are extended codes that will be expanded into
; ANSI/VT100 escape sequences.
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
WSHOME         EQU        $EB

; Keys with upper 4 bits set should NOT be recorded as key presses. These
; are the mode keys: CTRL, SHIFT etc
LOCK           EQU        $F8
MNU            EQU        $F9
FN1            EQU        $FA
FN2            EQU        $FB
OPT            EQU        $FC
CTRL           EQU        $FD
SHIFT          EQU        $FE
CAPS           EQU        $FF

LED_CAPS       EQU        $10

menu2Row       EQU        2         ; The scan row that includes the CTRL, SHIFT, OPT and CAPS keys
stateRow       EQU        6         ; The scan row that includes the CTRL, SHIFT, OPT and CAPS keys
fn2Row         EQU        7         ; The scan row that includes the CTRL, SHIFT, OPT and CAPS keys

; Define well known offsets into the table that we want to redfine.
O_MNU          EQU        $12
O_F1           EQU        $32
O_F2           EQU        $31
O_F3           EQU        $30
O_F4           EQU        $3F
O_F5           EQU        $3E
O_FN1          EQU        $33
O_FN2          EQU        $39
O_BS           EQU        $3A
O_PDWN         EQU        $3B
O_PUP          EQU        $46
O_TAB          EQU        $3C
O_ESC          EQU        $3D
O_RIGHT        EQU        $40
O_DOWN         EQU        $41
O_UP           EQU        $42
O_LEFT         EQU        $43
O_DEL          EQU        $44
O_HOME         EQU        $45
O_SPC          EQU        $47

; Defines for the four state tables
T_STD          EQU        $00
T_SHFT         EQU        $01
T_CTRL         EQU        $02
T_CAPS         EQU        $03

; Number of slots in each of the tab_??? tables.
NUMKEYS        EQU        $48

MD_VT100:      DB    T_STD,   O_UP,        UP
               DB    T_STD,   O_DOWN,      DOWN
               DB    T_STD,   O_LEFT,      LEFT
               DB    T_STD,   O_RIGHT,     RIGHT
               DB    T_STD,   O_HOME,      HOME
               DB    T_STD,   O_PUP,       PGUP
               DB    T_STD,   O_PDWN,      PGDN

               DB    T_CTRL,  O_UP,        HOME
               DB    T_CTRL,  O_DOWN,      END
               DB    T_CTRL,  O_LEFT,      SOL
               DB    T_CTRL,  O_RIGHT,     EOL
               DB    T_CTRL,  O_BS,        WRDDEL



               DB    $ff

MD_WSTR:       DB    T_STD,   O_UP,        $05   ; CTRL-E
               DB    T_STD,   O_DOWN,      $18   ; CTRL-X
               DB    T_STD,   O_LEFT,      $13   ; CTRL-S
               DB    T_STD,   O_RIGHT,     $04   ; CTRL-D
               DB    T_STD,   O_HOME,      WSHOME
               DB    T_STD,   O_PUP,       $12
               DB    T_STD,   O_PDWN,      $03

               DB    T_CTRL,  O_UP,        $12   ; CTRL-R
               DB    T_CTRL,  O_DOWN,      $03   ; CTRL-C
               DB    T_CTRL,  O_LEFT,      $01   ; CTRL-A
               DB    T_CTRL,  O_RIGHT,     $06   ; CTRL-F
               DB    T_CTRL,  O_BS,        $14   ; CTRL-T

               DB    $ff

; Default table
tab_std:        DB       '76543210'
                DB       ';][\=-98'
                DB       "ba",MNU,"/.,`'"    ;'
                DB       'jihgfedc'
                DB       'rqponmlk'
                DB       'zyxwvuts'
                DB       F3,F2,F1,FN1,LOCK,OPT,CTRL,SHIFT
                DB       CR,FN2,BS,PGDN,TAB,ESC,F5,F4
                DB       RIGHT,DOWN,UP,LEFT,DEL,HOME,PGUP,SPC


tab_shft:       DB       '&^%$',POUND,'@!)'
                DB       ':}{|+_(*'
                DB       "BA",MNU,'?><~"'  ;'
                DB       'JIHGFEDC'
                DB       'RQPONMLK'
                DB       'ZYXWVUTS'
                DB       F8,F7,F6,FN1,LOCK,OPT,CTRL,SHIFT
                DB       CR,FN2,BS,PGDN,TAB,ESC,F10,F9
                DB       RIGHT,DOWN,UP,LEFT,DEL,HOME,PGUP,SPC


tab_ctrl:       DB       '76543210'
                DB       ';][\=-98'
                DB       $02,$01,MNU,"/.,`'"    ;'
                DB       $0A,$09,$08,$07,$06,$05,$04,$03
                DB       $12,$11,$10,$0F,$0E,$0D,$0C,$0B
                DB       $1A,$19,$18,$17,$16,$15,$14,$13
                DB       FC3,FC2,FC1,FN1,LOCK,OPT,CTRL,SHIFT
                DB       CR,FN2,BS,PGDN,TAB,CSI,FC3,FC4
                DB       EOL,END,HOME,SOL,WRDDEL,HOME,PGUP,SPC

tab_caps:       DB       '76543210'
                DB       ';][\=-98'
                DB       "BA",MNU,"/.,`'"    ;'
                DB       'JIHGFEDC'
                DB       'RQPONMLK'
                DB       'ZYXWVUTS'
                DB       F8,F7,F6,FN1,LOCK,OPT,CTRL,SHIFT
                DB       CR,FN2,BS,PGDN,TAB,ESC,F10,F9
                DB       RIGHT,DOWN,UP,LEFT,DEL,HOME,PGUP,SPC

tabset:         DW       tab_std
                DW       tab_shft
                DW       tab_ctrl
                DW       tab_caps

; VT100 escape sequence mapping
V_F1            DC       '[10~'
V_F2            DC       '[11~'
V_F3            DC       '[12~'
V_F4            DC       '[13~'
V_F5            DC       '[14~'

V_UP            DC       '[A'
V_DOWN          DC       '[B'
V_RIGHT         DC       '[C'
V_LEFT          DC       '[D'

V_HOME          DC       '[7~'
V_END           DC       '[8~'

V_PGUP          DC       '[5~'
V_PGDN          DC       '[6~'


; WordStar sequences can go here
W_HOME          DB       $11,'E'+$80

; VT100 map starting with character $C0
VTMAP:          DW       0               ; $C0
                DW       V_F1
                DW       V_F2
                DW       V_F3
                DW       V_F4
                DW       0
                DW       0
                DW       0
                DW       0               ; E8
                DW       0
                DW       0
                DW       0
                DW       0
                DW       0
                DW       0
                DW       0
                DW       0               ; D0
                DW       0
                DW       0
                DW       0
                DW       0
                DW       0
                DW       0
                DW       0
                DW       0               ; D8
                DW       0
                DW       0
                DW       0
                DW       0
                DW       0
                DW       0
                DW       0
                DW       V_PGUP          ; E0
                DW       V_PGDN
                DW       V_HOME
                DW       V_END
                DW       0
                DW       0
                DW       V_UP
                DW       V_DOWN
                DW       V_LEFT
                DW       V_RIGHT
                DW       0
                DW       W_HOME
                DW       0
                DW       0
                DW       0
                DW       0                ; EF






                DSEG

firstRpt        EQU       10
reRep           EQU       1


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

; Keyboard scan data. Each scan line is 3 bytes. The first
; byte is the latest state, the second byte is the last state.
; The third byte is fixed and masks out any keys that are not
; character generating (eg a shift or ctrl key)
kbdState:       DB       0,0,11111111b
                DB       0,0,11111111b
                DB       0,0,11011111b
                DB       0,0,11111111b
                DB       0,0,11111111b
                DB       0,0,11111111b
                DB       0,0,11111000b
                DB       0,0,11111111b
                DB       0,0,11111111b


kbdCnt          DB       0    ; Number of characters in the kbdBuf
kbdBuf::        DS       KBD_BUFSIZE
;.END
