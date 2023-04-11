;==================================================================================
; libvdu
; Output driver that uses the memory mapped VDU card to emulate a basic
; VT100/ANSI terminal. No intention of implementing the entire VT100
; escape sequences, just the popular subset that's most frequently used.
;
; When required the video memory is mapped into the CPU bank 1 memory slot.
;
; This code includes logic to install a bespoke character set or to
; (re-)install the default character set.
;==================================================================================
; import config.asm
import defs.asm

  extrn  WRITE_8, WRITE_16
  public V_CLS,V_SCRUP,V_SCRDN,V_PRT,V_CTOG,V_CENABLE


; Definitions
PG_REG      EQU   $66        ; Bank 2
VPAGE       EQU   $FF        ; Last page

LNCHRS      EQU   80         ; Number of characters in a row
NUMLNS      EQU   30         ; Number of lines

; Start of video memory (when page FF mapped to top)
CHRSET      EQU   $8000      ; 4K from here
VIDEOM      EQU   $A000      ; Actual video memory to BLT to

; Location of CHR$ 255 used for the cursor
CURCHR      EQU   CHRSET+255*16

; Number of characters on the screen 80x30 characters)
PGCHRS      EQU   LNCHRS * NUMLNS

; Address of last byte of video memory
LASTBYTE    EQU   VIDEOM + PGCHRS

; Address of start of last line in video memory.
LASTLN      EQU   VIDEOM + PGCHRS - LNCHRS


; Start with some primitive operations to be used as building blocks.

MAPMEM   MACRO
         PUSH    AF
         LD      A,VPAGE
         ; OUT     (PG_CTRL + 2),A
         POP     AF
         ENDM

; ---- V_CLS
; Fill the entire 80x25 line display with spaces
V_CLS:    PUSH    HL
          PUSH    DE
          PUSH    BC
          MAPMEM
          LD      HL,VIDEOM
          LD      DE,VIDEOM+1
          LD      BC,PGCHRS
          LD      (HL),' '
          LDIR
          LD      HL,0
          LD      (VLINE),HL
          LD      HL,VIDEOM
          LD      (VPTR),HL
          CALL    CURS_MK
          POP     BC
          POP     DE
          POP     HL
          RET

; ------ V_SCRUP
; Scroll up - will scroll the current scroll region and clear the bottom line.
V_SCRUP:    PUSH    BC
            PUSH    DE
            PUSH    HL
            LD      DE,(SCR_MTOP)
            LD      HL,LNCHRS
            ADD     HL,DE                ; HL: Start of second line, DE top region line
            LD      BC,(SCR_MSZ)         ; Number of characters to LDIR (Should be 80 less than region size)
            LDIR

            ; Clear bottom line to spaces
            LD      HL,(SCR_LASTL)
            LD      D,H
            LD      E,L
            INC     DE
            LD      (HL),' '
            LD      BC,LNCHRS-1
            LDIR

            POP     HL
            POP     DE
            POP     BC
            RET

; ------ V_SCRDN
; Scroll down - will scroll characters down one line and clear the top line.
V_SCRDN:    PUSH    BC
            PUSH    DE
            PUSH    HL
            LD      HL,(SCR_MBOT)
            PUSH    HL
            LD      DE,LNCHRS
            OR      A
            SBC     HL,DE                 ; HL = one line up
            POP     DE                    ; DE = Memory bottom
            LD      BC,(SCR_MSZ)
            LDDR
            ; Clear top line
            LD      HL,(SCR_MTOP)
            LD      D,H
            LD      E,L
            INC     DE
            LD      (HL),' '
            LD      BC,LNCHRS-1
            LDIR
            ; If cursor in region then adjust, if outside region then do not adjust.
            CALL    isInReg
            JR      C,.outRegion
            LD      A,L
            INC     A
            LD      (VLINE),A
            LD      HL,(VPTR)
            LD      DE,LNCHRS
            ADD     HL,DE
            LD      (VPTR),HL
.outRegion: POP     HL
            POP     DE
            POP     BC
            RET

; ------ Insert `n` blank characters at the current cursor position. Cursor doesn't move.
V_CHINS:    LD      A,(VCOL)
            CP      LNCHRS-1          ; End of line?
            JR      Z,.eol

            ; Work out how many characters to insert.
            PUSH    HL
            LD      L,A               ; Save  VCOL
            LD      A,(VARG1)
            OR      A                 ; Zero means 1.
            JR      NZ,.ok
            INC     A
            LD      (VARG1),A
.ok:        ADD     L                 ; Position of last space to insert. If past end then just clear the line.
            CP      LNCHRS-1
            JR      NC,.ertoend        ; Inserting more than there's room for

            PUSH    DE
            PUSH    BC
            LD      HL,(VPTR)           ; Current position, need end of line
            LD      DE,80
            ADD     HL,DE
            LD      A,(VCOL)
            LD      E,A
            LD      D,0
            OR      A
            SBC     HL,DE             ; HL is end of line
            ; DEC     HL
            PUSH    HL
            LD      A,(VARG1)
            LD      E,A
            SBC     HL,DE             ; Move back number of chars to insert
            POP     DE                ; DE end of line, HL (VARG1) characters back from the end
            LD      BC,(VPTR)
.mv:        DEC     HL
            DEC     DE
            LD      A,(HL)
            LD      (DE),A
            LD      A,L
            CP      C
            JR      NZ,.mv
            LD      A,H
            CP      B
            JR      NZ,.mv
            ; Space move, now clear the new characters
.clr:       LD      (HL),' '
            INC     HL
            LD      A,L
            CP      E
            JR      NZ,.clr
            LD      A,H
            CP      D
            JR      NZ,.clr
            POP     BC
            POP     DE
            POP     HL
            CALL    CURS_MK
            JR      CURS_ON

            ; Number of characters to insert is more than room on the line so just clear the line.
.ertoend:   XOR     A
            LD      (VARG1),A
            POP     HL
            JR      L_LNERASE

.eol:       PUSH    HL
            LD      HL,(VPTR)
            LD      (HL),' '
            POP     HL
            JP      CURS_MK

; ------ Delete `n` blank characters at the current cursor position. Cursor doesn't move.
V_CHDEL:    LD      A,(VCOL)
            CP      LNCHRS-1          ; End of line?
            JR      Z,.eol

            ; Work out how many characters to insert.
            PUSH    HL
            LD      L,A               ; Save  VCOL
            LD      A,(VARG1)
            OR      A                 ; Zero means 1.
            JR      NZ,.ok
            INC     A
            LD      (VARG1),A
.ok:        ADD     L                 ; Position of last space to insert. If past end then just clear the line.
            CP      LNCHRS-1
            JR      NC,.ertoend       ; Deleting more than chars on the line so just delete line

            PUSH    DE
            PUSH    BC
            LD      HL,(VPTR)         ; Current position, need end of line
            PUSH    HL
            LD      DE,80
            ADD     HL,DE
            LD      A,(VCOL)
            LD      E,A
            LD      D,0
            OR      A
            SBC     HL,DE             ; HL is end of line
            LD      B,H
            LD      C,L               ; This is where to copy to
            POP     DE                ; Get VPTR back
            LD      A,(VARG1)
            LD      L,A
            LD      H,0
            ADD     HL,DE             ; Second pointer
.mv:        LD      A,(HL)
            LD      (DE),A
            INC     HL
            INC     DE
            LD      A,L
            CP      C
            JR      NZ,.mv
            LD      A,H
            CP      B
            JR      NZ,.mv
            ; Space move, now clear the new characters
.clr:       DEC     HL
            LD      (HL),' '
            LD      A,L
            CP      E
            JR      NZ,.clr
            LD      A,H
            CP      D
            JR      NZ,.clr
            POP     BC
            POP     DE
            POP     HL
            CALL    CURS_MK
            JR      CURS_ON

            ; Number of characters to insert is more than room on the line so just clear the line.
.ertoend:   XOR     A
            LD      (VARG1),A
            POP     HL
            JR      L_LNERASE

.eol:       PUSH    HL
            LD      HL,(VPTR)
            LD      (HL),' '
            POP     HL
            JP      CURS_MK






; ------ V_LNINS - Insert a line at the current cursor position but ONLY if
; the cursor is within the scroll region. If this is the bottom line of the
; scroll region then the line is cleared.
V_LNINS:    CALL    LNINSDEL    ; Check parameters and llok for shortcuts
            RET     Z           ; Didn't need to scroll

            ; Going to have to scroll the number of lines we're inserting.
            ; Work out/ do the block copy. A and VARG1 contain number of rows that need to
            ; be scrolled.
            PUSH    HL
            PUSH    DE
            PUSH    BC
            LD      HL,(SCR_MBOT)

            CALL    NEG            ; Negate value

            EX      DE,HL
            LD      A,(VARG1)      ; Number of lines
            LD      L,A
            LD      H,0

            ; Calculate: (HL * 80) + (-SCR_MBOT)
            CALL    HLby80

            ; Negate again to get the actual address: - ((HL * 80) + (- SCR_MBOT))
            CALL    NEG

            ; HL now points to the end of the last line to move
            LD      DE,(SCR_MBOT)

            ; Copy until HL = VPTR
            LD      BC,(VPTR)

.copynext:  DEC     HL
            DEC     DE
            LD      A,(HL)
            LD      (DE),A
            LD      A,L
            CP      C
            JR      NZ,.copynext
            LD      A,B
            CP      H
            JR      NZ,.copynext

            ; HL now contains VPTR and DE is the start of the line to which we need to clear.
.clrnx2:    LD      (HL),' '
            INC     HL
            LD      A,E
            CP      L
            JR      NZ,.clrnx2
            LD      A,D
            CP      H
            JR      NZ,.clrnx2

            ; Finished!
            POP     BC
            POP     DE
            POP     HL
            CALL    CURS_MK
            CALL    CURS_ON
            RET


; ------ V_LNDEL - Delete VARG1 lines at the current cursor position but ONLY if
; the cursor is within the scroll region. If this is the bottom line of the
; scroll region then the line is cleared.
V_LNDEL:    CALL    LNINSDEL    ; Check parameters and llok for shortcuts
            RET     Z           ; Didn't need to scroll

            ; Need to scroll/clear region.
            ; Work out/ do the block copy. A and VARG1 contain number of rows that need to
            ; be scrolled.
.doscroll:  PUSH    HL
            PUSH    DE
            PUSH    BC
            LD      DE,(VPTR)
            LD      A,(VARG1)      ; Number of lines
            LD      L,A
            LD      H,0

            ; Calculate: (HL * 80) + (-SCR_MBOT)
            CALL    HLby80

            ; HL now points to the start first line after the area to delete.
            LD      DE,(VPTR)

            ; Copy until HL = SCR_MBOT
            LD      BC,(SCR_MBOT)

.copynext:  LD      A,(HL)
            LD      (DE),A
            INC     HL
            INC     DE
            LD      A,L
            CP      C
            JR      NZ,.copynext
            LD      A,B
            CP      H
            JR      NZ,.copynext

            EX      DE,HL
            ; HL now contains VPTR and DE is the start of the line to which we need to clear.

.clrnx2:    LD      (HL),' '
            INC     HL
            LD      A,E
            CP      L
            JR      NZ,.clrnx2
            LD      A,D
            CP      H
            JR      NZ,.clrnx2

            ; Finished!
            POP     BC
            POP     DE
            POP     HL
            CALL    CURS_MK
            CALL    CURS_ON
            RET

; Do all the setup for line inset/delete. Return A=0 (Z set) to indicate nothing else needs to be done
; Return A!=0 to indicate that the region needs to be moved (scrolled).
LNINSDEL:   PUSH    HL
            CALL    isInReg          ; This leaves L=VLINE as a shortcut
            JR      C,.noop          ; This does nothing outside of a scroll region. Nothing to do.

            ; Make sure VARG1 is *AT LEAST* 1 and not >127
            LD      A,(VARG1)
            OR      A
            JR      NZ,.vok
            INC     A
.vok:       AND     7fh
            LD      (VARG1),A

            ; In region so all operations from here move to the start of the current line
            CALL    V_CR

            ; Consider different cases:
            ; 1. on the list line: clear the line
            ; 2. number of lines >= available lines: clear to end of region
            ; 3. Anything else, work out the block to delete.

            ; L still contains VLINE from isInReg.
            LD      A,(SCR_BOT)
            CP      L
            JR      NZ,.notlast

            ; Cursor is on the last line of the current scroll region so just clear the line.
            XOR     A
            LD      (VARG1),A      ; Clear line
            CALL    L_LNERASE
            XOR     A              ; Nothing for the caller to do.
            POP     HL
            RET

            ; Not on the last line. Work out MIN(VARG1, lines below curs in region)
            ; L contains VLINE, A contains SCR_BOT
.notlast:   SUB     L              ; Number of lines below cursor in region.
            LD      L,A            ; Save
            LD      A,(VARG1)
            CP      L              ; If +ve then asked for more lines than there are
            JR      C,.doscroll

            ; Number of lines to scroll is greater than the number of lines
            ; availabe so just clear to end of region.
            LD      HL,(VPTR)
            LD      A,' '
            PUSH    DE
            LD      DE,(SCR_MBOT)
.clrnext:   LD      (HL),' '
            INC     HL
            LD      A,L
            CP      E
            JR      NZ,.clrnext
            LD      A,H
            CP      D
            JR      NZ,.clrnext
            POP     DE
            POP     HL
            XOR     A
            CALL    CURS_MK
            JR      CURS_ON

            ; None of the shortcuts worked so get the caller to scroll for us.
.doscroll:  XOR     A        ; Return...
            INC     A        ; '1' to indicate scroll required.
            POP     HL
            RET

; There was nothing to do and no changes have been made (generally out of region)
.noop:      XOR     A
            POP     HL
            RET







; Return carry CLEAR if the current (VLINE) is within the scroll region.
; C:     Set if inside scroll region
; L:     Contains VLINE
isInReg:    LD      A,(VLINE)
            LD      L,A
            LD      A,(SCR_TOP)
            CP      L
            RET     Z              ; If matches top line then definitely in window, no more checks and carry is clear
            JR      NC,.outside    ; Definitely out of window. VLINE < SCR_TOP
            LD      A,(SCR_BOT)
            CP      L
            RET                    ; C clear if inside window
.outside:   SCF
            RET



; ------ V_PRT
; Write the character in A to the current cursor position and move
; forward one character, scrolling if necessary.
V_PRT:      MAPMEM
            LD      HL,(VPTR)
            LD      (VOLD),HL
            LD      HL,(VPTR+2)
            LD      (VOLD+2),HL
            PUSH    AF
            LD      A,(VSTATE)
            OR      A
            JR      NZ,SPECIAL
_normal:    POP     AF

            ; Check for special characters.
_normal2:   CALL    CURS_OFF
            CP      LF
            JR      Z,V_DOWN
            CP      CR
            JR      Z,V_CR
            CP      TAB
            JR      Z,V_TAB
            CP      FF
            JR      Z,V_CLS
            CP      ESC
            JR      Z,V_ESC
            CP      CSI          ; The 8 bit equivalent to ESC [
            JR      Z,V_CSI
            PUSH    HL

            CALL    Q_INS        ; Check if we're in insert move
            LD      HL,(VPTR)
            LD      (HL),A
            CALL    V_RIGHT    ; Move on
            POP     HL
            RET

; ------ V_ESC
; Start of an escape sequence. Change state.
V_ESC:     LD       A,S_ESC
           LD       (VSTATE),A
           RET

; ------ SPECIAL
; State machine processing of a character sequence. Nothing very clever at the
; moment. The state is stored in VSTATE. Values areL
; 0: Normal - just echo the character. This isn't handled in this routine.
; 1: ESC pressed - start of sequence seen.
; Called with A: the current state.
SPECIAL:   CP      S_ESC
           JR      Z,P_ESC
           CP      S_CSI
           JR      Z,P_CSI
           CP      S_CSIQ
           JR      Z,P_CSIQ

           ; No known state so return to normal mode and
           ; print whatever the latest character was
           POP     AF
_CLEAR:    LD      A,S_NORMAL
           LD      (VSTATE),A
           XOR     A
           LD      (VARG1),A
           LD      (VARG2),A
           JR      CURS_MK


; Escape character pressed. Decide what to do. Some are a continuation of other
; sequences.
P_ESC:     POP     AF
           CP      '['
           JR      Z,V_CSI

           ; Any sequence below WILL clear the state machine
           PUSH    AF
           LD      A,S_NORMAL
           LD      (VSTATE),A
           XOR     A
           LD      (VARG1),A
           POP     AF

           CP      ESC
           JR      Z,_CLEAR
           CP      '7'
           JR      Z,L_SAVEC

           ; The following change cursor position so switch off.
           CALL    CURS_OFF

           CP      '8'
           JR      Z,L_RESTC

           CP      'M'
           JR      Z,L_SUP         ; Similar to L_UP but will scroll if at top of scroll region.
           CP      'D'
           JR      Z,L_DOWN

           ; Unknown sequence
           JR      _CLEAR

; ------ V_CSI
; 0x9B pressed or '[' after ESC
V_CSI:     LD      A,S_CSI
           LD      (VSTATE),A
           XOR     A
           LD      (VARG1),A
           LD      (VARG2),A
           LD      HL,VARG1
           LD      (ARGPTR),HL
           RET


; ------ _DIGIT
; Check whether the character in A is a digit and if it is then adjust
; the current input argument. If it's a ';' then move to the second argument.
; Return Z:  Processed, no further action
;       !Z:  Unprocessed character (A unchanged)
_DIGIT:    CP      '9'+1
           JR      NC,.notdec
           SUB     '0'
           JR      C,.notdec

           ; Decimal digit. Add into argument
           PUSH    HL
           PUSH    DE
           LD      HL,(ARGPTR)
           LD      D,A
           LD      A,(HL)
           ADD     A
           LD      E,A
           ADD     A
           ADD     A
           ADD     E            ; A multiplied by 10
           ADD     D            ; Add in new units
           LD      (HL),A
           POP     DE
           POP     HL
           XOR     A            ; Processed so return zero (with Z set)
           RET

.notdec:   CP      ';'
           RET     NZ           ; Only other option supported

           PUSH    HL
           LD      HL,VARG2
           LD      (ARGPTR),HL
           POP     HL
           XOR     A            ; processed.
           RET



; ESC [ seen.
P_CSI:     POP     AF
           CALL    _DIGIT
           RET     Z           ; Nothing more to do.

           CALL    CURS_OFF

           ; All other options reset the state
           PUSH    AF
           LD      A,S_NORMAL
           LD      (VSTATE),A
           POP     AF

           CP      'r'
           JR      Z,L_REGION
           CP      'A'
           JR      Z,L_UP
           CP      'B'
           JR      Z,L_DOWN
           CP      'C'
           JR      Z,L_RIGHT
           CP      'D'
           JR      Z,L_LEFT
           CP      'H'
           JR      Z,L_MOVE
           CP      'J'
           JR      Z,L_ERASE
           CP      'K'
           JR      Z,L_LNERASE
           CP      'S'
           JR      Z,L_NSCUP
           CP      'T'
           JR      Z,L_NSCDN
           CP      'L'
           JR      Z,V_LNINS
           CP      'M'
           JR      Z,V_LNDEL
           CP      '@'
           JR      Z,V_CHINS
           CP      'P'
           JR      Z,V_CHDEL
           CP      'h'
           JR      Z,L_INS      ; Insert mode
           CP      'l'
           JR      Z,L_OVER     ; Overwrite mode
           CP      '?'
           JR      NZ,_CLEAR    ; Unknown - reset state.

           ; Enter CSIQ state
           LD      A,S_CSIQ
           LD      (VSTATE),A

           RET


; ESC [ ? sequence seen
P_CSIQ:    POP     AF
           CALL    _DIGIT
           RET     Z           ; Nothing more to do.

           CALL    CURS_OFF

           ; All other options reset the state
           PUSH    AF
           LD      A,S_NORMAL
           LD      (VSTATE),A
           POP     AF

           CP      'h'
           JR      Z,L_CURSEN
           CP      'l'
           JR      Z,L_CURSDIS
           JR      _CLEAR


; ------ V_TAB
; Move cursor forward until the next 8th char. Inefficient at
; the moment!
V_TAB:      PUSH    HL
            PUSH    DE
            LD      A,(VCOL)
            LD      HL,(VPTR)
            ; Calculate target
            LD      E,A
            ADD     8           ; Simple 8 character tabs
            AND     $F8         ; To get to the tab position. A is the target.
            LD      (VCOL),A    ; Save position
            SUB     E           ; Number of characters to move forward in A
            LD      E,A
            LD      D,0
            ADD     HL,DE       ; New VPTR position
            LD      (VPTR),HL
            POP     DE
            POP     HL
            JR      V_ADJ

; ------ V_CR
; Move to the start of the current line.
V_CR:       PUSH    HL
            PUSH    DE
            LD      A,(VCOL)
            LD      E,A
            XOR     A
            LD      D,A
            LD      HL,(VPTR)
            SBC     HL,DE
            LD      (VPTR),HL
            LD      (VCOL),A
            POP     DE
            POP     HL
            RET                 ; No need to adjust here

; ------ V_DOWN
; Move down one line
V_DOWN:     LD      A,(VLINE)
            INC     A
            LD      (VLINE),A
            PUSH    HL
            PUSH    DE
            LD      HL,(VPTR)
            LD      DE,LNCHRS
            ADD     HL,DE
            LD      (VPTR),HL
            POP     DE
            POP     HL
            JR      V_ADJ


; ------ LOOP
; Call the routine in HL the number of times specified
; by VARG1. If VARG1 is zero then call the routine once.
LOOP:       LD      A,VCEN
            PUSH    AF
            XOR     A
            CALL    V_CENABLE      ; Disable the cursor
            LD      A,(VARG1)
            OR      A
            JR      NZ,.next
            INC     A
.next:      LD      (VARG1),A
            CALL    CALLHL
            LD      A,(VARG1)
            DEC     A
            JR      NZ,.next
            POP     AF
            CALL    V_CENABLE       ; Restore cursor state
            RET

CALLHL:     JP      (HL)

; ------ Define the scroll region (top,bottom]
; First parameter is the top, second in the bottom.
L_REGION:   LD      A,(VARG1)       ; Is the top line in range?
            CP      NUMLNS-1
            JR      C,.cap1         ; Bad value
            LD      A,NUMLNS-1
            LD      (VARG1),A
.cap1:      LD      A,(VARG2)
            CP      NUMLNS
            JR      C,.cap2         ; Bad value
            LD      A,NUMLNS-1
            LD      (VARG2),A
            ; Bottom must be at least one more than top
.cap2:      PUSH    BC
            LD      B,A             ; Save VARG2 - bottom
            LD      A,(VARG1)
            LD      C,A             ; Save top line
            SUB     L               ; Number of lines in the region. Must be greater than 1
            JR      NC,.badReg      ; Invalid - region must be at least 2 lines.

            ; Calculate parameters for this region
            ; C: VARG1 - top
            ; B: VARG2 - bottom
            PUSH    DE
            PUSH    HL

            ; Multiply top line by 80 to get to start of regions.
            LD      L,C
            LD      H,0

            ; Work out memory address for start of first line in scroll region.
            LD      DE,VIDEOM
            CALL    HLby80                     ; HL points to start of first line in scroll region
            LD      (SCR_MTOP),HL              ; And save for future use
            PUSH    HL                         ; and locally to calculate the number of characters

            ; Do the same for the bottom line address
            LD      L,B
            LD      H,0
            LD      DE,VIDEOM
            CALL    HLby80                     ; HL points to start of last line in scroll region
            LD      DE,79
            ADD     HL,DE                      ; Last character at end of scroll region.
            LD      (SCR_MBOT),HL
            LD      DE,79
            OR      A
            SBC     HL,DE                      ; Start of last line
            LD      (SCR_LASTL),HL             ; Stored
            POP     DE
            OR      A
            SBC     HL,DE                      ; Number of bytes in scroll region
            LD      (SCR_MSZ),HL               ; And save

            ; And store the region in memory (use for optimsation?)
            LD      (SCR_TOP),BC
            POP     HL
            POP     DE
.badReg:    POP     BC
            JR      _CLEAR

; ------ Save the current cursor position
L_SAVEC:    PUSH    HL
            LD      HL,(VPTR)
            LD      (VSAVED),HL
            LD      HL,(VPTR+2)
            LD      (VSAVED+2),HL
            POP     HL
            JR      _CLEAR

L_RESTC:    PUSH    HL
            LD      HL,(VSAVED)
            LD      (VPTR),HL
            LD      HL,(VSAVED+2)
            LD      (VPTR+2),HL
            POP     HL
            JR      _CLEAR



; ------ Move cursor position to the [arg1, arg2] values
L_MOVE:     PUSH    HL
            LD      HL,(VARG1)            ; Arg1 and arg2 in HL
            LD      (VLINE),HL            ; Store as new position
            CALL    CALCVPT               ; Re-calculate VPTR
            POP     HL
            JR      V_ADJ                 ; Adjust for out of range


L_LEFT:     LD      HL,V_LEFT
            JR      LOOP

L_RIGHT:    LD      HL,V_RIGHT
            JR      LOOP

L_UP:       LD      HL,V_UP
            JR      LOOP

L_DOWN:     LD      HL,V_DOWN
            JR      LOOP

L_NSCUP:    LD      HL,V_SCRUP
            JR      LOOP

L_NSCDN:    LD      HL,V_SCRDN
            JR      LOOP

; ------ Enable or disable the cursor. The arg MUST be 25.
L_CURSEN:  LD      A,(VARG1)
           CP      25
           RET     NZ
           JR      V_CENABLE

L_CURSDIS: LD      A,(VARG1)
           CP      25
           RET     NZ
           XOR     A
           JR      V_CENABLE

; ------ Switch between overwrite and insert mode.
L_INS:     LD      A,(VARG1)
           CP      4
           RET     NZ
           LD      A,1
           LD      (VINS),A
           DEC     A
           LD      (VCURS),A
           LD      A,0x03
           LD      (VCMASK),A
           JR      CURS_MK

L_OVER:    LD      A,(VARG1)
           CP      4
           RET     NZ
           XOR     A
           LD      (VINS),A
           LD      (VCURS),A
           LD      A,0xFF
           LD      (VCMASK),A
           JR      CURS_MK





; ------ No loop count for this one. If at the top of the scroll area then
; scroll up. Otherwise move the cursor up one line.
L_SUP:      PUSH    HL
            LD      A,(VLINE)             ; Where we are now
            LD      L,A
            LD      A,(SCR_TOP)
            CP      L                     ; If same then scroll down
            CALL    Z,V_SCRDN
            POP     HL
            JR      V_UP

; ------ L_LNERASE
; Parameter dictates what to erase:
; No value/0   - Erase from cursor to end of line
; 1            - Erase from current position to start of line
; 2            - Erase entire line
L_LNERASE:  LD      A,(VARG1)
            OR      A
            JR      Z,.eraseEnd
            DEC     A
            JR      Z,.eraseStrt
            DEC     A
            JR      Z,.eraseLine
            JR      _CLEAR              ; Unknown

.eraseEnd:  PUSH    HL
            LD      A,(VCOL)
            LD      HL,(VPTR)
.next:      LD      (HL),' '
            INC     HL
            INC     A
            CP      80
            JR      NZ,.next
.fin:       POP     HL
            JR      _CLEAR

.eraseStrt: PUSH    HL
            LD      A,(VCOL)
            LD      HL,(VPTR)
.next2:     LD      (HL),' '
            DEC     HL
            OR      A
            JR      Z,.fin
            DEC     A
            JR      .next2
            JR      .fin


.eraseLine: PUSH    HL
            PUSH    DE
            LD      A,(VCOL)
            LD      E,A
            LD      D,0
            OR      A
            LD      HL,(VPTR)
            SBC     HL,DE           ; HL points to start of line
            LD      A,80
.next3:     LD      (HL),' '
            INC     HL
            DEC     A
            JR      NZ,.next3
            POP     DE
            POP     HL
            JR      _CLEAR



; ------ L_ERASE
; Parameter dictates what to erase:
; No value/0   - Erase to end of page from current cursor
; 1            - Erase from current position to top of page
; 2            - Erase entire screen, cursor home
L_ERASE:    LD      A,(VARG1)
            OR      A
            JR      Z,.eraseDown
            DEC     A
            JR      Z,.eraseUp
            DEC     A
            JR      Z,V_CLS
            JR      _CLEAR

            ; Clear from cursor to end of page
.eraseDown: PUSH    HL
            PUSH    DE
            PUSH    BC
            LD      HL,VIDEOM+PGCHRS
            LD      DE,(VPTR)
            OR      A
            SBC     HL,DE                 ; Number of characters to clear
            LD      A,L
            OR      H
            JR      Z,.done               ; Cursor at end of page
            LD      C,L
            LD      B,H
            LD      HL,(VPTR)
            LD      E,L
            LD      D,H
            INC     DE
            LD      (HL),' '
            DEC     BC
            LD      A,B
            OR      C
            JR      Z,.done                ; There was only one character
            LDIR
            JR      .done

            ; Erose from start of screen to cursor
.eraseUp    PUSH    HL
            PUSH    DE
            PUSH    BC
            LD      HL,(VPTR)
            LD      DE,VIDEOM
            OR      A
            SBC     HL,DE                 ; HL = number of characters to clear
            LD      A,L
            OR      H                     ; Zero characters?
            JR      Z,.done
            LD      C,L
            LD      B,H                   ; Count
            LD      HL,VIDEOM
            LD      (HL),' '
            DEC     BC                    ; Was there only one character?
            LD      A,C
            OR      B
            JR      Z,.done
            LD      D,H
            LD      E,L
            INC     DE
            LDIR                          ; Clear
.done:      POP     BC
            POP     DE
            POP     HL
            JR      _CLEAR



; ------ V_LEFT
; Move one cursor position to the left.
V_LEFT:     PUSH    HL
            LD      HL,(VPTR)
            DEC     HL
            LD      (VPTR),HL
            POP     HL
            LD      A,(VCOL)
            DEC     A
            LD      (VCOL),A
            JR      V_ADJ

V_UP:       CALL    _up
            JR      V_ADJ

_up:        LD      A,(VLINE)
            DEC     A
            LD      (VLINE),A
            PUSH    HL
            PUSH    DE
            LD      HL,(VPTR)
            LD      DE,LNCHRS
            OR      A
            SBC     HL,DE
            LD      (VPTR),HL
            POP     DE
            POP     HL
            RET


V_RIGHT:    PUSH    HL
            LD      HL,(VPTR)
            INC     HL
            LD      (VPTR),HL
            LD      A,(VCOL)
            INC     A
            LD      (VCOL),A
            POP     HL
            JR      V_ADJ        ; Adjust for line wrap, page scroll etc

; ------ CALCVPT
; Reset the VPTR value based on the [line, column] position.
CALCVPT:    PUSH    HL
            PUSH    DE
            LD      A,(VLINE)
            LD      H,0
            LD      L,A
            ; Multiple HL by 80 to get start of line.
            LD      DE,VIDEOM
            CALL    HLby80
            LD      A,(VCOL)
            LD      E,A
            LD      D,0
            ADD     HL,DE        ; Add column offset
            LD      (VPTR),HL
            POP     DE
            POP     HL
            RET

; ------ HLby80
; Multiply HL by 80 and add DE
HLby80:     LD      A,L
            OR      H
            JR      Z,.nomult
            ADD     HL,HL        ; *2
            ADD     HL,HL        ; *4
            ADD     HL,HL        ; *8
            ADD     HL,HL        ; *16
            PUSH    DE
            LD      E,L
            LD      D,H
            ADD     HL,HL        ; *32
            ADD     HL,HL        ; *64
            ADD     HL,DE        ; 64+16 = 80
            POP     DE
.nomult:    ADD     HL,DE        ; Add to start of video memory
            RET

; ------ V_ADJ
; Check the current cursor position to make sure it's in the visible window
; after a cursor move. If not then make suitable adjustments to make it
; visible.
V_ADJ:      PUSH    HL

            ; Reset keyboard state after any adjustment.
            LD      A,S_NORMAL
            LD      (VSTATE),A

            LD      A,(VCOL)

            ; If the column number is negative then we've gone past the start of the line
            BIT     7,A
            JR      Z,.nounder

            ; Gone off the start of the row. Decrement the line count
            ; and add 80 to the column number. VPTR here will be OK.
            ADD     LNCHRS
            LD      (VCOL),A
            LD      A,(VLINE)
            DEC     A
            JR      .save

.nounder:   SUB     LNCHRS
            ; If above 80 then wrapped off the end of the line.
            JR      C,.checkline

            ; Off the end of the line. Move down
            LD      (VCOL),A
            LD      A,(VLINE)
            INC     A
.save:      LD      (VLINE),A

            ; Adjust line number if out of screen and scroll as necessary IF WITHIN THE SCROLL WINDOW.
.checkline: LD      A,(VLINE)
            BIT     7,A
            JR      Z,.nolunder

            ; Gone off the top of the screen. Adjust line offset down.
            INC     A
            LD      (VLINE),A
            LD      A,LNCHRS           ; Increase cursor pointer by one line's worth of chars
            LD      HL,(VPTR)
            ADD     L
            LD      L,A
            LD      A,0
            ADC     H
            LD      H,A
            LD      (VPTR),HL
            JR      .checkline         ; Still off the top of the screen?

            ; Check for dropping off the bottom of the scroll region.
.nolunder:  LD      L,A                ; Save VLINE
            LD      A,(SCR_BOT)
            INC     A
            CP      L
            JR      NZ,.noscroll       ; Can't scroll

            ; One line below the scroll region. Check whether the OLD value was within the
            ; scroll region.
            LD      A,(VOLD+2)
            CALL    _INREGION
            JR      NZ,.noscroll       ; NZ means outside scrollable region.

            ; Dropped off the bottom of the scroll window. If the bottom of the screen contains the
            ; scroll
            POP     HL
            CALL    V_SCRUP
            JR      V_UP
            ; JR      V_ADJ

            ; Not going to scroll but make sure that we stay withing visible window.
.noscroll:  LD      A,L
            CP      NUMLNS
            JR      C,.alldone

            ; Off the bottom of the screen but can't scroll.
            DEC     A
            LD      (VLINE),A
            LD      HL,(VPTR)
            LD      A,L
            SUB     LNCHRS
            LD      L,A
            JR      NC,.nocarry
            DEC     H
.nocarry:   LD      (VPTR),HL
            POP     HL
            JR      V_ADJ

.alldone:   CALL    CURS_MK
            CALL    CURS_ON
            POP     HL
            RET

; ------ _INREGION
; Check whether the value in A is within the scroll region.
; Return Z if A is within the scroll region.
; All registers saved.
_INREGION:  PUSH    HL
            LD      HL,(SCR_TOP)       ; L = TOP, H = BOTTOM

            ; PUSH    AF
            ; CALL    WRITE_8
            ; LD      A,':'
            ; RST     08h
            ; LD      A,L
            ; CALL    WRITE_8
            ; LD      A,'-'
            ; RST     08h
            ; POP     AF


            CP      L
            JR      C,.outside         ; Above the top line
            INC     H

            ; PUSH    AF
            ; CALL    WRITE_8
            ; LD      A,':'
            ; RST     08h
            ; LD      A,H
            ; CALL    WRITE_8
            ; POP     AF

            CP      H
            JR      NC,.outside         ; Above the top line

            ; Inside the region. Return Z cleared
            XOR     A
            POP     HL
            RET

.outside:   XOR     A
            INC     A
            POP     HL
            RET



; ------ CURS_MK
; Make a cursor character by inverting the bitmap of the current cursor location.
CURS_MK:    PUSH    AF
            PUSH    HL

            LD      HL,(VPTR)
            LD      A,(VCURS)          ; Current stored cursor
            CP      (HL)
            JR      Z,.nochange        ; Nothing to do

            ; Need to make a new bitmap cursor
            LD      A,(HL)
            LD      (VCURS),A

            LD      L,A
            LD      H,0
            ; Multiple by 16
            ADD     HL,HL
            ADD     HL,HL
            ADD     HL,HL
            ADD     HL,HL
            PUSH    DE
            PUSH    BC
            LD      DE,CHRSET
            ADD     HL,DE               ; HL points to the current character definition for the cursor position.
            LD      DE,CURCHR           ; Where to build the new cursor definition
            LD      B,16                ; Number of rows in the character definition
            LD      A,(VCMASK)          ; What to use to create the cursor (which bits to toggle)
            LD      C,A
.nextline:  LD      A,(HL)
            XOR     C
            LD      (DE),A
            INC     HL
            INC     DE
            DJNZ    .nextline
            POP     BC
            POP     DE
.nochange   POP     HL
            POP     AF
            RET

Q_INS:      PUSH    AF
            LD      A,(VINS)
            OR      A
            JR      Z,.overwrite

            ; Make space for the new character on this line by moving characters right.
            LD      A,(VCOL)
            CP      LNCHRS-1
            JR      Z,.overwrite

            ; Need to find end of line
            PUSH    HL
            PUSH    DE
            PUSH    BC
            LD      HL,(VPTR)
            LD      E,A
            LD      A,LNCHRS-1
            SUB     E                     ; Number of characters that need to be shifted
            LD      B,A                   ; The count
            ADD     L
            LD      L,A
            LD      A,0
            ADC     H
            LD      H,A
            LD      D,H
            LD      E,L
            DEC     HL
.next:      LD      A,(HL)
            LD      (DE),A
            DEC     HL
            DEC     DE
            DJNZ    .next
            EX      DE,HL
            LD      (HL),' '
            POP     BC
            POP     DE
            POP     HL
.overwrite: POP     AF
            RET
; ------ CURS_ON
; Write the real character to the cursor position, if the cursor is enabled.
CURS_ON:    PUSH    AF
            LD      A,(VCEN)
            OR      A
            JR      Z,.fin              ; Cursor is off

            PUSH    HL
            LD      HL,(VPTR)
            LD      (HL),$ff
            POP     HL

.fin        POP     AF
            RET

; ------ CURS_OFF
; Write the real character to the cursor position.
CURS_OFF:   PUSH    HL
            PUSH    AF
            LD      HL,(VPTR)
            LD      A,(VCURS)
            LD      (HL),A
            POP     AF
            POP     HL
            RET

; ------ V_CTOG
; Toggle the cursor at the current location.
V_CTOG:     PUSH    AF
            PUSH    HL

            LD      A,(VCEN)
            OR      A
            JR      Z,.fin

            LD      HL,(VPTR)
            LD      A,(HL)
            INC     A
            JR      Z,.cursoff
            CALL    CURS_ON
            JR      .fin
.cursoff:   CALL    CURS_OFF
.fin:       POP     HL
            POP     AF
            RET

; ------- V_CENABLE
; Store the value of 'A' as the current state of the cursor for display
V_CENABLE:  OR       A
            LD       (VCEN),A
            JR       NZ,.enable

            ; Disable cursor. If ON then switch off
            CALL     CURS_OFF
            RET

            ; Enable the cursor at the current location.
.enable:    CALL     CURS_MK
            CALL     CURS_ON
            RET
; ------ NEG
; Twos complement of HL. Does NOT preserve A. Result returned in HL
NEG         LD      A,L
            CPL
            LD      L,A
            LD      A,H
            CPL
            LD      H,A
            INC     HL
            RET


            DSEG
VPTR::      DW      VIDEOM     ; Pointer to cursor location
VLINE       DB      0          ; Line number
VCOL        DB      0          ; Col position

VCURS       DB      0          ; Character that appears under the cursor
VCEN        DB      1          ; NOT Zero, show cursor

VINS        DB      0          ; If '1' then insert mode
VCMASK      DB    $FF          ; What to use to create a cursor

; Save cursor position at start of print operation.
VOLD        DW      VIDEOM,0

; Saved copy of VPTR and [VLINE,VCOL]. Initialised to home.
VSAVED:     DW      VIDEOM,0

ARGPTR      DW      VARG1      ; Pointer to current modifier.
VARG1       DB      1          ; First modifier in an escape sequence
VARG2       DB      1          ; Second modifier in an escape sequence

; Define the scroll region with precalculated values for scrolling.
SCR_TOP::   DB      0
SCR_BOT     DB      NUMLNS-1
SCR_MTOP    DW      VIDEOM          ; First byte in the scroll region
SCR_MBOT    DW      LASTBYTE        ; Last byte in the scroll region
SCR_LASTL   DW      LASTBYTE-LNCHRS ; Start of last line
SCR_MSZ     DW      PGCHRS-LNCHRS+1 ; Number of bytes to DMA to scroll the region

; Sequence decoding is via a simplified state machine. The state is
; stored in VSTATE and is one of the following:
; S_NORMAL:     Normal character processing
; S_ESC:        Escape key pressed
; S_CSI:      Seen ESC [

S_NORMAL    EQU     0
S_ESC       EQU     1
S_CSI       EQU     2
S_CSIQ      EQU     3

VSTATE      DB      0          ; 0: normal, 1: ESC pressed etc
