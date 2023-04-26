; **********************************************
; Main entry point/startup code for ZLoader:
; 1. copies self from flash to RAM and executes
; 2. Enables memory paging
; 3. Initialises ZIOS and relevant subsystems
; 4. Runs the command line
; 5. Dispatches user commands
; **********************************************
; Copyright Peter Wilson 2022
; https://github.com/peterw8102/Z80-Retro
; **********************************************
import defs.asm
import config.asm
import zapi.asm

import pcb_def.asm
import zlib.asm
import zios.asm
import zload.asm

          ; Load commands
          extrn  BOOT

          ; Z80 register management
          extrn  SHOW_RGS

          ; From exec
          extrn  DO_BP,BRK_HDLR

          ; From hw
          extrn  SH_HW

          ; From SDCard
          extrn  SDLDDEF

          ; From CMDTAB
          extrn FNDCMD,SETMODE

          ; Console selection for UI
          extrn RSTCONS,SELCONS

          ; Exported
          public main
          public END_RES

          public START

          public AUTO_RUN

          ; Error messages
          public E_PRTERR,E_NOTF,E_ERROR,E_UNKWN,E_NEMPT,E_NOSD,E_BADPS

          ; Command handlers
          public CLS,DECCHR,EXDBG

          ; Common strings
          public S_YES,S_NO,S_NOTF,S_NOTEMP,S_COLSTR

; OP Code to use in RST vectors
VEC_CODE  EQU   $C3

          ASEG
          ORG    0
          DI
          JR     START
          JR     main       ; Warm start for the loader.

          ORG    $08
          JR     CNS_OUT

          ORG    $0b
          JR     DO_BP      ; Entry point for breakpoints

          ORG    $10
          JR     CNS_IN

          ORG    $13
          JR     BRK_HDLR   ; Entry point for breakpoints

          ORG    $18
          JR     CNS_CHK

          ORG    $3C
if !IS_DEVEL
          DEFB   'ZIOS'
else
          DEFB   'DEVL'
endif

END_RES   EQU     $

CSEG

; ------------------------------ START ------------------------------------
; Entry point. Psuedo code - this needs to be done BEFORE we use any RAM
; including for a stack.
;  1.   TEST for LOW_RAM
;       IF LOW_RAM
;  2.     IF running from PAGE 0
;  3.        COPY BANK 0 to PAGE 1F (last RAM)
;            MAP PAGE 1F to BANK 0
;            STORE 0x00 as RAM CTRL
;         FI
;       ELSE running from FLASH
;         -- FLASH is LOW
;  4.     TEST running from FLASH?
;         IF running from FLASH
;  5.       COPY BANK 0 to PAGE 3F (lastRAM)
;           MAP PAGE 1F to BANK 0
;           STORE 0x20 as RAM CTRL
;         FI
;       FI
;  6.   INITIALISE STACK
;  7.   LISTEN FOR COMMANDS:
;         LOAD
;         RUN [address]
; RAM page 0, 1, 2 and 3 are avaiable for application loading
; Running a programme:
;   Map PAGE 1, 2, 3 to BANK 1, 2, 3
;   WRITE JP CODE TO LAST 10 BYTES of PAGE 3
;   JP to restart code:
START:     ; 0. Make sure we're in paged memory mode
           ; At reset we're definitely running from PAGE 0. Don't know if this is FLASH or RAM though. Set up the
           ; page register then switch to paged mode.

           ; ------ REMOVE THIS SETUP BEFORE BURNING TO FLASH ------



if IS_DEVEL
            ; Two options here. Either run in-situ, which can make some debug
            ; functions (as in debugging ZLoader) easier.
ifdef OVERWRITE
            ; Want to overwrite the live version with this version
            ; Have to do the copy in two sections (for this release)
            ; 1. Move our first page to page 20 (current installed monitor)
            BANK  2,20h
            LD    HL,0         ; Copy our current first page into page 20
            LD    DE,$8000
            LD    BC,$4000
            LDIR
            ; 2. Map this into bank zero to free page 21
            BANK  0,20h        ; Replaces ourselves

            ; 3. Copy our second page (in bank 3) into page 21
            BANK  2,21h
            LD    HL,$4000     ; Copy RAM page 22 to RAM page 21
            LD    DE,$c000
            LD    BC,$4000
            LDIR

            ; 4. And finally map the new contents of page 21 into bank 3, freeing up the page we were using.
            BANK  3,21h        ; Replaces ourselves
endif
else
            ; ----- Running from Flash - copy to RAM -----
            ; Copy the first 32KB of Flash to the first 32K of RAM.
            BANK  0,FSH_PG_0   ; Flash page 0 -> bank 0
            BANK  1,FSH_PG_0+1 ; Flash page 1 -> bank 1
            BANK  2,MN_PG      ; RAM page 0 into bank 2
            BANK  3,MN2_PG     ; RAM page 1 into bank 3
            EN_PAGE

            LD    HL,0         ; Copy Flash page 0 to RAM page 0
            LD    DE,$8000
            LD    BC,$8000
            LDIR
endif

RUN_CLI:    ; We're running in supervisor mode
            BANK  0,MN_PG      ; RAM page 0 into bank 2
            BANK  3,MN2_PG     ; RAM page 1 into bank 3

            ; Give ourselves a stack at the end of our reserved page.
            LD    SP,SP_STK

            CALL  ZIOS_INI
            LD    (HW_SWTCH),A       ; State of the hardware config switches

            ; Decide which console to use
            LD    A,(HW_SWTCH)    ; State of the hardware config switches
            AND   04h             ; Only interested in bit 2.
            JR    Z,.def
            LD    A,1
.def:       CALL  SELCONS

            ; Really simple CLI now. Display
NOSIO:      LD    HL,_INTRO
            CALL  PRINT_LN
            CALL  SH_HW

if !IS_DEVEL
          ; Check the state of the DIL switches and look for autoboot mode
          LD    A,(HW_SWTCH)    ; State of the hardware config switches
          AND   03h             ; Only interested in bits 0 and 1.

          DEC   A               ; If 01 then do Pi boot
          JR    Z,BOOT
          DEC   A               ; If 02 then do SDCard boot
          JR    NZ,main         ; If not 1 or 2 then just run the command line.

          ; SDCard Boot
          LD    A,1
          LD    (AUTO_RUN),A    ; Set auto-run mode
          JR    SDLDDEF         ; Checksum OK so go load default image.
endif
          ; In development mode don't use the DIL switches. This allows us to develop the loader more efficiently.
          JR    main

E_NOSD:   LD    HL,_NOSDADD
          JR    _prterr
E_NEMPT:  LD    HL,S_NOTEMP
          JR    _prterr
E_ERROR:  LD    HL,_ERROR
          JR    _prterr
E_UNKWN:  LD    HL,_UNKWN
          JR    _prterr

E_NOTF:   ; File doesn't exist so error
          LD    HL,S_NOTF
          JR    _prterr

E_BADPS:  LD    HL,_IOERR
E_PRTERR:
_prterr:  CALL  PRINT_LN

main:     CALL  RSTCONS

          LD    HL, _PROMPT
          CALL  PRINT
          CALL  GET_LINE
          CALL  NL

          CALL  WASTESPC
          JR    Z, main

; Process an unbroken chain of characters looking for a command match
EXEC_LN:: CALL  FNDCMD
          JR    NC,E_UNKWN
          JP    (HL)

; ----- CLS
; Clear screen. If in debug mode then redraw registers etc.
CLS:      LD    HL,_CLRSCR
          CALL  PRINT
          XOR   A
          CALL  SETMODE
          RRCA
          JR    C,main       ; Standard mode so nothing more to do
          JR    _fcdb

; ----- EXDBG
; Switch between command and debug modes
EXDBG:    XOR   A
          CALL  SETMODE
          LD    C,A
          RRCA
          LD    A,C
          JR    NC,_std1     ; In debug mode so can always switch to standard

          ; Check whether debugging is allowed. Requires our drivers.
if IS_DEVEL
          LD     A,1         ; For driver load for debug version
else
          LD     A,(NVRAM)
endif
          RRCA
          LD    A,C
          JR    C,_std1
          LD    HL,_NODRV
          JR    _prterr

_std1:    ; Toggle mode
          LD    A,$FF
          CALL  SETMODE

          ; 'A' contains the new mode
          LD    DE,_INTRO
          RRCA
          JR    C,_std
_fcdb:    LD    DE,_DBMD
_std:     EX    DE,HL
          PUSH  AF
          CALL  PRINT_LN
          POP   AF
          JR    NC,SHOW_RGS
          JR    main

DECCHR:     RST      10h
            CP       3
            JR       Z,main
            CALL     WRITE_8
            JR       DECCHR


; --------------------- STRINGS
; _INTRO:   DEFB 13,"Z80 ZIOS 2.0.5",NULL
_INTRO:   DEFB ESC,"[2J",ESC,"[H",ESC,"[J",ESC,"[0;50r"
_TITLE:   DEFB "Z80 ZIOS 2.0.7",NULL
_CLRSCR:  DEFB ESC,"[2J",ESC,"[1;50r",NULL

; Set scroll area for debug
_DBMD:    DEFB ESC,"[2J",ESC,"[1m",ESC,"[1;6HSTACK",ESC,"[m",ESC,"[11;50r",ESC,"[9;1H  >",ESC,"[12,1H",NULL

_PROMPT:  DEFB "> ",0
_UNKWN:   DEFB "unknown",0
_IOERR:   DEFB "Param error",0

_ERROR       DEFB "Error",0
_NODRV       DEFB "No OS", NULL
_PRMBC       DEFB "BP:   @", NULL
_NOSDADD     DEFB "Bad SDcard address", NULL

; Common (shared) strings
S_COLSTR:    DEFB CR,ESC,'[25C',NULL
S_YES:       DEFB "YES", NULL
S_NO:        DEFB "NO", NULL
S_NOTF:      DEFB "Not found", NULL
S_NOTEMP:    DEFB "Not empty", NULL

          DSEG
; ---------------------- VARIABLES
; If you want the monitor code in ROM then add an ORG here to locate variables somewhere in RAM
HW_SWTCH:  DEFS    1              ; Status of the 3 hardware config bits on boot

FIN_CODE:  DEFB    0
AUTO_RUN:  DEFB    0

DUMP_CHRS  EQU   SCRATCH

.END
