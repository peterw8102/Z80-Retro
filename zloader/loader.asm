import defs.asm
import config.asm
import zapi.asm

import pcb_def.asm
import zlib.asm
import zios.asm
import zload.asm

          ; Utilities
          extrn  DISASS, SETOFF

          ; External commands for the command table.
          extrn  FILL,CLRSCR,INPUT,OUTPUT,MODIFY,SDMOD,HELP
          extrn  MORE

          extrn  DNVRAM,DECINST,DMP16,DUMPM,DUMPI,DUMP

          ; Load commands
          extrn  LDF,LDH,LDT,LOADF,BOOT,BOOTIX

          ; Date time commands
          extrn  DTIME,SHDTIME

          ; Command table
          extrn  BDG_TABLE,CMD_TABLE

          ; SDCard definitions
          extrn  BTPREP,SBCALCS
          extrn  SADDR

          ; Z80 register management
          extrn  DO_RGS,SHOW_RGS

          ; From exec
          extrn  DO_BP,BRK_HDLR

          ; From cfg
          extrn  CFG_TAB

          ; Exported
          public main,BADPS,OPMODE

          public SDPAGE
          public PRTERR
          public END_RES

          public START
          public COLSTR
          public AUTO_RUN

          ; Error messages
          public E_NOTF,E_ERROR,E_UNKWN,E_NEMPT,E_NOSD

          ; Command handlers
          public CLS,DECCHR,EXDBG,MAPDSK
          ; public SDDIR,SDLOAD,SDRUN,SDUMP
          public SHWHIST,SMAP,SWRITE

; OP Code to use in RST vectors
VEC_CODE  EQU   $C3


          ASEG
          ORG    0
          DI
          JR     START
          JR     main       ; Warm start for the loader.

          ORG    $08
          JR     TXA

          ORG    $0b
          JR     DO_BP      ; Entry point for breakpoints

          ORG    $10
          JR     RXA

          ORG    $13
          JR     BRK_HDLR   ; Entry point for breakpoints

          ORG    $18
          JR     CKINCHAR

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
            BANK   3,MN2_PG       ; Map second supervisor page into bank 3
            JR    RUN_CLI      ; THIS LINE SHOULD BE IN FOR DEVELOPMENT BUT OUT BEFORE PROGRAMMING TO FLASH
endif
            ; Copy the first 32KB of Flash to the first 32K of RAM.
            BANK  0,FSH_PG_0   ; Flash page 1 -> bank 1
            BANK  1,FSH_PG_0+1 ; Flash page 1 -> bank 1
            BANK  2,MN_PG      ; RAM page 0 into bank 2
            BANK  3,MN2_PG     ; RAM page 1 into bank 3
            EN_PAGE

            LD    HL,0         ; Copy Flash page 0 to RAM page 0
            LD    DE,$8000
            LD    BC,$8000
            LDIR

RUN_CLI:    ; We're running in supervisor mode
            BANK  0,MN_PG      ; RAM page 0 into bank 2
            BANK  3,MN2_PG     ; RAM page 1 into bank 3

            ; Give ourselves a stack at the end of our reserved page.
            LD    SP,SP_STK

            CALL  ZIOS_INI
            LD    (HW_SWTCH),A       ; State of the hardware config switches

            ; Set mode
            LD    A,1
            LD    (OPMODE),A      ; Command mode (don't start in debug)
            LD    HL,CMD_TABLE
            LD    (CMDTAB),HL     ; Search table for commands

            ; Set default DUMP mode to disassemble memory
            ; LD     A,'I'
            ; LD     (DUMP_MODE),A

if CSET
            ; Install character set
            CALL   INITCSET
endif

            ; Really simple CLI now. Display
NOSIO:      LD    HL, _INTRO
            CALL  PRINT_LN
            CALL  SH_HW

if !IS_DEVEL
          ; Check the state of the DIL switches and look for autoboot mode
          LD    A,(HW_SWTCH)       ; State of the hardware config switches
          AND   03h             ; Only interested in bits 0 and 1.

          DEC   A               ; If 01 then do Pi boot
          JR    Z,BOOT
          DEC   A               ; If 02 then do SDCard boot
          JR    NZ,main         ; If not 1 or 2 then just run the command line.

          ; SDCard Boot
          LD    A,1
          LD    (AUTO_RUN),A    ; Set auto-run mode
          CALL  BTPREP
          JR    Z,SDLDDEF       ; Checksum OK so go load default image.
endif
          ; In development mode don't use the DIL switches. This allows us to develop the loader more efficiently.
          JR    main

E_NOSD:   LD    HL,_NOSDADD
          JR    _prterr
E_NEMPT:  LD    HL,_NOTEMP
          JR    _prterr
E_ERROR:  LD    HL,_ERROR
          JR    _prterr
E_UNKWN:  LD    HL,_UNKWN
          JR    _prterr

E_NOTF:   ; File doesn't exist so error
          LD    HL,_NOTF
          JR    _prterr

BADPS:    LD    HL,_IOERR
PRTERR:
_prterr:  CALL  PRINT_LN

main:     LD    SP,SP_STK       ; Dump the stack and restart on *main*
          LD    HL, _PROMPT
          CALL  PRINT
          CALL  GET_LINE
          CALL  NL

          CALL  WASTESPC
          JR    Z, main

; Process an unbroken chain of characters looking for a command match
EXEC_LN::
FNDCMD:   LD    HL,(CMDTAB)
_nxtchr:  CALL  BUFCHR
          JR    Z,_miss
          CALL  TOUPPER        ; Need uppercase only
          LD    C,A            ; Save the input character
          LD    A,(HL)

          LD    B,A            ; Save the test character$
          AND   $7f            ; Ignore MSB

          CP    C              ; Compare with input character
          JR    NZ,_miss

          ; Input character matched test character. End of command if MSB of B is set.
          INC   HL
          RLC   B
          JR    NC,_nxtchr     ; Step to next command

          ; Next two bytes are the target address.
          LD    E,(HL)
          INC   HL
          LD    D,(HL)
          EX    DE,HL
          CALL  WASTESPC       ; Set buffer ready for next token
          JP    (HL)           ; Call with A being the next character

          ; Command mismatch. Step to next command, reset input buffer and try again.
_miss:    LD    A,$80
          AND   (HL)
          INC   HL
          JR    Z,_miss
          ; Step over the two address bytes
          INC   HL
          INC   HL
          CALL  RESINP
          CALL  WASTESPC
          LD    A,(HL)
          OR    A
          JR    Z,E_UNKWN
          JR    _nxtchr          ; Check against the next command...



; ----- SHWHIST
; Display history with index rows
SHWHIST:  LD    B,$11
_nhist:   DEC   B
          CALL  GETHIST
          JR    Z,_gnxt      ; Nothing in that slot
          ; Got a history line, pointer in HL.
          CALL  PRINT_LN
_gnxt:    LD    A,B
          OR    A
          JR    NZ,_nhist
          JR    main


; ----- CLS
; Clear screen. If in debug mode then redraw registers etc.
CLS:      LD    HL,_CLRSCR
          CALL  PRINT
          LD    A,(OPMODE)
          RRCA
          JR    C,main      ; Standard mode so nothing more to do
          JR    _fcdb

; ----- EXDBG
; Switch between command and debug modes
EXDBG:    LD    A,(OPMODE)
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

_std1:    XOR   3
          LD    (OPMODE),A   ; Toggled mode
          LD    HL,CMD_TABLE
          LD    DE,_INTRO
          RRCA
          JR    C,_std
_fcdb:    LD    DE,_DBMD
          LD    HL,BDG_TABLE
_std:     LD    (CMDTAB),HL
          EX    DE,HL
          PUSH  AF
          CALL  PRINT_LN
          POP   AF
          JR    NC,SHOW_RGS
          JR    main




; ------------ _PGCALC
; Take a 16 bit address in application space and translate that into a block number (0-3)
; and a 16 offset. Arrange for the 16 bit offset to point into a block one address. This
; then allows us to map the application space page into block one regardless of where it
; is in the application space.
; HL - load address. Translate into an offset  and return the offset in HL
; Return:
; A  - The block number (0-3) in application space
; HL - The adjusted offset into that page, mapped as though in block 1.
; DE - WORKING SPACE, NOT SAVED!!
_PGCALC:  LD    A,H
          RLCA
          RLCA
          AND   03h        ; A now contains the block number.
          LD    D,A        ; Save to return

          ; And map the address in HL so it's in BLOCK 1.
          LD    A,3Fh
          AND   H

          ; Set bit 6 so it's in block 1
          OR    A,40h

          ; And move back to the address
          LD    H,A
          LD    A,D        ; Return the page we selected in A and D
          RET

; ------- MAPDSK
; Display current mapped drives
MAPDSK:  LD    HL,SCRATCH
         LD    BC,100h | A_QDSKMP    ; B must be zero
         RST   30h            ; Get the current drive map
         LD    BC,1000h
         LD    A,'A'
         LD    HL,SCRATCH     ; Where the data went
_dmapn:  PUSH  AF
         RST   08h
         LD    A,':'
         RST   08h
         LD    A,' '
         RST   08h

         LD    A,(HL)         ; Device
         INC   HL
         PUSH  AF
         LD    E,(HL)
         INC   HL
         LD    D,(HL)
         INC   HL
         EX    DE,HL
         CALL  WRITE_D
         EX    DE,HL
         POP   AF
         CALL  _wrdev         ; Write SDCard name (from A)
         CALL  NL
         POP   AF
         INC   A

         DJNZ  _dmapn

         JR    main


_wrdev:  PUSH   HL
         PUSH   AF
         LD     A,'/'
         RST    08h
         LD     HL,_SDEV
         CALL   PRINT
         POP    AF
         ADD    A,'0'
         RST    08h
         POP    HL
         RET


; ----- SMAP
; Map a logical dic
; SM L PPPP - Map Physical drive (decimal 0-511) to logical drive letter L
SMAP:     JR    Z,MAPDSK
          CALL  BUFCHUP
          CP    'A'
          JR    C,_baddrv
          CP    'A'+16
          JR    C,_gotdrv

          ; Report bad drive letter
_baddrv:  LD    HL,_NODRIVE
          JR     _prterr

          ; Want a physical drive (0-1023 decimal)
_gotdrv:  PUSH   AF          ; Push DRIVE letter
          CALL   GET_DEC
          JR     Z,E_ERROR

          ; Number from 1-1023 (Don't allow 0 as a target)
          LD     A,$FC
          AND    H
          JR     NZ,E_NOSD
          LD     A,H
          OR     L
          JR     Z,E_NOSD

          ; Optional SDCard
          CALL   BUFCHR
          CP     ':'
          LD     A,'0'
          JR     NZ,_gotsd

          ; Must be a '0' or '1'
          CALL   BUFCHR
          JR     Z,E_ERROR
          CP     '0'
          JR     Z,_gotsd
_not1:    CP     '1'
          JR     NZ,E_ERROR


          ; A has the SDCard (ASCII) number, which needs to be in E for the API call
_gotsd:   LD     E,A        ; SDCard (into E) as a letter '0' or '1'
          POP    AF         ; Drive letter...
          PUSH   AF
          PUSH   HL         ; disk number
          PUSH   AF
          PUSH   HL         ; disk number
          LD     HL,_MAPD
          CALL   PRINT
          POP    HL
          CALL   WRITE_D
          LD     A,'/'
          RST    08h
          LD     HL,_SDEV    ; 'sd'
          CALL   PRINT
          LD     A,E         ; SDCard
          RST    08H
          LD     A,E
          SUB    '0'
          LD     E,A
          LD     HL,_MAPD_TO
          CALL   PRINT
          POP    AF         ; drive
          RST    08H
          CALL   NL
          POP    HL
          POP    AF         ; drive

          ; Tell the API the mapping we want.
          SUB    'A'
          LD     D,A             ; Drive slot
          LD     B,0             ; Standard mapping
          LD     C,A_DSKMP       ; Map drive

          ; D:  Drive slot (0-15)
          ; E:  Physical SDCard (0 or 1)
          ; HL: Virtual disk on the SDCard
          RST    30h

          JR     main

; -------- SWRITE
; Write the current SDPAGE data back to the SDCard. If no address is specified then write
; to the address prevously loaded.
SWRITE:  CALL  WASTESPC

         CALL  SADDR
         JR    C,E_NOSD

         ; Special case if this is raw write to address 0:0 on disk 0. In this
         ; case patch up the reserved page checksum.
         LD    A,C
         CP    A_DSKRW
         JR    NZ,_nocs
         XOR   A
         OR    E   ; Checksum if the raw read from address zero (start of SDCard)
         OR    D
         OR    L
         OR    H
         JR    NZ,_nocs

         PUSH  HL
         PUSH  DE
         ; Raw read so if all bytes are
         CALL  SBCALCS

         ; Write checksum into page buffer
         LD    A,$FF
         LD    (SDPAGE+480),A
         LD    (SDPAGE+481),HL
         POP   DE
         POP   HL

         ; ---------- DEBUG DUMP
         ; 'C' is the read command. Need the write command.
_nocs:   LD    A,A_DSKWR-A_DSKRD
         ADD   C
         LD    C,A
         RST   30h
         JR    main
         ; ---------- DEBUG DUMP



; ---- _disres
; Display a title string then a value string. The value string is determined by the
; contents of the Z flag. Z: 'yes', NZ: 'no'
;   HL:    Message string
;    Z:    Test value
_disres:    PUSH     AF
            LD       A,H
            OR       L
            JR       Z,_nomsg
            CALL     PRINT
_nomsg:     LD       HL,COLSTR
            CALL     PRINT
            POP      AF
            LD       HL,_yes
            JR       NZ,_isyes
            LD       HL,_no
_isyes:     JR       PRINT_LN

; ---- SH_SDC
; Display status of SDCard 1 and 2
SH_SDC:     LD       HL,_SDIS
            CALL     PRINT
            LD       A,'1'
            RST      08h
            LD       A,1
            AND      E
            LD       HL,0
            CALL     _disres
            LD       HL,_SDIS
            CALL     PRINT
            LD       A,'2'
            RST      08h
            LD       A,2
            AND      E
            JR       _disres

; ---- SH_VDU
; Show the status of the VDU card (installed or not)
SH_VDU:     LD       HL,_VDIS
            LD       A,8
            AND      E
            JR       _disres

; ---- SH_PIO
SH_PIO:     LD       HL,_PDIS
            LD       A,4
            AND      E
            JR       _disres

; ---- SH_SW
; Display the current value of the configuration DIP switch.
SH_SW:      LD       HL,_CFGSW
            CALL     PRINT
            LD       HL,COLSTR
            CALL     PRINT
            LD       A,D
            CALL     WRITE_8
            CALL     NL
            RET


SH_HW:      CALL  SHDTIME
            CALL  NL
            ; Grab the inventory
            LD    C,A_HWINV
            RST   30h

            ; Inventory in DE. Display the results
            CALL  SH_SW
            CALL  SH_SDC
            CALL  SH_VDU
            CALL  SH_PIO
            CALL  NL
            RET


; Only need this code if we're configured to load a user defined default character set to
; the graphics card. If there is no graphic card then the monitor image can be smaller.
if CSET
; ----------- INITCSET
; Initialise the VGA character set
INITCSET:   PUSH     AF
            PUSH     BC
            PUSH     DE
            PUSH     HL

            BANK     2,$FF     ; Character set in flash
            BANK     1,CSET_PG ; Where to get the character set data

            LD       BC,$1000  ; The character set is 4K
            LD       HL,$4000  ; Source address
            LD       DE,$8000  ; Desitnation address
            LDIR

            ; Set up test data in video memory
            LD       HL,$A000  ; Start of display memory
            XOR      A         ; Character to write
            LD       B,$10     ; Outer loop

_c_cs1:     LD       C,0
_c_cs2:     LD       (HL),A
            INC      A
            INC      HL
            DEC      C
            JR       NZ,_c_cs2
            DJNZ     _c_cs1

            POP      HL
            POP      DE
            POP      BC
            POP      AF
            RET
endif

DECCHR:     RST      10h
            CP       3
            JR       Z,main
            CALL     WRITE_8
            JR       DECCHR




; --------------------- STRINGS
_INTRO:   DEFB "Z80 ZIOS 1.18.10",NULL
; _INTRO:   DEFB ESC,"[2J",ESC,"[H",ESC,"[J",ESC,"[1;50rZ80 ZIOS 1.18.8",NULL
_CLRSCR:  DEFB ESC,"[2J",ESC,"[1;50r",NULL

; Set scroll area for debug
_DBMD:    DEFB ESC,"[2J",ESC,"[1m",ESC,"[1;6HSTACK",ESC,"[m",ESC,"[11;50r",ESC,"[9;1H  >",ESC,"[12,1H",NULL

_PROMPT:  DEFB "> ",0
_UNKWN:   DEFB "unknown",0
_IOERR:   DEFB CR,LF,"Param error",0

; VT100 sequences
CURS_UP:   DEFB ESC,"[A",NULL
COLSTR:    DEFB CR,ESC,'[25C',NULL

_ERROR       DEFB "Error",0
_yes:        DEFB "YES", NULL
_no:         DEFB "NO", NULL
_NODRV       DEFB "No OS", NULL
_PRMBC       DEFB "BP:   @", NULL
_NOSDADD     DEFB "Bad SDcard address", NULL
_NODRIVE     DEFB "Drive A-", 'A'+15, NULL
_MAPD        DEFB "Map disk: ",NULL
_MAPD_TO:    DEFB " to drive ", NULL
_VDIS:       DEFB "Video",TAB,NULL
_CFGSW:      DEFB "Cfg Sw",TAB,NULL
_SDIS:       DEFB "SDCard",TAB,NULL
_SDEV:       DEFB "sd",NULL
_PDIS:       DEFB "PIO",TAB,NULL
_EMP:        DEFB " Missing", NULL
_PRE:        DEFB " Present", NULL
_INST:       DEFB "Installed", NULL
_SFND:       DEFB "Found", NULL
_NOTF:       DEFB "Not found", NULL
_NOTEMP:     DEFB "Not empty", NULL


          DSEG
; ---------------------- VARIABLES
; If you want the monitor code in ROM then add an ORG here to locate variables somewhere in RAM
OPMODE:    DEFS    1              ; Operational mode. 1=normal. 2=debug
CMDTAB:    DEFS    2              ; Operational mode. 1=normal. 2=debug

HW_SWTCH:  DEFS    1              ; Status of the 3 hardware config bits on boot

FIN_CODE:  DEFB    0
AUTO_RUN:  DEFB    0


SDPAGE     DEFS    512


DUMP_CHRS  EQU   SCRATCH


.END
