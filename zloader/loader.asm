import ../zlib/defs.asm
import config.asm

          extrn  PAUSE

          extrn  PRINT,PRINT_LN,GET_LINE,SKIPSPC,WASTESPC,BUFCHR,WRITE_8,WRITE_16,INHEX,INHEX_2,INHEX_4
          extrn  BRK_HK,SETHIST,GETHIST
          extrn  CMD_B
          extrn  DISASS, SETOFF
          extrn  GET_HEX,GET_DEC,INPTR,INBUF,INITSIO,MAPCASE,RXA,TXA,CKINCHAR,SERINT
          extrn  DEC2BIN

          extrn  DISPATCH

          ; Include SPI/SDCard library
          extrn  SD_INIT, SD_RBLK, SD_WBLK

          ; And the RTC/i2c library
          extrn  RTC_INI, RTC_MRD, RTC_MWR, RTC_GET, RTC_SET

          extrn  FILL,CLRSCR,INPUT,OUTPUT,MODIFY,HELP

          public main,BADPS,NL,OPMODE

          public PAGE_MP
          public OPMODE

          public ENDDIS, ERRDIS
          public APP_STK
          public PGADJ, PGMAP, PGMAPX, PGREST
          public DRVMAP
          public SDPAGE
          public PRTERR

; OP Code to use in RST vectors
VEC_CODE    EQU   $C3

; Config flags (for byte 0, NVRAM)
CF_LOADOS   EQU   00000001b

; Default flag byte if NVRAM invalid
CFG_DEF     EQU   CF_LOADOS

; Number of breakpoint locations (in code)
NUM_BK      EQU    64
BP_SIZE     EQU     4

; MN_PG is the memory page number from which this loader is running. NORMALLY this will be the page number
; of the first RAM page (20h if RAM is high). In debug this needs to be 1 because the real monitor loads
; into the first page.
MN_PG       EQU   IS_DEVEL

          ASEG
          ORG    0
          DI
          JR     START

; Install a breakpoint handler.
          ORG     BRK_HANDLER
BP_RST:   JR      DO_BP_S      ; Immediately jump to the correct BP handler.
_DIS:     JR      DISPATCH     ; Where I really wanted to go. Not efficient except in memory

; 30h is the OS entry point. This allows applications running in other pages to request
; a low level OS function (eg page mapping). Installed code can optionally handle all the
; hardware itself or can rely on the OS installing this handler. Note: It has to be one
; or the other!!! If installed then RST 30h invokes the OS services See '_DIS2' for available
; services and parameters.
          ORG    30h           ; OS entry point
          DI                   ; have to disable interrupts for IO calls because we're about to
                               ; switch out page 0
          LD    A,(MON_MP)     ; Page mask for page zero monitor
          OUT   (PG_PORT0),A
          JR    _DIS           ; Short jump because not enough space for absolute

; ----- Generic interrupt handler for MODE 1. Need to eventually move to MODE 2 and use more
; flexible vectoring. For now though stick with mode 1.
          ORG    38H
          ; ----- _EISR
_EISR:    PUSH   AF

          LD     A,(CONTXT)          ; Are we running in Supervisor mode?
          OR     A
          JR     Z,_SISR             ; Yes, so simple, fast and no context switching

          ; Switch to supervisor mode
          LD     A,(MON_MP)
          OUT    (PG_PORT0),A        ; Supervisor in CPU memory page 0
          LD     (APP_STK),SP        ; Save the application stack
          LD     SP,SP_STK          ; Get our supervisor stack

          CALL   _doisr_             ; Do the interrupt
          LD     SP,(APP_STK)        ; Restore the application stack
          LD     A,(PAGE_MP)         ; Context switch page 0
          OUT    (PG_PORT0),A        ; Back into application space
          POP    AF                  ; Restore A
          EI                         ; Back to application
          RETI

;----- RUN
; In reserved space ($08-$ff). A contains the first page number.
_RUN:     LD     HL,PAGE_MP+3
          LD     C,PG_PORT0+3
          LD     B,4
_sp2:     LD     A,(HL)
          OUT    (C),A
          DEC    C
          DEC    HL
          DJNZ   _sp2
          EI
          RST    0          ; And run...

_SISR:    CALL   _doisr_    ; Not supervisor so no page switching.
          POP    AF
          EI
          RETI
; WORK AROUND - calling SERINT directly causes an assembler/linker error. No idea why this
; fixes it :/
_doisr_:  JR     SERINT

; ----- HANDLE
; Intermediate routine to get the supervisor pages switched into memory
; before jumping to monitor code and then restoring the application space
; on completion. Keep as short as possible.
;    HL: Address of handler routine
HANDLE:   PUSH   AF
          LD     A,(CONTXT)
          OR     A
          JR     Z,_INSUP

          ; Running in application context so change to supervisor without losing any register values.
          LD     A,(MON_MP)         ; Map bank 0
          OUT    (PG_PORT0),A       ; Into supervisor space
          JR     _EXHNDLE           ; Do as much as possible in supervisor space

; -------------- _INSUP
; Running as supervisor so no context switch. Just make sure the registers haven't been changed.
_INSUP:   POP    AF
          LD     (R_PC_S),HL        ; Save the address of the routine we want to execute
          POP    HL                 ; Restore the original HL
          JR     JP_RUN             ; Go to the called routine, all registers restored.

;----- DO_BP_S
; Setup for DO_BP living in the application space.
; DON'T DO ANYTHING THAT SETS FLAGS!!!
DO_BP_S:  DI                   ; have to disable interrupts while in the debugger
          LD    (R_AF_S+1),A   ; So we can page switch... But flags NOT stored so don't damage!
          LD    A,(MON_MP)     ;
          OUT   (PG_PORT0),A   ; Switched out the application page. Now running from supervisor
          JR    DO_BP          ; In supervisor mode so can do breakpoint.

; ------ CONT
; Continue execution. Map all application pages into memory (PAGE_MP), restore the
; application registers then continue from where we left off.
_CONT:    OUT   (PG_PORT0),A
          POP    AF
          EI
          JR     JP_RUN

; ------------- _AENDH
; Application space tail end of a system call. Application memory mapped.
_AENDH:    POP     AF
           POP     HL
           RET

; ------------- ENDDIS
; Jump here at the end of an IO call to reinstate the application environment. All registers need to be
; carefully saved at this point and this code MUST be present in both the application space.
ENDDIS:   LD     SP,(APP_STK)   ; Got the appication stack back but it might not be in our address space so can't use
          LD     A,(PAGE_MP)
          OUT    (PG_PORT0),A   ; Back into application space
          EI                    ; Safe to allow interrupts again
          RET                   ; And carry on


; Debugger. Storage must be in page 0 reserved space
; JP_RUN - C3 is the JP opcode. By jumping to JP_RUN execution will
; continue from the current value of the PC. This avoids us having to
; push values onto the applications stack.
JP_RUN:    DEFB    $C3

; Storage area for working registers
R_PC_S     DEFS    2
R_SP_S     DEFS    2
R_AF_S     DEFS    2

MON_MP     DEFS    1    ; The page to select to switch to the monitor (the one in RAM)
CONTXT     DEFS    1    ; 0 if running as supervisor, 1 if running as application

END_RES    EQU     $

CSEG

; ------------- _EXHNDLE
; Now in supervisor space. The application stack has AF then HL which need to be restored. HL
; currently contains the address of the supervisor routine we want to run.
_EXHNDLE: LD     (R_PC),HL
          LD     (R_SP),SP
          LD     SP,SP_STK     ; Supervisor stack
          LD     HL,_ENDHND     ; End of call handler
          PUSH   HL
          PUSH   AF             ; Save flags
          CALL   PGMAPX         ; HL now has the adjusted application stack. Read/restore AF and HL
          POP    AF
          PUSH   DE
          INC    HL
          LD     A,(HL)         ; AF restored
          INC    HL             ; HL pointing to original HL value. Temp into DE
          LD     E,(HL)
          INC    HL
          LD     D,(HL)
          EX     DE,HL          ; HL restored
          POP    DE
          JR     JP_RUN         ; And go

; ------------- _ENDHND
; End handler - needs to restore as much as possible in supervisor space then return to app space for final switch.
_ENDHND:  PUSH   DE
          PUSH   AF
          PUSH   HL
          LD     HL,(R_SP)
          CALL   PGMAPX         ; HL now has the adjusted application stack. Read/restore AF and HL
          POP    DE             ; Old value for HL
          DEC    HL
          LD     (HL),D
          DEC    HL
          LD     (HL),E
          POP    DE             ; Old value for AF
          DEC    HL
          LD     (HL),D
          DEC    HL
          LD     (HL),E
          POP    DE
          LD     SP,(R_SP);
          LD     A,(PAGE_MP)
          OUT    (PG_PORT0),A        ; Back into application space
          JR     _AENDH

ERRDIS:   LD     HL,_IOERR
          CALL   PRINT_LN
          JR     ENDDIS


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
           JR    SET_FLASH          ; THIS LINE SHOULD BE IN FOR DEVELOPMENT BUT OUT BEFORE PROGRAMMING TO FLASH
endif
           XOR   A
           OUT   (PG_PORT0),A            ; Page 0 - might be Flash, might be RAM
           LD    A,$03              ; Switch to page mode
           OUT   (PG_CTRL),A
           ; Page 0 mapped to block 0. If it's read only then we have flash low, normal operation
           LD    A,$55
           LD    ($3F00),A
           LD    A,($3F00)
           CP    $55
           JR    NZ,FLASH_LOW        ; This would be normal operation
           LD    A,$AA
           LD    ($3F00), A
           LD    A,($3F00)
           CP    $AA
           JR    NZ, FLASH_LOW       ; If we read 0xAA then RAM is in the low 32 pages

SET_RAM:   ; RAM is low (so running through PI probably). Store 0x00 as the
           ; mask to use for RAM page selection
           XOR   A
           LD    (PAGE_MASK), A

           ; And ready to go...
           JR    RUN_CLI

FLASH_LOW:  ; 4. We're running from Flash. Copy to RAM page zero and map that to block zero.
            LD    A,RAM_PG_0
            OUT   (PG_PORT0+3),A
            LD    HL,0
            LD    DE,$C000
            LD    BC,$4000
            LDIR
            ; Switch RAM page 0 to block 0 (which should now be the same content and flash page 0 now)
            LD    A,RAM_PG_0
            OUT   (PG_PORT0),A

SET_FLASH:  ; Flash is LOW and we're running from RAM
            LD     HL,_STRTDS    ; Clear data area
            LD     DE,_STRTDS+1
            LD     (HL),0
            LD     BC,_ENDDS-_STRTDS+1
            LD    A,2
            LD    ($335e),A
            LDIR

            ; Initialise the application page map to the defaul pages on boot.
RUN_CLI:    LD    A,RAM_PG_0     ; This is the selector value for the first page of RAM when Flash is low
            LD    (PAGE_MASK), A
            ADD   LD_PGOFF       ; This will be the first page into which we're loading code
            LD    (LD_PAGE),A
            LD    HL,PAGE_MP
            LD    B,4
_wrn:       LD    (HL),A         ; Initialise the application page map
            INC   A
            INC   HL
            DJNZ  _wrn

            ; We're running in supervisor mode
            XOR   A
            LD    (CONTXT),A

            ; Give ourselves a stack at the end of our reserved page.
            LD    SP,SP_STK
            ; JR    NOSIO

            ; Set mode
            LD    A,1
            LD    (OPMODE),A
            LD    HL,CMD_TABLE
            LD    (CMDTAB),HL

            ; Prepare dump area
            LD     HL,DUMP_CHRS
            LD     A,20h
            LD     (HL),A
            INC    HL
            LD     (HL),A

            ; Set default DUMP mode
            LD     A,'I'
            LD     (DUMP_MODE),A

            ; Initialise the memory map
            LD     A,(PAGE_MASK)
            ADD    MN_PG
            LD     (MON_MP),A         ; The page we're running in

            ; Set up initialisation vectors - explicitly handled in code now
            LD     A,VEC_CODE      ; Set up RST jump table
            LD     ($08),A
            LD     ($10),A
            LD     ($18),A

            LD     HL,TXA
            LD     ($09),HL
            LD     HL,RXA
            LD     ($11),HL
            LD     HL,CKINCHAR
            LD     ($19),HL

            ; Initialise the SD card library
            CALL  SD_INIT

            ; Initialise drive map. Map logical drives 0-15 to physical blocks 1-16. Need to store
            ; the physical address << 6 (Upper 10 bits of the 32 bit SD card address).
            LD     HL,DRVMAP
            LD     B,16
            LD     DE,0000h        ; 0000 0000 0100 0000
_nxtdrv:    LD     (HL),E
            INC    HL
            LD     (HL),D
            INC    HL
            LD     A,40h         ; Add 40h to get to the next logical drive.
            ADD    E
            LD     E,A
            LD     A,0
            ADC    D
            LD     D,A
            DJNZ   _nxtdrv

            ; Initialise i2c and read the NV RAM
            CALL   RTC_INI
            CALL   NVLD

            ; LD     A,1            ; For driver load for debug version
            ; LD     (NVRAM),A

            ; Initialise the serial IO module
            XOR    A              ; Initialise SIO without implicit interrupt vectors (we do it).
            CALL   INITSIO
            IM     1              ; Using interrupt mode 1 AT THE MOMENT. Need to move to vectored at some point
            EI

            ; Really simple CLI now. Display
NOSIO:      LD    HL, _INTRO
            CALL  PRINT_LN
            CALL  SH_DTIME

if CSET
            CALL  INITCSET
endif

if !IS_DEVEL
          ; Check the state of the DIL switches and look for autoboot mode
          IN    A,(PG_PORT0)

          ; Upper 3 bits are the DIL switch settings. Use the bottom
          ; two to decide on auto boot mode.

          ; Move upper 3 bits to lower 3
          OR    A
          RLCA
          RLCA
          RLCA
          AND   03h             ; Only interested in bits 0 and 1.

          DEC   A               ; If 01 then do Pi boot
          JR    Z,BOOTPI
          DEC   A               ; If 02 then do SDCard boot
          JR    NZ,main
          LD    A,1
          LD    (AUTO_RUN),A    ; Set auto-run mode
          CALL  SDPREP
          JR    Z,_ld01         ; Checksum OK so go load default image.
          JR    main
else
          ; DEBUG - drop straight into an SDCard boot - it's broken at the moment.
          ; CALL  SDPREP
          ; JR    NZ,main
          ; LD    A,1
          ; LD    (AUTO_RUN),A       ; Set auto-run mode
          ; JR    _ld01         ; Checksum OK so go load default image. Boot OS:1
          JR      main
          ; In development mode don't use the DIL switches. This allows us to develop the loader more efficiently.
endif

NL:       WRITE_CRLF
          RET

BADPS:    LD    HL,_IOERR
PRTERR:
_prterr:  CALL  PRINT_LN

main:     LD    SP,SP_STK      ; Dump the stack and restart on *main*
          LD    A,1
          CALL  MAPCASE         ; Map all letters to uppercase
          LD    HL, _PROMPT
          CALL  PRINT
          CALL  GET_LINE
          CALL  NL
          CALL  SKIPSPC
          OR    A
          JR    NZ, _newcmd
          ; empty line so check if we have a 'LAST_CMD'
          LD    A,(LAST_CMD)
          OR    A
          JR    Z,main ; No last command

          LD    HL,CURS_UP
          CALL  PRINT
          LD    A,(LAST_CMD)

          ; Which command?
_newcmd:  LD    C,A
          CP    'D'  ; 'D', 'N', 'S' and 'G' are repeatable
          JR    Z,_cancnt
          CP    'S'
          JR    Z,_cancnt
          CP    'N'
          JR    Z,_cancnt
          CP    'G'
          JR    Z,_cancnt
          XOR   A
_cancnt:  LD    (LAST_CMD),A   ; Store for next time
          LD    HL,(CMDTAB)
_nxtcmd:  LD    A,(HL)
          INC   HL
          LD    E,(HL)
          INC   HL
          LD    D,(HL)
          INC   HL
          OR    A
          JR    Z,err      ; End of table
          CP    C
          JR    NZ,_nxtcmd
          ; Found the command, address in DE
          EX    DE,HL
          JP    (HL)
err:      LD    HL, _ERROR
          CALL  PRINT_LN
          XOR   A              ; Clear last command
          LD    (LAST_CMD),A
          JR    main

; ------------------- _GDECNUM
; Read two decimal digits into the A register. C used for workspace. Return
; FFh if there's an error.
_GDECNUM: PUSH  BC
          CALL  BUFCHR
          JR    Z,_faildec
          CALL  DEC2BIN
          JR    C,_faildec
          ADD   A,A
          ADD   A,A
          ADD   A,A
          ADD   A,A
          LD    C,A
          CALL  BUFCHR
          JR    Z,_faildec
          CALL  DEC2BIN
          JR    C,_faildec
          OR    C
          POP   BC
          OR    A       ; Clear carry flag
          RET

_faildec: POP   BC
          SCF
          RET

; ------------------- DTIME
; Display and eventually set date time
DTIME:    CALL     BUFCHR        ; Step past the 'T'
          CALL     RTC_INI       ; Initialise RTC
          LD       HL,_TIME      ; and
          CALL     RTC_GET       ; get the current time
          CALL     WASTESPC
          JR       Z,shtime

          ; Expect a string in the format:
          ;   YYMMDD:HHMMSS
          ; OR
          ;   :HHMMSS
          ; Build the time up into the _TIME structure before writing to
          ; the RTC.
          ;
          ; Read current time first
          CP       ':'
          JR       Z,_stime

          CALL     _GDECNUM     ; Expect a 2 digit year.
          JR       C,shtime
          LD       (_TIME+6),A

          CALL     _GDECNUM     ; 2 digit month
          JR       C,shtime
          LD       (_TIME+5),A

          CALL     _GDECNUM     ; 2 digit date
          JR       C,shtime
          LD       (_TIME+4),A

_stime:   CALL     BUFCHR
          JR       Z,_sdtim     ; End so only setting date
          CP       ':'
          JR       NZ,shtime

          CALL     _GDECNUM     ; Expect a 2 hours
          JR       C,shtime
          LD       (_TIME+2),A

          CALL     _GDECNUM     ; Expect a 2 mins
          JR       C,shtime
          LD       (_TIME+1),A

          CALL     _GDECNUM     ; Expect a 2 secs
          JR       C,_sdtim     ; seconds optional
          LD       (_TIME),A

_sdtim:   LD       HL,_TIME
          CALL     RTC_SET

shtime:   CALL     SH_DTIME
          JR       main


; ------------------- DNVRAM
; Dump the 56 bytes of NVRAM in the RTC chip
DNVRAM:   LD     HL,NVRAM
          LD     BC,3800h    ; Read 22(?) bytes from offset 0
          CALL   RTC_MRD
          LD     HL,NVRAM
          LD     C,7         ; 7 rows
_nrnv:    LD     B,8         ; 8 bytes per row
_ncnv:    LD     A,(HL)
          CALL   WRITE_8
          INC    HL
          WRITE_CHR SPC
          DJNZ   _ncnv
          WRITE_CRLF
          DEC    C
          JR     NZ,_nrnv
          JR     main

; ------------------- DUMP
DUMP:     LD    A,(DUMP_MODE)
          LD    B,A               ; Accepted mode in 'B'
          LD    HL, (DUMP_ADDR)   ; HL will be the last dumped address+1
          CALL  WASTESPC
          JR    Z,cnt_dump
          CP    'N'               ; Dump NV RAM (all 56 bytes)
          JR    Z,DNVRAM
          CP    'T'               ; Dump date time
          JR    Z,DTIME
          CP    'G'               ; If >= 'G' then it can't be the start of a hex number so it must be mode.
          JR    C,no_mode
          CALL  SKIPSPC
          LD    B,A               ; Whatever the character is, we store this as the mode
          LD    (DUMP_MODE), A

no_mode:  CP    'D'               ; Dump SD card block
          JR    Z,MAPDSK
          CALL  GET_HEX
          JR    NZ,_useval
          LD    HL,(DUMP_ADDR)
          ; Can append a '+' or '-' to set application or physical address space
_useval:  LD    (DUMP_ADDR),HL
          CALL  SKIPSPC
          JR    Z,cnt_dump
          CP    '-'
          JR    NZ,_nphy
          ; Physical addressing
          LD    A,1
          LD    (DUMP_TX),A
          JR    cnt_dump

_nphy:    CP    '+'
          JR    NZ,cnt_dump
          XOR   A
          LD    (DUMP_TX),A

cnt_dump: LD    A,B
          CP    'D'               ; Dump SD card block
          JR    Z,MAPDSK
          CP    'M'
          JR    NZ,decode         ; Mode is not 'M' so it's an instruction level dump

          ; Display 8 blocks of 16 characters
          LD    C,8

dloop2:   LD    A,(DUMP_TX)       ; Need to translate the address?
          OR    A
          JR    Z,_toap           ; Translate the address

          ; Using physical addresses so highlight
          WRITE_CHR '+'
          CALL  WRITE_16
          JR    dloop3

_toap:    CALL  WRITE_16          ; 4 hex digits from HL
          CALL  PGMAPX            ; Translate into an offset and a page number and map application pages into memory

          ; Dump address (start of line)
dloop3:   WRITE_CHR SPC
          LD    DE, DUMP_CHRS+2
          LD    B,16

dloop:    LD    A,(HL)
          CP    20h
          JR    C, outdot
          CP    7fh
          JR    NC, outdot
          LD    (DE),A
          JR    writeout
outdot:   LD    A,'.'
          LD    (DE), A
          LD    A,(HL)
writeout: INC   DE
          INC   HL
          CALL  WRITE_8
          WRITE_CHR SPC
          DJNZ  dloop
          PUSH  HL
          LD    HL, DUMP_CHRS
          CALL  PRINT_LN
          POP   HL
          LD    HL,(DUMP_ADDR)
          LD    A,16
          ADD   L
          LD    L,A
          LD    A,0
          ADC   H
          LD    H,A
          LD    (DUMP_ADDR), HL
          DEC   C
          JR    NZ,dloop2
          CALL  PGRESTX            ; Reset application space registers
          JR    main


BOOT:     CALL  WASTESPC
          CP    'O'                ; To boot MUST use 'BO' to avoid conflict with BP in debug mode
          JR    NZ,BP
          CALL  BUFCHR             ; Step over the O
          CALL  SKIPSPC
          CP    'S'                ; Load from SD card
          JR    Z,SDLOAD           ; Use SDCard loader
BOOTPI:   CP    '-'
          JR    Z,_bootihx         ; Don't autorun

          LD    A,1
          LD    (AUTO_RUN),A       ; Set auto-run mode

_bootihx: XOR   A
          LD    (LOAD_MODE),A      ; Force Intel Hex format load
          LD    HL,_BOOTHEX        ; From the default file
          LD    BC,8
          JR    LOADF

; _recerr: Return with A set to 0xfe and the Z flag clear (NZ)
_recerr:  XOR   A
          DEC   A
          DEC   A
          RET
; _impeof - End of file record. Return NZ to stop processing and 0xFF in A to indicate no error
_impeof:  XOR   A
          DEC   A
          RET

; Function to process a single line of text from the in-buffer. Pulled this into a subroutine
; so it can be called from both a file load and a paste to terminal. Must only be called
; once the INBUF has been primed with text.
; INPUT:  INBUF contains a line of text ready to be processed.
; OUTPUT: Z  - return with with Z flag set if no error, otherwise Z clear for error, code in A (?)
_procln:  CALL  BUFCHR
          CP    ':'
          RET    NZ                ; Return on invalid start of line
          ; Accept this line. Format: [LEN 1][ADDR 2][REC_TYPE 1][DATA 2]+[CHKSM 1]
          CALL  INHEX_2            ; Length -> A

          JR    C, _recerr
          LD    B,A                ; Length into B
          CALL  INHEX_4            ; Address - HL
          JR    C, _recerr
          CALL  INHEX_2            ; Command - which should be 00. If not then EOF so end.
          JR    C, _recerr
          OR    A
          JR    NZ, _impeof
          LD    A,B                ; Check for zero length
          OR    A
          JR    Z,_recerr

          ; The upper two bits of the address identifies the page block - set
          ; up the page register so it's in the range 4000h-7fffh.
          CALL  PGMAP

          ; And write these bytes into that page
next_b:   CALL  INHEX_2            ; Get a hex byte
          JR    C, _recerr
          LD   (HL), A
          INC   HL
          DJNZ  next_b
          ; END of that record (ignoring checksum)
          ; Return with Z flag set (A=0)
          XOR   A
          RET

; --------------------------- DUMP decode (instruction decode and display) MIGHT become
; a disassembler at some point.
decode:   LD    B,20       ; Number of instructions
_nexti:   LD    A,(DUMP_TX)
          CALL  DECINST
          DJNZ  _nexti
          LD    (DUMP_ADDR), HL
          CALL  PGRESTX    ; Undo any application space page damage
          JR    main

; -- DECINST
; Decode and display single instruction. HL points to the sart of the instruction. Displays
; the HEX bytes for this instruction followed by a newline.
; INPUT:  HL - the application space address of the instruction
; OUTPUT: HL - First byte of _next_ instruction
; Registers not saved: A
DECINST:  PUSH  BC         ; DISASS returns instruction information we don't need in BC
          PUSH  DE
          PUSH  HL         ; Application space address we want to display
          OR    A
          JR    Z,_appdec2
          ; LD    B,A
          ; WRITE_CHR '+'
          ; LD    A,B
          ; CALL  WRITE_16   ; Write out the application space address
          JR    _phydec

_appdec2: CALL  WRITE_16   ; Write out the application space address
          CALL  PGMAPX     ; Translate into an offset and a page number and map application pages into memory

          ; A contains the block number of the address, 0-3:
          ;    0: -4000h
          ;    1:  0000h
          ;    2:  4000h
          ;    3:  8000h
          RRCA
          RRCA             ; Now in top 2 bits
          SUB   40h        ; Gives the top byte of the offset
          LD    D,A
          LD    E,0
          EX    DE,HL
          CALL  SETOFF
          EX    DE,HL

_phydec:  WRITE_CHR SPC
          PUSH  HL
          CALL  DISASS     ; HL now points at the description
          LD    C,A        ; The length of the instruction - saved for later
          LD    D,H        ; Save pointer to disassembled string
          LD    E,L
          POP   HL         ; Back to pointing to the start of the instruction
          LD    B,C        ; Number of bytes to display
_nextb:   LD    A,(HL)     ; Write out 'B' HEX bytes for this instruction
          CALL  WRITE_8
          WRITE_CHR SPC
          INC   HL
          DJNZ  _nextb

          ; Output disassembler description
          LD    HL,COLSTR
          CALL  PRINT
          LD    H,D
          LD    L,E
          CALL  PRINT_LN
          POP   HL          ; Restore the adddress we were given originally. BC contains the length of the instruction
          LD    A,L
          ADD   C
          LD    L,A
          LD    A,B         ; Which will be zero because we finised a DJNZ loop
          ADC   H
          LD    H,A         ; Adjusted
          ; Row complete
          POP   DE
          POP   BC
          RET


; When loading a binary file, the user can optionally enter a hex load address. The default address is 100h
cmd_bin:  LD    A,1
          LD    (LOAD_MODE),A     ; Binary mode
          CALL  WASTESPC
          JR    Z,cmd_load
          CALL  INHEX_4
          JR    C,BADPS
          LD    (PROGADD),HL
          JR    cmd_load

; ---------------- LOAD
; Process all lines starting with a ':'
LOAD:     LD    HL,100h           ; Default load address for binary data
          LD    (PROGADD),HL
          XOR   A
          LD    (AUTO_RUN),A      ; Cancel auto-run mode
          LD    (LOAD_MODE),A     ; Default to HEX mode
          CALL  SKIPSPC
          JR    Z,_hex_ld         ; End of command line. Default to HEX from stdin
          CP    'F'               ; Load from file
          JR    Z,cmd_bin         ; Use CMD_B to load
          CP    'H'               ; Load from file (HEX format)
          JR    Z,cmd_load        ; Use CMD_B to load, HEX format
_hex_ld:  XOR   A
          CALL  SETHIST
          LD    HL, _WAITING      ; Command line HEX input
          CALL  PRINT_LN
nextline: CALL  GET_LINE
          JR    Z, nextline       ; Ignore blank lines

          CALL  _procln           ; If Z flag is set then the line has been processed property
          JR    Z, nextline
          ; If A contains 0xFF then end of file (?)
          CALL  SETHIST           ; Know A is not zero at this point
          INC   A
          LD    HL,_COMPLETE
          JR    Z,_prterr
          LD    HL,_REC_ERR
          JR    _prterr           ; Otherwise an error


; Use the CMD_B loader to read blocks of data. Rest of line is the name of the file
cmd_load: LD    HL,_FNAME
          CALL  PRINT
          XOR   A
          CALL  MAPCASE    ; Don't map case
          CALL  GET_LINE
          LD    A,1
          CALL  MAPCASE    ; Map lowercase to uppercase
          CALL  NL
          LD    BC,0
          LD    HL,INBUF
          ; Count number of characters until end of line OR space
_cnxt:    LD    A,(HL)
          OR    A
          JR    Z,_eofn
          CP    ' '
          JR    Z,_eofn
          INC   C
          INC   HL
          JR    _cnxt
_eofn:    LD    HL,(INPTR)

          ; This is also an entry point. Call this with:
          ;   HL: Pointer to file name
          ;   BC: Number of characters in the file name
LOADF:    LD    A,$10         ; Open file command
          LD    DE,FILE_BUF
          CALL  CMD_B         ; Open the file on the remote system

          ; A contains the status. Zero means the file exists, !0 it doesn't
          JR    Z,_blkdn

          ; File doesn't exist so error
          LD    HL,_FERR
          JR    _prterr

_blkdn:   ; File is OPEN. LOAD_MODE tells us whether to process input blocks as ASCII hex or not.
          LD    A,(LOAD_MODE)
          OR    A
          JR    Z,_hexld

          ; ---- BINARY LOAD
          ; loaded from the address stored in PROGADD. We're going to load into
          ; user pages though which are to be located in bank 1.
          LD    HL,0
          LD    (LOAD_CNT),HL  ; Clear block count
          LD    DE,(PROGADD)   ; Where to put the file we're loading (binary load)

          EX    DE,HL
          CALL  PGMAP          ; Map programme space address into user space.
          LD    (PROGPG),A     ; Save the selected page
          EX    DE,HL

_nxtblk:  LD    A,$11      ; Read block command
          LD    BC,0       ; No payload
          CALL  CMD_B
          CALL  SHOWBCNT   ; Display block count and increment
          OR    A
          JR    NZ,_fin
          ; DE now points to the first byte AFTER the downloaded block. If the upper 2 bits is
          ; no longer '01' then we've overflowed the logical page boundary so adjust.
          LD    A,D
          RLCA
          RLCA
          AND   03h        ; If it's still 01 then can just contine
          DEC   A
          JR    Z,_nxtblk
          LD    A,D        ; Map back to start of block 1
          AND   3fh
          OR    40h
          LD    D,A
          LD    A,(PROGPG)
          INC   A          ; Map the next page into block 1 space
          LD    (PROGPG),A
          OUT   (PG_PORT0+1),A    ; Set the hardware
          JR    _nxtblk

_fin:     INC   A
          JR    Z,_errdn
          CALL  NL
          JR    main
_errdn:   LD    HL,_ERRDNM
          JR    _prterr

; ------------------- _hex_load
; File is open. Read blocks into our own space and use to fill the input line buffer and call
; the HEX load for each line.
_hexld:   LD    DE,INBUF
          LD    (INPTR),DE
          XOR   A
          LD    (FIN_CODE),A
_hexldn:  LD    A,(FIN_CODE)
          OR    A
          JR    NZ,_eofhx
          LD    A,$11        ; Read block command
          LD    BC,0         ; No payload
          LD    DE,FILE_BUF  ; Into our 128 byte buffer
          CALL  CMD_B
          LD    (FIN_CODE),A ; Status for THIS block - but doesn't mean this block doesn't include data.
          LD    B,80h        ; Size of block (bytes)
          LD    HL,FILE_BUF
          LD    DE,(INPTR)
_nexchr:  LD    A,(HL)
          LD    (DE),A
          INC   HL
          CP    0Dh
          JR    Z,_goon
          INC   DE
          CP    0Ah          ; Newline?
          JR    NZ,_goon
          ; Have a full line in the buffer so process now
          PUSH  HL
          PUSH  BC
          XOR   A
          DEC   DE
          LD    (DE),A
          LD    HL,INBUF
          LD    (INPTR),HL
          CALL  _procln
          POP   BC
          POP   HL
          JR    NZ, _eofhxok   ; Saw end of hex data record
          LD    DE,INBUF

_goon:    DJNZ  _nexchr
          ; End of block - should be a new block!
          LD    (INPTR),DE
          JR    _hexldn

_eofhxok: LD    A,(AUTO_RUN)
          OR    A
          LD    HL,0        ; Run from address 0000h
          JR    NZ,PREPRUN
          LD    HL,_COMPLETE
          JR    _prterr

_eofhx:   LD    HL,_REC_ERR
          JR    _prterr           ; Otherwise an error

; ------------------- SET_RGS
; CMD: Set register value. R reg=val
; reg is A,B,C,D,E,H,L,BC,DE,HL,IX,IY
; val is an 8 or 16 bit hex value
SET_RGS:  CALL  SKIPSPC    ; Get the name of the register, one or two characters
          JR    Z,SHOW_RGS ; nothing to use
          LD    D,A        ; First character (required)
          LD    E,0
          CALL  BUFCHR     ; either a space or '=' otherwise use it
          CP    '='
          JR    Z,_getval
          CP    ' '
          JR    Z,_8bit
          LD    E,A
_8bit:    CALL  SKIPSPC    ; Waste characters until '=' or end of line
          JR    Z,_rerr    ; End of line
          CP    '='
          JR    NZ,_8bit
          ; Now get the hex value to write. Don't care about size at this point
_getval:  CALL  GET_HEX    ; Value in HL
          JR    Z,_rerr    ; no value entered
          ;     DE: One or two character register name
          ;     HL: Value to store in register
          CALL  _reg_addr
          JR    C,_rerr    ; Unknown register name
                           ; DE  now contains the address of the register to write. A:0 8 bit, A!=0 16 bit
          EX    DE,HL      ; 8 bit is common between the two options
          LD    (HL),E
          JR    NZ,_rend   ; Z will be set for an 8 bit register
          INC   HL         ; It's 16 bits so write the second byte
          LD    (HL),D
_rend:    JR    SHOW_RGS

_rerr:    LD    HL,_ERROR
          JR    _prterr

; -------------- reg_addr
; INPUT:  DE: One or two byte name of the register (ASCII)
; SAVED:  HL
; OUTPUT: DE: The address of the register
;         C:  Set on unknown register name
;
_reg_addr:PUSH  HL       ;DE contains the name of the register
          LD    A,E
          OR    A        ; If zero then 8 bit
          JR    Z,_lu8
          ; 16 bit look up
          LD    HL,R_ADDR_16
_nxt16:   LD    A,(HL)
          OR    A
          JR    Z,_nfnd  ; End of table - no match
          INC   HL
          CP    D
          JR    NZ,_miss16
          LD    A,(HL)
          CP    E
          JR    NZ,_miss16
          ; Found - the address is in the next two bytes
          XOR   A
_resreg:  INC   HL
          LD    E,(HL)
          INC   HL
          LD    D,(HL)
          POP   HL
          RET
_miss16:  INC   HL
          INC   HL
          INC   HL
          JR    _nxt16
_lu8:     LD    HL,R_ADDR_8
_nxt8:    LD    A,(HL)
          OR    A
          JR    Z,_nfnd
          CP    D
          JR    NZ,_miss8
          OR    A        ; Clear C flag
          ; Found
          JR    _resreg
_miss8:   INC   HL
          INC   HL
          INC   HL
          JR    _nxt8
_nfnd:    ; Bad register name, set the carry flag and return
          POP  HL
          SCF
          RET


SHOW_RGS: CALL  DO_RGS
          JR    main

; ------------------- SHOW_RGS
DO_RGS:
SHOW_RGX: LD    HL,SAVE_POS
          CALL  PRINT

          ; Build a text representation of the Flags in the 'INBUF' ready to be displayed.
          LD    HL,R_F_DESC
          CALL  PRINT
          LD    HL,FLAGS_DESC
          LD    DE,INBUF
          PUSH  DE
          LD    BC,9
          LDIR
          LD    HL,INBUF-1
          LD    A,(R_AF)
          LD    B,8
_nextf:   INC   HL
          RLCA
          JR    C,_nextfn
          LD   (HL),' '
_nextfn:  DJNZ  _nextf
          POP   HL
_skipxx:  CALL  PRINT

          ; Write out the 8 bit A value
          LD    HL,R_A_DESC
          CALL  PRINT
          LD    A,(R_AF+1)
          CALL  WRITE_8
          ; Do the same for shadow A

          LD    HL,R_AP_DESC
          CALL  PRINT
          LD    A,(R_AF_P+1)
          CALL  WRITE_8

          ; Write out the main 11x16 bit registers
          LD    HL,REG_DESC
          LD    DE,R_PC              ; DE pointing to the values
          LD    B,12

          ; Output description
_nextreq: PUSH  DE                   ; Save pointer to value. DE: value ptr, HL: dec pointer
          LD    A,(HL)
          LD    E,A
          INC   HL
          LD    D,(HL)
          INC   HL
          OR    D
          JR    NZ,_dorreg            ; Skip this value

          POP   DE
          INC   DE
          INC   DE
          JR    _rnxt

_dorreg:  EX    DE,HL
          CALL  PRINT                ; Print the description
          POP   HL                   ; Pointer to value in HL
          PUSH  DE                   ; Save pointer to desc table
          LD    E,(HL)
          INC   HL
          LD    D,(HL)
          INC   HL
          EX    DE,HL                ; current value in HL. DE contains value pointer
          CALL  WRITE_16             ; DISPLAY
          POP   HL                   ; Get desc pointer back
_rnxt:    DJNZ  _nextreq

          ; UPDATE STACK CONTENT. Show one lower then the previous 7
          LD    HL,HOME
          CALL  PRINT
          LD    HL,(R_SP)
          LD    DE,15
          ADD   HL,DE
          LD    E,L            ; HL is the application address
          LD    D,H            ; Save application address in DE
          CALL  PGMAPX         ; HL is now the physical address, DE application address
          LD    B,8
_nextstk: PUSH  HL             ; HL is now the physical address, DE application address
          LD    HL,STK_NXT
          CALL  PRINT
          POP   HL
          ; Display application space address
          EX    DE,HL          ; HL application, DE physical
          DEC   HL             ; Want to display the application address
          CALL  WRITE_16
          WRITE_CHR ':'
          WRITE_CHR ' '
          EX    DE,HL          ; DE application, HL physical
          DEC   DE
          PUSH  DE             ; Save application address
          LD    D,(HL)
          DEC   HL
          LD    E,(HL)
          DEC   HL
          EX    DE,HL          ; Display the data read. Physcial address back into DE, HL=stack data
          CALL  WRITE_16
          EX    DE,HL          ; Physical address in HL
          POP   DE             ; Restore application address to DE
          DJNZ  _nextstk

          LD    HL,REST_POS
          CALL  PRINT
          RET

; ----- Single Step
SSTEP:    LD    HL,1
          LD    (STP_CNT),HL   ; By default step once
          CALL  WASTESPC
          JR    Z,_stp1
          CALL  GET_DEC        ; Get optional execution address into HL
          JR    C,_stp1
          LD    (STP_CNT),HL   ; By default step once
_stp1:    LD    HL,0
          LD    E,1            ; A -> !0
          CALL  SSTEP_BP       ; Set single step BP then go
          JR    DO_GO

; ------------------- go
GO:       CALL  WASTESPC
          JR    Z,_noadd

          ; Is there an address?
          CALL  GET_HEX
          JR    Z,_noadd

          ; Have an address - put it in the virtual PC
          LD    (R_PC),HL

_noadd:   LD    HL,1
          LD    (STP_CNT),HL ; Make sure we don't keep running when we hit a breakpoint!!
          LD    E,2
          XOR   A
DO_GO:    PUSH  DE
          CALL  INSTBP       ; Install all permanent breakpoints
          POP   DE
          LD    A,E
          CP    2
          JR    NZ,_godo
          LD    A,(AUTO_STP)
          OR    A
          JR    NZ,SSTEP     ; Transmute into a single step

_godo:    LD    HL,BRK_HDLR  ; Our break handler - was DO_BP_S but that's wrong now. Will need to get there at some point though
          LD    (BRK_HK),HL

          ; This could be the first time we'e run so check whether we've installed the drivers.
          CALL  INSTDRV

          LD    A,(PAGE_MP)
          OUT   (PG_PORT0+1),A        ; Back into application space
          LD    HL,(R_PC)
          LD    (R_PC_S+$4000),HL     ; Where we want to continue execution from

          LD    HL,(R_SP)
          CALL  PGMAPX                ; Stack memory now accessible in HL
          LD    DE,(R_AF)
          DEC   HL
          LD    (HL),D
          DEC   HL
          LD    (HL),E
          LD    HL,(R_SP)
          DEC   HL
          DEC   HL                    ; So we can later POP AF
          LD    (R_SP),HL

          ; Map all pages EXCEPT page 0
          LD     HL,PAGE_MP+3    ; Map all app pages into memory
          LD     C,PG_PORT0+3
          LD     B,3
_sp3:     LD     A,(HL)
          OUT    (C),A
          DEC    C
          DEC    HL
          DJNZ   _sp3

          ; Restore as many regs as possible here. No longer need our stack so can
          ; discard. Only thing that can't be restored is A which is needed to reset
          ; the application space.
          LD    SP,R_AF_P
          POP   AF
          EX    AF,AF'
          POP   BC
          POP   DE
          POP   HL
          EXX               ; Alternate register set restored
          POP   BC
          POP   DE
          POP   HL
          POP   IX
          POP   IY

          LD    SP,(R_SP)   ; and load the application SP
          LD    A,(PAGE_MP)

          JR    _CONT       ; And......... GO!


; ------- SSTEP_BP
; Set a single step break point. Uses the current R_PC, disassembles the current
; operation and then decides where this instruction can go. Most instruction just step
; forward however branches, calls, returns etc need more work.
; INPUT: E - Boolean. If true then step into, if false the step over.
SSTEP_BP: LD    A,E
          LD    (STP_IN),A  ; Record whether this is step over or into
          LD    HL, (R_PC)  ; HL is the current application PC
          ; Get this into physical memory
          PUSH  HL          ; Save application space address
          CALL  PGMAPX      ; Convert app space to physical space
          LD    D,H
          LD    E,L         ; DE now points into the first physical address
          CALL  DISASS      ; Only disassemble to get control flow information
          POP   HL          ; And get the original PC back again (pointing to start of instruction for rel jumps)
          ; A: Instruction length
          ; C: Extended status
          ; HL: Unchanged - start of THIS instruction
          ; DE: Mapped physical address of target instruction (start of)
          CALL  _ADD16      ; Adjust the application address to start of next opcode
          LD    E,L         ; DE now points at the mapped address space start of next opcode
          LD    A,1         ; Set a type 1 BP here (single step)
          CALL  SETBP       ; HL point to next instruction
          LD    A,C         ; Control flow from this instruction - can it go anywhere else?
          AND   7
          RET   Z           ; No change of control so no more BP to set

          ; C determines the type of reference:
          ; 01 - relative : last byte of instruction is PC relative offset
          ; 02 - absolute : last two bytes of inststruction is an absolute address
          ; 03 - return   : look at the stack to find the next instruction
          ; 04 - rst      : RST xxH call
          ; 05 - special  : JR (HL)(IX)(IY) - special processing required.
          ; 06 - call     : Call to absolute address
          DEC   A
          JR    NZ,_type2

          ; HL contains the application address of the next instruction. This is the address from
          ; which any offset must be added.

          DEC   DE         ; It's RELATIVE. DE is the physical address of the start of the next instruction
                           ; Want the offset from the next byte
          LD    A,(DE)     ; A now contains the relative offset (2s comp). Realtive to HL
          LD    E,A        ; Make DE the 16 bit sign extended offset
          ADD   A,A        ; Sets the carry flag if bit 7 set - negative address
          SBC   A,A        ; Zero or FF depending on C flag - ie
          LD    D,A        ; DE = sign extended 16 bit offset
          ADD   HL,DE      ; Work out the target of the jump. HL is the app space target address

_setbp:   LD    A,1
          CALL  SETBP      ; And set the BP
          RET

_type2:   DEC   A
          JR    NZ,_type3

          ; DE: Physical address of next op, HL application address of next op
_absjp:   EX    DE,HL
          ; HL: Physical address of next op, DE application address of next op

          DEC   HL         ; Absolute address - use last 2 bytes of the instruction (DE pointing to phy mapped address)
          LD    D,(HL)
          DEC   HL
          LD    E,(HL)
          EX    DE,HL      ; HL is now the target of the jump (or call)
          PUSH  AF
          PUSH  HL
          WRITE_CHR '+'
          CALL  WRITE_16
          POP   HL
          POP   AF
          JR    _setbp

_type3:   DEC   A
          JR    NZ,_type4
          ; It's a RET instruction or variant. Return address is on the stack.
          LD    HL,(R_SP)
          CALL  PGMAPX      ; Convert app space to physical space
          LD    E,(HL)
          INC   HL
          LD    D,(HL)      ; DE is return address read from the stack
          EX    DE,HL
          JR    _setbp

_type4:   DEC   A
          JR    NZ,_type5
          ; It's an RSTxx instruction. We don't try to step into these right now. They are generally system calls
          ; and it's not good to tamper!
          RET

_type5:   DEC   A
          JR    NZ,_type6
          LD    HL,(R_PC)  ; JP (XX) - HL, IX, IY. Instruction determines which
          CALL  PGMAPX      ; Convert app space to physical space
          LD    A,(HL)
          CP    0E9h
          JR    NZ,_not_hl
          LD    HL,(R_HL)
          JR    _setbp
_not_hl:  CP    0DDh
          JR    NZ,_not_ix
          LD    HL, (R_IX)
          JR    _setbp
_not_ix:  LD    HL, (R_IY)
          JR    _setbp

_type6    ; It's a subroutine call. Same as absolute jump except check E=0. If 0 then
          ; we want to step over the sub call so don't add the extra breakpoint
          LD    A,(STP_IN)
          OR    A
          RET   Z
          JR    _absjp   ; Treat it as an absolute jump


; ------------------- BRK_HDLR
; Break handler - called in ISR context. Stack will look like:
;    Ret address from SERINT -> _EISR ()
;    AF at start of ISR
;    HL at start of ISR
;    AF containing the break character
;    <--- SP
; The application stack (APP_STK) in application stack will have AF at the top
; and above that the the application execution address in operation when break
; was pressed.
;
; The only value on the stack of any value is the original value of HL.
BRK_HDLR: LD     HL,_exitisr    ; Get us out of ISR mode
          PUSH   HL
          EI
          RETI
_exitisr: LD     HL,_BRK
          CALL   PRINT_LN
          ; Disable multi-step
          XOR   A
          LD    (AUTO_STP),A
          LD    (STP_CNT+1),A
          INC   A
          LD    (STP_CNT),A

          PUSH   DE
          LD     HL,(APP_STK)   ; Application stack pointer, AF is at the top of that stack
          CALL   PGMAPX         ; Map stack pages into memory - txlated address in HL
          LD     E,(HL)
          INC    HL
          LD     D,(HL)         ; DE contains what was in AF
          ; Next 2 bytes are the return address. This will get decrements in the BP handler
          ; because it thinks it's been called via a single byte RST call. To get around this
          ; increment the address on the stack.
          PUSH   DE
          INC    HL
          LD     E,(HL)
          INC    HL
          LD     D,(HL)
          INC    DE
          LD     (HL),D
          DEC    HL
          LD     (HL),E

          LD     HL,(APP_STK)
          INC    HL
          INC    HL             ; HL now has the old value of the stack pointer
          LD     (R_SP),HL

          ; Save the value of A into application space
          LD    A,(PAGE_MP)        ; Application page 0 that includes shadow registers
          OUT   (PG_PORT0+1)       ; Mapped into $4000+ (bank 1)
          PUSH   DE
          POP    AF
          LD     (R_AF_S+$4001),A  ; This is where the BP code expects to find old A
          POP    DE             ; All registers EXCEPT SP restored to values before ISR call
          POP    HL             ; Unused interim AF value from the ISR
          POP    HL             ; Saved version of HL from the application
          ; Everything now is back where we need it to be so can do standard break point processing
          JR     _cnt_bp

; ------------------- DO_BP
; This is the target of the BP handler. We're operating in supervisor mode. The following
; registers have been saved in shadow memory and need to be copied to our OS copy:
;     R_PC_S, R_SP_S, R_HL_S, R_AF_S
; All other registers need to be saved. Interrupts are currently DISABLED.
DO_BP:    LD    (R_SP),SP      ; Save registers so we can do some real work
_cnt_bp:  LD    SP,R_IY+2      ; Now push everything we have into temp store.
          PUSH  IY
          PUSH  IX
          PUSH  HL
          PUSH  DE
          PUSH  BC
          EXX
          PUSH  HL
          PUSH  DE
          PUSH  BC
          EXX
          EX    AF,AF'
          PUSH  AF
          EX    AF,AF'

          ; Everything saved except AF but flags not yet changed. A is stored in application space though
          LD    A,(PAGE_MP)        ; Application page 0 that includes shadow registers
          OUT   (PG_PORT0+1)       ; Mapped into $4000+ (bank 1)
          LD    A,(R_AF_S+$4001)   ; Value we saved earlier
          PUSH  AF

          LD    SP,SP_STK     ; Restore our own SP

          ; Only thing left is the PC. TO get this we need to read the top of the application's stack
          LD    HL,(R_SP)
          CALL  PGMAPX         ; Stack memory now accessible in HL
          LD    E,(HL)
          INC   HL
          LD    D,(HL)         ; DE is now one more than the actual PC address
          DEC   DE             ; To point to the RST reset that got us to the BP
          LD    (R_PC),DE
          LD    HL,(R_SP)
          INC   HL
          INC   HL             ; Effectively pop the RST return address, going to JP to continue
          LD    (R_SP),HL

          ; Disable break processing
          LD    HL,0
          LD    (BRK_HK),HL
          EI

          XOR   A              ; Coming from execution not from a clear BP CLI op
          CALL  SUSPALL        ; Remove all BP from code and SS BPs from table

_bp_nf:   LD    A,(AUTO_STP)
          OR    A
          JR    Z,_bfdo

          ; AUTO_STP is set so do another exec
          XOR   A
          LD    (AUTO_STP),A
          JR    GO            ; Go again

_bfdo:    EX    DE,HL         ; Put the current address into HL so we can display it
          XOR   A             ; Use application space memory
          CALL  DECINST       ; Display the next instruction
          CALL  DO_RGS
          LD    HL,(STP_CNT)  ; Any outstanding step count?
          DEC   HL
          LD    A,L
          OR    H
          JR    Z,main
          LD    (STP_CNT),HL

          ; Step again
          LD    A,(STP_IN)
          CALL  SSTEP_BP    ; Set single step BP then go
          JR    DO_GO

; REST_RGS - HL will contain the PC
REST_RGS: LD   HL,(R_AF)
          PUSH HL
          POP  AF
          LD   BC,(R_BC)
          LD   DE,(R_DE)
          LD   HL,(R_HL)
          LD   IX,(R_IX)
          LD   IY,(R_IY)
          RET

;
; ------------------- BP: Set breakpoint
; Get address from command line. If not specfied then BP at the current PC value
BP:       CALL  WASTESPC
          JR    Z,_LISTBP
          LD    E,1
          CP    '-'              ; Removing a breakpoint
          JR    NZ,_gadd
          CALL  BUFCHR           ; Waste the '-'
          DEC   E
_gadd:    CALL  GET_HEX          ; Address for breakpoint
          JR    Z, BADPS

          ; Have an address. Set or clear a BL
          DEC   E

          JR    Z,_doadbp

          ; Remove BP at address specified
          EX    DE,HL
          CALL  FINDBP

          JR    Z,_LISTBP
          LD    (HL),0           ; Cleared
          JR    _LISTBP

          ; Set a BP at the address in HL
_doadbp:  EX    DE,HL
          CALL  FINDBP           ; Already have a BP at this location?
          JR    NZ,_LISTBP
          EX    DE,HL
          LD    A,2              ; BP type
          CALL  SETBP
          ; Drop through and display set breakpoints
_LISTBP:  LD    HL,BPOINTS
          LD    B,NUM_BK
_lnxt:    PUSH  HL
          LD    A,(HL)
          OR    A
          JR    Z,_lemp
          EX    DE,HL
          DEC   A
          JR    NZ,_lprm
          LD    HL,_TMPBC
          JR    _ladd
_lprm:    LD    HL,_PRMBC
_ladd:    CALL  PRINT
          EX    DE,HL
          INC   HL
          LD    E,(HL)
          INC   HL
          LD    D,(HL)
          EX    DE,HL
          CALL  WRITE_16
          CALL  NL
_lemp:    POP   HL
          INC   HL
          INC   HL
          INC   HL
          INC   HL
          DJNZ  _lnxt
          JR    main

; ------- INSTDRV
; Check whether drivers are installed. If not check if they should be installed. Then intall them if reqjuired.
INSTDRV:  LD    A,(PAGE_MP)          ; Application page 0
          OUT   (PG_PORT0+1),A       ; Temp map to page 1 to install drivers

          ; This will check whether the drivers are installed BUT this causes problems on a reload
          ; or if memory is filled. To avoid this always install the drivers.
          ;LD    A,(INITD)
          ;OR    A
          ;RET   NZ      ; Already loaded

          ;INC   A
          ;LD    (INITD),A

          ; Check configuration to see whether we're meant to be installing drivers
if IS_DEVEL
          LD     A,1            ; For driver load for debug version
else
          LD     A,(NVRAM)
endif
          RRCA
          RET    NC

          ; LD    HL,_wrdrv
          ; CALL  PRINT_LN

          ; Install the break driver
          LD    HL,BRK_HDLR  ; Our break handler - was DO_BP_S but that's wrong now. Will need to get there at some point though
          LD    (BRK_HK),HL

          ; Copy my page zero data EXCEPT the first 8 bytes to application space
          LD    HL,$0008
          LD    DE,$4008
          LD    BC,END_RES-08h   ; Page zero
          LDIR

          ; And FORCE the CONTXT value to be 1 to identify application context
          LD   A,1
          LD   (CONTXT+$4000),A

          RET

; ------------------- RUN
; This initial version loads the first four RAM pages into the four banks, reserving the last 16 bytes of
; page 03 to store an address switcher. Once switch it jumps to address 0 to run that code.
RUN:      DI
          CALL  WASTESPC
          CALL  GET_HEX    ; Get optional execution address into HL
          JR    NZ,PREPRUN
          LD    HL,0

PREPRUN:; Map application page 0 into block 1 so we can initialise the reserved space.
          PUSH  HL
          CALL  INSTDRV
          POP   HL
          LD    A,L
          OR    H
          JR    Z,_RMODE       ; Run from zero if not specified

          ; Specific address so add a JP operation at address zero
          LD    A,$F3          ; DI
          LD    ($4000),A
          LD    A,$C3          ; JP
          LD    ($4001),A
          LD    ($4002),HL     ; Start address

_RMODE:   CALL  PAUSE          ; Let the UART complete sending
          LD    A,(NVRAM)
          AND   1
          JR    NZ,_RUN        ; Drivers installed so don't need to jump through hoops

          ; Copy loader code to the end of our page
          LD    DE,$3fff
          LD    HL,_end_ldr-1
          LD    BC,_end_ldr - _st_ldr +1
          LDDR

          ; Map ourselves into the last available page slot
          LD    A,(MON_MP)     ; My page
          OUT   (PG_PORT0+3),A ; Into last bank
          JR    $0000 - ( _end_ldr - _st_ldr )

_st_ldr:  DI
          LD     HL,PAGE_MP+$C000
          LD     C,PG_PORT0
          LD     B,3
_sp5:     LD     A,(HL)
          OUT    (C),A
          INC    C
          INC    HL
          DJNZ   _sp5
          ; Should be right at the end of memory. Last operation is to map the last application page
; CALL  _wait
          LD     A,(HL)
          OUT    (C),A
          ; And drop off the end of memory to address 0 and execute.
_end_ldr  EQU   $




SHOWBCNT:  PUSH  HL
           PUSH  AF
           LD    HL,_nxtblk$
           CALL  PRINT
           LD    HL,(LOAD_CNT)
           INC   HL
           LD    (LOAD_CNT),HL
           CALL  WRITE_16
           POP   AF
           POP   HL
           RET


; Record format
; All records are 32 bytes and the 512 byte first block contains 15 records
; 00 (1): TYPE              : 0: unused, 1: used
; 01 (1): ID                : Numeric ID for this image. Must be unique and non-zero.
; 02 (8): NAME              : Printable
; 0A (4): SD_ADDR           : Byte offset into SD Card (SD card address)
; 0E (2): LOAD_ADD          : Address in application space to load this image
; 10 (2): LENGTH            : Number of bytes to load
; 12 (2): EXEC_ADDR         : Once loaded, execure from this address
; 14 (1): FLAGS             : Bit 0. If bit 0 set then load libraries, otherwise DON'T
_dumpbs:  ; List all known boot sectors
          LD    HL,_SDINFO
          CALL  PRINT_LN
          LD    HL,SDPAGE
          LD    B,15
_nxsec:   PUSH  BC
          PUSH  HL
          LD    A,(HL)      ; Record type.
          DEC   A
          JR    NZ,_sksec
          DEC   A           ; Only understand record type 1
          JR    Z,_sksec
          INC   HL
          LD    A,(HL)      ; Unique image ID. Must be non-zero
          OR    A
          JR    Z,_sksec
          INC   HL

          ; Usable to display
          CALL  WRITE_8
          WRITE_CHR ' '
          LD    B,8         ; 8 byte name
_nxc:     LD    A,(HL)
          INC   HL
          OR    A
          JR    NZ,_noch    ; Zero bytes as a spacer
          LD    A,' '
_noch:    RST   08H
          DJNZ  _nxc
          WRITE_CHR ' '
          ; SD Card Address is 4 bytes (32 bits) LSW first so need to reverse
          LD    E,(HL)
          INC   HL
          LD    D,(HL)
          INC   HL
          PUSH  DE
          LD    E,(HL)
          INC   HL
          LD    D,(HL)
          INC   HL
          EX    DE,HL
          CALL  WRITE_16
          POP   HL
          CALL  WRITE_16
          EX    DE,HL
          LD    B,3            ; Write next three 16 bit values
_nxsd:    WRITE_CHR ' '
          LD    E,(HL)
          INC   HL
          LD    D,(HL)
          INC   HL
          EX    DE,HL
          CALL  WRITE_16
          EX    DE,HL
          DJNZ  _nxsd

          WRITE_CHR ' '       ; And display the boot flags
          LD    A,(HL)
          CALL  WRITE_8

          CALL  NL
_sksec:   POP   HL
          LD    A,32
          ADD   L
          LD    L,A
          LD    A,0
          ADC   A,H
          LD    H,A
          POP   BC
          DJNZ  _nxsec

          JR    main

SDPREP:   ; Always need to load the first 512 byte block that includes the boot information
          LD    HL,0          ; Block 0
          LD    DE,0
          LD    BC,SDPAGE
          CALL  SD_RBLK
          JR    _calc_cs      ; Jump, save the RET byte
          ; XOR   A
          ; RET

SDLOAD:   ; SD Card Load. Two options:
          ; No parameters - Load OS type 01 - which is CP/M
          ; With a digit (0-9) look for and load a specific type
          ; With '?' - display all available options and exit
          CALL  SDPREP

          ; Default to auto-run
          LD    A,1
          LD    (AUTO_RUN),A

          ; Calculate the checksum
          JR    NZ,_bad_cs

          CALL  SKIPSPC        ; Next character to decide what to do
          JR    Z,_ld01        ; Boot OS with ID 01
          CP    '?'
          JR    Z,_dumpbs
          CP    '-'
          JR    NZ,_sdauto
          XOR   A
          LD    (AUTO_RUN),A   ; Auto-run off

          CALL  SKIPSPC        ; Next character to decide what to do
          JR    Z,_ld01        ; Boot OS with ID 01

_sdauto:  SUB   '0'
          JR    C,main
          CP    9
          JR    NC,main
          JR    _bsload

          ; Only get here as a result of a Z test so A=0. Want 1.
_ld01:    LD    A,1

_bsload:  LD    C,A            ; The load ID we're looking for
          LD     HL,_BSD
          CALL  PRINT

          ; Find the requested boot block
          LD    HL,SDPAGE
          LD    B,16

          ; Check this block - should countain 01 if it's usable
_again:   LD    A,(HL)
          DEC   A
          JR    NZ,_nxtbs

          INC   HL             ; Usable block - is this the OS we want
          LD    A,(HL)
          DEC   HL
          CP    C              ; The one we want?
          JR    Z,_doload      ; Yes. HL points to start of block descriptor
_nxtbs:   LD    A,32
          ADD   L
          LD    L,A
          LD    A,0
          ADC   H
          LD    H,A
          DJNZ  _again

          ; Not found
          LD    HL,_ERRDNM
          JR    _prterr

          ; Found the one we want so load
_doload:  INC   HL
          INC   HL

          ; At start of name. Print name as we scan past it.
          LD    B,8
_nchr:    LD    A,(HL)
          INC   HL
          OR    A
          JR    Z,_na
          CALL  TXA
_na:      DJNZ  _nchr
          LD    A,' '
          RST   08h

          ; Rest of the entry:
          ; 4 byte SD card block address
          ; 2 byte load address
          ; 2 byte length
          ; 2 byte exec address
          ; 1 byte flags
          LD    E,(HL)
          INC   HL
          LD    D,(HL)
          INC   HL
          LD    (SDADDR), DE

          LD    E,(HL)
          INC   HL
          LD    D,(HL)
          INC   HL
          LD    (SDADDR+2), DE

          LD    E,(HL)
          INC   HL
          LD    D,(HL)
          INC   HL
          LD    (LOADADD),DE

          PUSH  HL
          INC   HL            ; Step over the length to store the exec address
          INC   HL
          LD    E,(HL)
          INC   HL
          LD    D,(HL)
          LD    (EXECADD),DE
          INC   HL
          LD    A,(NVRAM)     ; Flag byte
          AND   $FE           ; Only flag we use
          OR    A,(HL)        ; Execution flags
          LD    (NVRAM),A
          POP   HL

          ; Need to work out how many whole blocks we need to load
          LD    E,(HL)        ; DE is the length
          INC   HL
          LD    D,(HL)

          ; Number of whole 512 byte blocks
          LD    A,D
          OR    A
          RRA
          LD    (FULBLKS),A

          ; Need a partial block at the end?
          LD     A,D
          AND    1
          LD     D,A
          LD     (LASTB),DE

          ; Start loading blocks.
_bxtbl:   LD     HL,(LOADADD)         ; Current load address. Need to now translate into a valid page address
          CALL   PGMAP                ; Select page and adjust. HL is now the address we want to load into
          LD     B,H
          LD     C,L                  ; Move to BC (DMA address)

          LD     DE,(SDADDR)
          LD     HL,(SDADDR+2)
          CALL   SD_RBLK              ; Load the next page in to RAM

          ; Increase SD address and memory address by 512
          LD     HL,(LOADADD)
          LD     DE,512
          ADD    HL,DE
          LD     (LOADADD),HL

          LD     HL,(SDADDR)
          INC    H
          INC    H
          LD     (SDADDR),HL
          LD     HL,(SDADDR+2)
          LD     A,0
          ADC    A,L
          LD     L,A
          LD     A,0
          ADC    A,H
          LD     H,A
          LD     (SDADDR+2),HL

          ; Any more full blocks to load?
          LD     A,(FULBLKS)
          DEC    A
          LD     (FULBLKS),A
          JR     NZ,_bxtbl

          ; Full blocks loaded and addresses set up for any partial last block
          LD     DE,(LASTB)
          LD     A,E
          OR     D
          JR     Z,_exec

          ; DE contains number of bytes from last partial block. Load block into standard buffer and use from there.
          LD     BC,SDPAGE
          LD     DE,(SDADDR)
          LD     HL,(SDADDR+2)
          CALL   SD_RBLK              ; Load the next page in to RAM

          ; Copy to destination
          LD     HL,(LOADADD)         ; Current load address. Need to know translate into a valid page address
          CALL   PGMAP                ; HL contains the target address

          LD     DE,SDPAGE            ; The partial block
          LD     BC,(LASTB)           ; Number of bytes
          EX     DE,HL
          LDIR                        ; Copy

_exec:    ; Load complete, auto-execute?
          LD     HL,(EXECADD)
          LD    A,(AUTO_RUN)
          OR    A
          JR    NZ,_doexec
          ; Don't auto execute, just display the load address
          EX    DE,HL
          LD    HL,_execad
          CALL  PRINT
          EX    DE,HL
          CALL  WRITE_16
          CALL  NL
          JR    main

          ; Set up pages and execute.
_doexec:  LD     A,H
          OR     L
          JR     Z,PREPRUN


          ; And run from EXECADD
          LD     HL,(EXECADD)
          JR     PREPRUN

_bad_cs:  LD    HL,_badbs
          JR    _prterr

; ----- SETBP - set breakpoint
; HL: The application space address at which to set a BP
; A:  The type code for the BP. Must be >0.
;   1: single step BP (reserved)
;   2: standard BP - permanent until cleared
; Find an available breakpoint slot.
SETBP:    PUSH  BC
          PUSH  DE
          PUSH  HL
          LD    C,A             ; A holds the BP type to set - need to keep this

          ; Need to get the physical memory/address for the BP
          LD    D,H             ; Save the application space address
          LD    E,L
          CALL  PGMAPX          ; A: page number, HL: mapped address
          LD    A,(HL)          ; If there's already a BP at this location do nothing
          CP    BRK_OPCODE
          JR    Z,_dupbp
          LD    HL,BPOINTS      ; Search the BP table for a free slot
          LD    B,NUM_BK        ; Size of BP table
nextbp:   LD    A,(HL)
          OR    A
          JR    Z,fndbp         ; If this is Z then it's a free BP slow and it can be used
          INC   HL              ; N ot found so step over this entry (4 bytes)
          INC   HL
          INC   HL
          INC   HL
          DJNZ  nextbp
          ; If we get here then can't set a BP - error. Discard stack.
          LD    HL,_nobpavail
          JR    _prterr
fndbp:    LD    (HL),C          ; Save the BP type
          INC   HL
          LD    (HL),E           ; Next 2 bytes are the address at which thi BP is set
          INC   HL
          LD    (HL),D
          INC   HL               ; HL now points at the place to save the breakpoint

          ; If 'type' is 1 then set the BP. Any other value and the BP
          ; is set at runtime
          DEC   C
          JR    NZ,_dupbp
          ; Inefficient, but not time critical. Convert BP address back to physical address
          EX    DE,HL
          CALL  PGMAPX           ; A: page number, HL: mapped address
          LD    A,(HL)           ; HL is the address at which we're setting the BP. We need the op-code stored there (in A)
          LD    (DE),A           ; And store that in the BP record
          LD    (HL),BRK_OPCODE  ; Replace programme opcode with our break point opcode.
_dupbp:   POP   HL
          POP   DE
          POP   BC
          RET

; ------ FINDBP - Find the BP at the address pointed to by HL. Result returned in DE.
; DE - Address (in application space) of BP (IN)
; HL - Address of matching BP record if found (OUT)
; Z  - Set if BP not found
FINDBP:   PUSH  BC
          LD    HL,BPOINTS
          LD    B,NUM_BK   ; Step through the breakpoint table
_chknxt:  LD    A,(HL)
          OR    A          ; Unused slot if this is zero
          JR    Z,_nf1
          INC   HL         ; Next 2 bytes are the address
          LD    A,(HL)
          INC   HL
          CP    E
          JR    NZ,_nf2
          LD    A,(HL)
          CP    D
          JR    NZ,_nf2
          ; Found...
          DEC   HL
          DEC   HL         ; Set HL to point back to the start of the record.
          OR    1          ; Clear Z flag
          JR    _fnd1

_nf1:     INC   HL
          INC   HL
_nf2      INC   HL
          INC   HL
          DJNZ  _chknxt
          ; If we get here then there's no matching BP
_fnd1     POP   BC
          RET

; ------ INSTBP
; Install ALL permanent breakpoints prior to running the code. If a permanent break point
; is being set at the current PC location then it's skipped. All BP addresses are in the application
; space 64K range so need to be mapped.
; A:  0 - set the breakpoints
; A: !0 - remove the breakpoints
; E:  If non-zero then transmute into a single step if a BP points to the current PC and set AUTO_RN
INSTBP:   LD    C,A
          LD    A,E
          LD    (BPCT),A                ; Save the call type
          LD    HL,BPOINTS
          LD    B,NUM_BK
_inextbp: LD    A,(HL)                  ; Does this slot contain an active BP?
          CP    2                       ; Only interested in permanent BPs (type 2)
          CALL  Z,_insbp
          INC   HL                      ; Move on to the next one
          INC   HL
          INC   HL
          INC   HL
          DJNZ  _inextbp
          RET                           ; Can't swt a BP here

_insbp:   PUSH  HL                      ; Store a BP at this location.
          INC   HL                      ; Address HL points to contains the BP address
          LD    E,(HL)
          INC   HL
          LD    D,(HL)                  ; DE is the address in code to set the BP. Need to conv. to local space
          INC   HL                      ; HL points to where to store the code we replace
          LD    A,C                     ; Flag - NZ: remove, Z: set
          OR    A
          JR    NZ,_insrm
          ; Check whether this BP is at the current PC location
          LD    A,(R_PC)
          CP    E
          JR    NZ,_insok
          LD    A,(R_PC+1)
          CP    D
          JR    NZ,_insok
          ; Can't directly set a BP at the current PC location. Instead turn this into a single step and
          ; then a run
          LD    A,(BPCT)                ; GET the call type
          CP    2
          JR    NZ,_iskip               ; It was a normal 'go' request and there's a BP at the current address
          LD    (AUTO_STP),A            ; Force autorun
          JR    _iskip

_insok:   EX    DE,HL                   ; Needs to be set so...
          CALL  PGMAPX                  ; ...Convert and map...
          LD    A,(HL)                  ; BP already installed?
          CP    BRK_OPCODE
          JR    Z,_iskip
          LD    (DE),A                  ; Store the byte we're replacing with the BP opcode
          LD    (HL),BRK_OPCODE         ; Installed. Don't need to BP address any longer
_iskip:   POP   HL
          RET
_insrm:   EX    DE,HL                   ; Needs to be removed so...
          CALL  PGMAPX                  ; ...Convert and map...
          LD    A,(HL)
          CP    BRK_OPCODE
          JR    NZ,_noclr
          LD    A,(DE)      ; The opcode that needs to be reset into the code
          LD    (HL),A
_noclr:   POP   HL
          RET




; ------ CLRBP
; DE: Address in code where a BP is set.
CLRBP:    CALL  FINDBP
          RET   Z              ; No BP known at that address
CLRBP2:   PUSH  HL             ; HL: Points to the BP record
          PUSH  DE             ; DE: address of the BP in code
          LD    A,(HL)         ; The type of the BP
          DEC   A              ; Type 1 means clear all of this type (single step)
          JR    Z,_clrsstp     ; Just a single break point to clear
          CALL  _clrbp
          JR    _cbpfin
_clrsstp: ; Find all breakpoints type 1 and clear
          LD    HL,BPOINTS
          LD    B,NUM_BK
_nextbp:  LD    A,(HL)
          DEC   A
          CALL  Z,_clrbp
          ; Not available so skip
          INC   HL
          INC   HL
          INC   HL
          INC   HL
          DJNZ  _nextbp
_cbpfin:  POP   DE
          POP   HL
          RET

; ------ SUSPALL
; Suspend breakpoint all breakpoint entries. Single step BP table entries are
; removed. Permanent BPs are left in the table.
SUSPALL:  LD    HL,BPOINTS
          LD    B,NUM_BK
_rmnxt:   LD    A,(HL)
          OR    A
          CALL  NZ,SUSBP
          ; Skip to next
          INC   HL
          INC   HL
          INC   HL
          INC   HL
          DJNZ  _rmnxt
          RET

; ------ SUSBP
; Suspend breakpoint. Remove from code but not from the BP table.
; HL: Address of the BP record.
SUSBP:    PUSH  HL
          PUSH  DE
          LD    A,(HL)
          DEC   A
          JR    NZ,_prm
          LD    (HL),A
_prm:     INC   HL
          LD    E,(HL)
          INC   HL
          LD    D,(HL)
          INC   HL          ; And the last byte is the one we replaced with the BP trigger RST opcode
          EX    DE,HL       ; HL now contains the address in application space of the BP
          CALL  PGMAPX      ; Convert app space to physical space
          LD    A,(HL)      ; Byte stored at the application address
          CP    BRK_OPCODE  ; It *should* be the BP opcode if it's already installed
          JR    NZ,_nobp    ; It's not so nothing to clear
          LD    A,(DE)      ; Get the opcode to be restored.
          LD    (HL),A      ; Restore the byte we overwrote
_nobp:    POP   DE
          POP   HL
          RET


_clrbp:   LD    (HL),0      ; Clear the 'type' field to free this slot
          JR    SUSBP

; ----- Step Over
NSTEP:    LD    HL,1
          LD    (STP_CNT),HL   ; By default step once
          LD    E,0          ; In E
          CALL  SSTEP_BP
          JR    DO_GO

; ----- SHWHIST
; Display history with index rows
SHWHIST:  LD    B,$10
_nhist:   LD    A,B
          CALL  GETHIST
          JR    Z,_shist      ; Nothing in that slot

          ; Got a history line.
          CALL  PRINT_LN
_shist:   DJNZ  _nhist
          JR    main


; ----- CLS
; Clear screen. If in debug mode then redraw registers etc.
CLS:      LD    HL,_CLRSCR
          CALL  PRINT
          LD    A,(OPMODE)
          RRCA
          JR    C,main      ; Standard mode so nothing more to do
          JR    _fcdb

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




; ------------ PAGE
; Display current application pages or change a page number
; Changing is in the form blknum=pagenum. Eg 3=21
PAGE:     CALL  SKIPSPC
          JR    Z,_shpg
          SUB   '0'
          JR    C,_shpg
          CP    4
          JR    NC,_shpg
          ; Calculate PAGE_MP position
          LD    HL,PAGE_MP
          ADD   L
          LD    L,A
          CALL  SKIPSPC
          CP    '='
          JR    NZ,_shpg
          CALL  INHEX_2
          JR    C,_shpg

          LD    (HL),A
          ; Drop through and display current settings.

_shpg:    LD    HL, _APPPG
          CALL  PRINT
          LD    HL,PAGE_MP
          LD    B,4
_npd:     LD    A,(HL)
          CALL  WRITE_8
          WRITE_CHR ' '
          INC   HL
          DJNZ  _npd
          CALL  NL
          JR    main

; Z flag is the state of the flag. HL points to the name
_say:     PUSH  AF
          PUSH  HL
          JR    NZ,_sayyes
          LD    HL,_no
          JR    _saynow
_sayyes:  LD    HL,_yes
_saynow:  EX    (SP),HL
          CALL  PRINT
          LD    HL,COLSTR
          CALL  PRINT
          POP   HL
          CALL  PRINT
          CALL  NL
          POP   AF
          RET

; ------------ CONFIG
; Show or set a configuration options. All are boolean at the moment. Currently
; only using the first byte as 8 boolean flags.
; Bit 0:  Default for 'BOOT' - DON'T install page 0 drivers (image is stand alone)
CONFIG:   CALL   WASTESPC
          JR     Z,_cfshw

          ; Expect a hex(ish) number for the parameter ID
          CALL   GET_HEX
          JR     Z,BADPS
          LD     A,L
          LD     HL,CFG_TAB
          JR     C,_cfshw

          ; Find the parameter ID
          OR     A
          JR     Z,_cfset
          LD     B,A             ; B = config param ID
_cfgsn:   LD     A,(HL)          ; Bit mask
          OR     A
          JR     Z,BADPS         ; If zero then reached end of table
_cfcnxt:  INC    HL
          INC    HL
          CALL   _skpstr
          DJNZ   _cfgsn
_cfset:   LD     A,(HL)
          OR     A
          JR     Z,BADPS
          LD     C,A             ; Save mask for later

          INC    HL
          LD     B,(HL)          ; Byte offset
          INC    HL              ; Points to description/name
          EX     DE,HL           ; Save description for later in DE

          ; Calculate the address of the config byte into HL
          LD     HL,NVRAM
          LD     A,B
          CALL   _ADD16

          LD     A,C             ; The mask

          CPL                    ; Invert
          AND    (HL)            ; Current value with flag cleared
          LD     B,A             ; Save the cleared config parameter

          ; And decide whether we need to set it.
          ; Get the new setting from the user
          CALL   SKIPSPC
          CP     '='
          JR     NZ,BADPS
          CALL   SKIPSPC
          ; RST    08h
          CP     'N'
          JR     Z,_cfsv           ; Save the cleared value
          CP     '0'
          JR     Z,_cfsv           ; Save the cleared value
          ; Any other value and we have to set the flag.
          LD     A,C               ; The original mask
          OR     B                 ; Set the bit
          LD     B,A
_cfsv:    LD     A,B
          LD     (HL),B

          ; Print description
          AND    C                 ; test the flag just configured
          EX     DE,HL
          CALL   _say
          CALL   NVSAV
          JR     main

_cfshw:   LD     HL,CFG_TAB
          LD     B,0             ; Config index
_cfnc:    LD     A,(HL)
          OR     A
          JR     Z,main          ; End of table

          LD     C,A             ; Save mask for later
          ; Output the config parameter number (used for setting)
          LD     A,B
          CALL   WRITE_8
          WRITE_CHR ' '

          ; A contains the config bit mask
          INC    HL
          LD     A,(HL)          ; The byte offset
          EX     DE,HL
          LD     HL,NVRAM
          CALL   _ADD16
          LD     A,(HL)          ; The byte containing the flag
          EX     DE,HL           ; HL back to the table
          INC    HL              ; points to the description
          AND    C               ; Mask to the relevant bit
          PUSH   HL
          CALL   _say
          POP    HL
          ; Then step HL past the description to the next config entry
          CALL   _skpstr

          ; Step one more to the start of the next config entry
          INC    B               ; Next command ID
          JR     _cfnc

; --------- _skpstr
; Step over a null terminated string and return HL pointing to the first byte after the null
_skpstr:  XOR    A
_cfnch:   INC    HL
          CP     (HL)
          JR     NZ,_cfnch
          INC    HL          ; Step past null
          RET

; ------------ _PGCALC
; Take a 16 bit address in application space and translate that into a block number (0-3)
; and an offset.
; HL - load address. Translate into a page and offset return the offset in HL
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

; ------------ PGADJ
; Take a 16 bit address in application space and translate that into a page number and an offset.
; HL - load address. Translate in a page and offset then load that page into board page 1 and
;      return the offset in HL
; Return:
; A  - The actual physical page number to be mapped
; HL - The adjusted offset into that page
; DE - WORKING SPACE, NOT SAVED!!
PGADJ:    CALL  _PGCALC

          ; A is the logical block number in application space. Translate that in a RAM
          ; page number.
          LD    DE,PAGE_MP
          ADD   E
          LD    E,A      ; Monitor memory is linked from 2000h so won't cross a 256 byte boundary.
          LD    A,(DE)   ; DE is now the address of the PAGE_MP for the addressed page in application space
          RET


; ------------- PGRESTX
; Restore page 1 and 2 to the application space. Used after extended instructions that have to map
; two pages to deal with page boundarys.
PGRESTX:  LD    A,(PAGE_MP+2)
          OUT   (PG_PORT0+2),A

; ------------- PGREST
; Restore page 1 to the application space.
PGREST:   LD    A,(PAGE_MP+1)
          OUT   (PG_PORT0+1),A
          RET

; HL - load address. Translate in a page and offset then load that page into board page 1 and
;      return the offset in HL
; A returns the page mapped.
PGMAP:    PUSH  DE
          CALL  PGADJ

          ; And map into page 1 memory
          OUT   (PG_PORT0+1),A

          POP   DE
          RET

; ---- PGMAPX
; Same as PGMAP but also places the *next* application space page into bank 2. Use this if
; operations could cross a 16K page boundary.
PGMAPX:   PUSH  DE
          CALL  _PGCALC         ; HL: Adjusted address, A the 16K application block number to map (0-3)
          PUSH  AF
          LD    DE,PAGE_MP
          ADD   E
          LD    E,A      ; Monitor memory is linked from 2000h so won't cross a 256 byte boundary.
          LD    A,(DE)   ; DE is now the address of the PAGE_MP for the addressed page in application space
          OUT   (PG_PORT0+1),A
          INC   DE
          LD    A,(DE)
          OUT   (PG_PORT0+2),A
          POP   AF        ; AF includes the logical bock number (0-3)
          POP   DE
          RET

; Calculate checksum. Calculated over 15 slots using a simple shift algorithm Result 16 bits.
; Compare calculated value with existing value and set Z if they are the same.
_calc_cs: LD    HL,0         ; the calculated partial CS
          LD    DE,SDPAGE    ; Byte we're working on
          LD    B,240        ; Number of 16 bit words to calculate sum over

_nxt_wd:  ; Add in the next 16 buts
          LD    A,(DE)
          ADD   L
          LD    L,A
          INC   DE
          LD    A,(DE)
          ADC   H
          LD    H,A
          INC   DE
          DJNZ  _nxt_wd

          ; HL is the new checksum. Compare it to the current value
          ; and set the Z flag if it matches.
          INC   DE         ; Skip end entry marker
          LD    A,(DE)
          CP    L
          RET   NZ
          INC   DE
          LD    A,(DE)
          CP    H
          RET

; ------ _ADD16
; Add A to HL and correctly deal with carry from L to H. HL and A changed.
_ADD16:   ADD   L
          LD    L,A
          LD    A,0
          ADC   H
          LD    H,A
          RET

; ------- _NVC
; Calculate the NVRAM checksum. 1s complement of the sum of the first 15 bytes
; Return: HL - Points to the first byte after the data
;         A  - Calculate checksum
_NVC:     PUSH  BC
          LD    HL,NVRAM
          LD    B,15
          LD    C,0
_nvnx:    LD    A,(HL)
          INC   HL
          CPL
          ADD   C
          LD    C,A
          DJNZ  _nvnx
          POP   BC
          RET

; ------- NVCHK
; Checksum the first 15 bytes of the NV memory and compare with the check sum in the last byte. Calculated on
; the 1s complement of each data byte.
; Return: A Calculate checksum
;         Z is set on exit of the checksum matches the stored value.
NVCHK:   CALL   _NVC
         CP     (HL)
         RET

; ------- NVCALC
; Calculate and store the checksum for the current NVRAM data
NVCALC:  CALL   _NVC
         LD     (HL),A
         RET

; ------- NVINI
; Initialise the NVRAM structure
NVINI:   LD      HL,NVRAM
         LD      (HL),CFG_DEF
         INC     HL
         LD      B,14
         XOR     A
_nvcl:   LD      (HL),A
         INC     Hl
         DJNZ    _nvcl
         JR      NVSAV

; ------- NVLD
; Load NVRAM and validate. If not valid then initialise
NVLD:     LD     HL,NVRAM
          LD     BC,1000h    ; Read 16 bytes from offset 0
          CALL   RTC_MRD

          ; Check chksum
          CALL   NVCHK
          CALL   NZ,NVINI
          RET

; ------- NVSAV
; Save the content of the NVRAM area to the RTC
NVSAV:   CALL   NVCALC
         LD     HL,NVRAM
         LD     BC,1000h    ; WRITE 16 bytes from offset 0
         CALL   RTC_MWR
         RET


; ------- MAPDSK
; Either:
;    S LL=PPPP - Map Physical drive PPPP to logical drive LL
;    S         - Display current mapped drives
MAPDSK: CALL  SKIPSPC
        JR    Z, _dmapd
        CP    'D'
        JR    Z, SDUMP

_dmapd: LD    B,16
        LD    HL,DRVMAP
        LD    A,0
_dmapn: PUSH  AF
        CALL  WRITE_8
        LD    A,'='
        CALL  TXA
        LD    E,(HL)
        INC   HL
        LD    D,(HL)
        INC   HL
        EX    DE,HL
        CALL  WRITE_16
        EX    DE,HL
        CALL  NL
        POP   AF
        INC   A
        DJNZ  _dmapn
        JR    main


_sdbad: LD     HL,_NOSDADD
        JR     _prterr


; -------- SDUMP
; Dump the contents of a 512 byte SD card sector. The sector number is (potentially) 32 bits
; Command format is SD 0000:0000. Also accept 0000 for one of the lowest 64K sectors
SDUMP:   CALL  SD_INIT
         CALL  WASTESPC
         JR    Z,_sdbad

         ; Expect a 32 bit number
         CALL  INHEX

         JR    C,_sdbad

         ; HLDE is the full SD card BLOCK address. Need to
         ; multiple by 512 to get a byte address for the
         ; SDcard interface.

_sdump:  LD    H,L     ; Multiple by 256
         LD    L,D
         LD    D,E
         LD    E,0

         SLA   D       ; And then by 2 to get to 512 block address
         RL    L
         RL    H

         PUSH  DE
         PUSH  HL
         LD    HL,_BTADD
         CALL  PRINT
         POP   HL
         PUSH  HL
         CALL  WRITE_16
         WRITE_CHR ':'
         LD    H,D
         LD    L,E
         CALL  WRITE_16
         POP   HL
         POP   DE

         LD    BC,SDPAGE  ; Use our default buffer for the input
         CALL  SD_RBLK
         ; And dump the read block.
         CALL  NL
         ; 512 bytes to dump in blocks of 32 bytes
         LD    HL,0       ; The block offset to display
         LD    DE,SDPAGE  ; Source for data
         LD    B,16       ; Row count
_sdnxt:  ; Display a row
         PUSH  BC
         CALL  WRITE_16   ; The offset
         WRITE_CHR SPC
         LD    B,16
_sdbnx1: LD    A,(DE)
         CALL  WRITE_8
         INC   DE
         WRITE_CHR SPC
         DJNZ  _sdbnx1
         WRITE_CHR SPC

         LD    B,16
_sdbnx2: LD    A,(DE)
         CALL  WRITE_8
         INC   DE
         WRITE_CHR SPC
         DJNZ  _sdbnx2
         CALL  NL

         LD    A,32
         ADD   L
         LD    L,A
         JR    NC,_sdnx3
         INC   H
_sdnx3:  POP   BC
         DJNZ  _sdnxt
         JR    main



; --------------------- SH_DTIME
; Display date/time from RTC
SH_DTIME:   CALL     RTC_INI
            LD       HL,_TIME
            CALL     RTC_GET

            ; Data read from the RTC
            LD       DE,_TIME

            ; Build up the display string
            LD       HL,_FMT+18
            LD       A,(DE)
            AND      7Fh
            CALL     _DWRD

            ; Minutes
            LD       A,(DE)
            CALL     _DWRD

            ; Hours
            LD       A,(DE)
            AND      3Fh
            CALL     _DWRD

            ; Hours
            INC      DE      ; Skip day of week
            LD       A,(DE)
            CALL     _DWRD

            ; Month
            LD       A,(DE)
            CALL     _DWRD

            ; Year
            LD       A,(DE)
            CALL     _DWRD

            LD       HL,_FMT
            CALL     PRINT_LN
            RET

_DWRD:      INC      DE
            CALL     _WRD
            DEC      HL
            RRA
            RRA
            RRA
            RRA
            CALL     _WRD
            DEC      HL
            DEC      HL
            RET


_WRD:       PUSH     AF
            PUSH     DE
            AND      0Fh
            LD       DE,_DIGITS
            ADD      A,E
            LD       E,A
            LD       A,(DE)
            LD       (HL),A
            POP      DE
            POP      AF
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

            LD       A,$FF     ; Where to write the data (Video memory)
            OUT      (PG_PORT0+3), A
            LD       A,CSET_PG ; Where to get the character set data
            OUT      (PG_PORT0+2), A

            LD       BC,$1000  ; The character set is 4K
            LD       HL,$8000  ; Source address
            LD       DE,$C000  ; Desitnation address
            LDIR

            ; Set up test data in video memory
            LD       HL,$E000  ; Start of display memory
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
_INTRO:   DEFB ESC,"[2J",ESC,"[H",ESC,"[J",ESC,"[1;50rZ80 ZIOS 1.17.1",NULL
_CLRSCR:  DEFB ESC,"[2J",ESC,"[1;50r",NULL

; Set scroll area for debug
_DBMD:    DEFB ESC,"[2J",ESC,"[1m",ESC,"[1;6HSTACK",ESC,"[m",ESC,"[11;50r",ESC,"[9;1H  >",ESC,"[12,1H",NULL

_PROMPT:  DEFB "> ",0
_ERROR:   DEFB "unknown",0
_REC_ERR: DEFB CR,LF,"Bad rec",NULL
_WAITING: DEFB "Waiting...",NULL
_COMPLETE:DEFB CR,LF,"Done",0
_IOERR:   DEFB CR,LF,"Param error",0

; VT100 sequences
CURS_UP:   DEFB ESC,"[A",NULL
COLSTR:    DEFB CR,ESC,'[25C',NULL
SAVE_POS:  DEFB ESC,"7",NULL
REST_POS:  DEFB ESC,"8",NULL
HOME:      DEFB ESC,"[H",NULL
STK_NXT:   DEFB CR,LF,ESC,"[3C",NULL ; Down one line then to character 2

_FNAME:      DEFB "File? ",0
_FERR:       DEFB "Doesn't exist",0
_ERRDNM      DEFB CR,LF,"Error",0
_nxtblk$     DEFB CR,"Block: ",0
_badbs       DEFB "Invalid block",0
_BOOTHEX     DEFB "boot.ihx",0
_BSD         DEFB "SDCard Boot: ", 0
_nobpavail:  DEFB "Full",NULL
_execad:     DEFB "Exec addr: ",NULL
_SDINFO:     DEFB "ID NAME     SDADDR   LOAD  LEN EXEC FL",NULL
_APPPG:      DEFB "Application pages: ", NULL
_yes:        DEFB "YES", NULL
_no:         DEFB "NO", NULL
_wrdrv:      DEFB "Installing drivers...", NULL
_NODRV       DEFB "No debug drivers installed", NULL
_TMPBC       DEFB "Temp: @", NULL
_PRMBC       DEFB "BP:   @", NULL
_CMDNM       DEFB "CMD: ", NULL
_DMAP        DEFB "MAPPED: ", NULL
_DMAAD       DEFB "DMA ADDR: ", NULL
_BRK         DEFB "BREAK...", NULL
_NOSDADD     DEFB "No SDcard sector address", NULL
_BTADD       DEFB "Byte address: ", NULL

; Alternate command table format: LETTER:ADDRESS
BDG_TABLE:      DB       'B'
                DW        BP
                DB       'N'
                DW        NSTEP
                DB       'R'
                DW        SET_RGS
                DB       'S'
                DW        SSTEP
                DB       'G'
                DW        GO

CMD_TABLE:      DB       'B'
                DW        BOOT
                DB       'C'
                DW        CONFIG
                DB       'D'
                DW        DUMP
                DB       'F'
                DW        FILL
                DB       'H'
                DW        CLS
                DB       'I'
                DW        INPUT
                DB       'L'
                DW        LOAD
                DB       'M'
                DW        MODIFY
                DB       'N'
                DW        NSTEP
                DB       'O'
                DW        OUTPUT
                DB       'P'
                DW        PAGE              ; Display/change application page assignment
                DB       'Q'
                DW        DECCHR
                DB       'S'
                DW        MAPDSK            ; Map a logical to physical SD card.
                DB       'T'
                DW        EXDBG
                DB       '.'
                DW        SHWHIST
                DB       'G'
                DW        RUN
                DB       '?'
                DW        HELP
                DB        0


; ---------- CFG_TAB
; Set of boolean flags that can be configured in NVRAM. Format is:
; MASK | ByteOffset | Desc | 0
CFG_TAB:        DEFB      00000001b                ; Bit 0
                DEFB      0
_instdrv:       DEFB      "Install OS: ",0

                DEFB      00000010b                ; Bit 1
                DEFB      0
                DEFB      "Autoboot: ",0

                DEFB      0         ; Terminator

R_ADDR_8:  DEFB    'A'
           DEFW    R_AF+1
           DEFB    'B'
           DEFW    R_BC+1
           DEFB    'C'
           DEFW    R_BC
           DEFB    'D'
           DEFW    R_DE+1
           DEFB    'E'
           DEFW    R_DE
           DEFB    'H'
           DEFW    R_HL+1
           DEFB    'L'
           DEFW    R_HL
           DEFB     0
R_ADDR_16: DEFB    "BC"
           DEFW    R_BC
           DEFB    "DE"
           DEFW    R_DE
           DEFB    "HL"
           DEFW    R_HL
           DEFB    "IX"
           DEFW    R_IX
           DEFB    "IY"
           DEFW    R_IY
           DEFB    "PC"
           DEFW    R_PC
           DEFB    "SP"
           DEFW    R_SP
           DEFW    0

; VT100 sequences
FLAGS_DESC:  DEFB "SZ5H3VNC",NULL

; Table driven the register display. A is the only one this doesn't work for
REG_DESC: DEFW    R_PC_DESC
          DEFW    R_SP_DESC
          DEFW    0                ; Step over the two AF values
          DEFW    0
          DEFW    R_BCP_DESC
          DEFW    R_DEP_DESC
          DEFW    R_HLP_DESC
          DEFW    R_BC_DESC
          DEFW    R_DE_DESC
          DEFW    R_HL_DESC
          DEFW    R_IX_DESC
          DEFW    R_IY_DESC

; Register labels
R_PC_DESC:  DEFB ESC,"[11;50r",ESC,"[2;20H  PC: ",NULL
R_SP_DESC:  DEFB ESC,"[3;20H  SP: ",NULL
R_BCP_DESC: DEFB ESC,"[3;60H  BC':",NULL
R_DEP_DESC: DEFB ESC,"[4;60H  DE':",NULL
R_HLP_DESC: DEFB ESC,"[5;60H  HL':",NULL
R_A_DESC:   DEFB ESC,"[2;40H  A:  ",NULL
R_AP_DESC:  DEFB ESC,"[2;60H  A': ",NULL
R_BC_DESC:  DEFB ESC,"[3;40H  BC: ",NULL
R_DE_DESC:  DEFB ESC,"[4;40H  DE: ",NULL
R_HL_DESC:  DEFB ESC,"[5;40H  HL: ",NULL
R_F_DESC:   DEFB ESC,"[6;40H  F:  ",NULL
R_IX_DESC:  DEFB ESC,"[5;20H  IX: ",NULL
R_IY_DESC:  DEFB ESC,"[6;20H  IY: ",NULL
R_WIN_TOP:  DEFB ESC,"[0;12r",NULL
R_WIN_BOT:  DEFB ESC,"[13;50r",NULL

_DIGITS:  DEFB "0123456789"

_FMT:     DEFB "2019-01-01 00:00.00",NULL

; Time format:
; 00: ESSSSSSS - E: 1 to disable clock. S - seconds
; 01: MINS
; 02: 0-X-Y-HRS
;       X: 0 - 12 hour, 1 - 24 hour reply
;       Y: 0 - AM/PM if 12 hour, or MSB of 24 hour clock
; 03: DAY OF WEEK - 1 to 7
; 04: DATE
; 05: MONTH
; 06: YEAR, 0-99
; 07: X00X00XX
;     |  |  ++ -> RS1,RS0  - 00: 1Hz, 01: 4KHz, 10: 8KHz, 11: 32KHz
;     |  +------> SQWE     - 1: Enable square wave output
;     +---------> OUT      - State of clock out when SQWE is disabled. 0 or 1
_TIME:    DEFB 00h             ; Secs + enable clock
          DEFB 22h             ; Mins
          DEFB 00000000b | 23h ; 24hr clock, 1am
          DEFB 01h             ; Day of week (?)
          DEFB 12h             ; Date
          DEFB 01h             ; Month
          DEFB 20h             ; Year
          DEFB 10010000b       ; Output enabled, 1Hz clock


          DSEG
; ---------------------- VARIABLES
; If you want the monitor code in ROM then add an ORG here to locate variables somewhere in RAM
_STRTDS    EQU     $

LOAD_MODE: DEFB    0
PAGE_MASK: DEFB    0              ; For test add 1 to the page mask
LOAD_CNT   DEFW    0
LD_PAGE:   DEFB    0              ; This will be the basis for all code loads and executes. CONST
PAGE_MP:   DEFS    4              ; Used to restore/track active pages
APP_STK:   DEFS    2              ; Place to store the applications stack pointer before an IOS call
DMA_PAGE   DEFS    1              ; Page the application wants us to wrote SDcard data to
DMA_ADDR   DEFS    2              ; Offset into the page of the DMA buyffer
FNAME:     DEFS   22              ; 20 byte file name for boot
LOADADD:   DEFS    2              ; For the load command, where to start loading binary data
INITD:     DEFS    1              ; Set to '1' once OS initialissation for an application has been done.
OPMODE:    DEFS    1              ; Operational mode. 1=normal. 2=debug
CMDTAB:    DEFS    2              ; Operational mode. 1=normal. 2=debug
LAST_CMD:  DEFS    1
DUMP_ADDR: DEFS    2
DUMP_MODE: DEFS    1
DUMP_TX:   DEFS    1              ; By default application addresses. Set to 1 to use physical addresses
DUMP_CHRS: DEFS    2
           DEFS   16
           DEFS    1

; Define a 256 byte RAM block to receive data from the Pi file system
FILE_BUF:  DS      256
FIN_CODE:  DEFB    0
AUTO_RUN:  DEFB    0

FULBLKS    DEFB    8
LASTB      DEFW    0
SDADDR     DEFW    0, 0

PROGADD    DEFS    2
PROGPG     DEFS    1

EXECADD    DEFW    0
SDPAGE     DEFS    512

; SDCard disk emulation. Treat the SD card as an array of 4M chunks (physical disks). The mapped
; space is 10bit (1024 physical spaces) each 4MB is size allowing a theoretical maximum of 4GB
; although at the moment we're limiting the code to the original 32bit address space mode of
; smaller cards. Generally 2GB cards are the target. Each physical space can be mapped into
; 16 logical drives. This allows an application to 'mount' up to 16x4MB partitions. By default
; the 16 available logical drives map to physical drives 1-16. Drive 0 is used by the loaded
; and so is not normally used by guest operating systems.
;
; Each entry in this table is the upper 10 bits of the 32 bit SD card address.
;
;THIS SPACE NEEDS TO BE INITIALISED ON RESTART.
DRVMAP     DEFS    16*2


; Breakpoints. Each entry is 4 bytes:
; Type|AddrX2|Opcode
; A breakpint in placed in memory by replacing an instruction with a single byte RST 20h (or replacement) opcode
; Type: 0 - free slot - can be used to store a new breakpoint
;       1 - single shot - removed once hit
;       2 - permanent breakpoint (WIP)
BPOINTS    DEFS    NUM_BK*BP_SIZE

; If 'AUTO_RUN' is non-zero then the BP handler will automatically execute a 'GO', setting new BPs
AUTO_STP   DEFS    1
BPCT       DEFS    1      ; Value passed into INSTBP

; Single Step Count...
STP_CNT    DEFS    2      ; Up to 64K steps!!!
STP_IN:    DEFS    1      ; Temp store for setting a BP. True means step into and CALL ops.

; 56 bytes of NV RAM. First 16 bytes used by the loader, others available by the application.
NVRAM      DEFS    56

; Suspenended execution registers. NOTE - application page 0
; has the applications AF and PC.
R_PC       DEFS    2
R_SP       DEFS    2 ; Initial application stack is NOT the same as ours
R_AF       DEFS    2
R_AF_P     DEFS    2
R_BC_P     DEFS    2
R_DE_P     DEFS    2
R_HL_P     DEFS    2
R_BC       DEFS    2
R_DE       DEFS    2
R_HL       DEFS    2
R_IX       DEFS    2
R_IY       DEFS    2

_ENDDS     EQU     $
.END
