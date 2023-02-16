import defs.asm
import config.asm
import api.asm

import pcb_def.asm
import zios.asm

          ; Utilities
          extrn  PAUSE,STRCMP,ADD8T16,TOUPPER

          extrn  PRINT,PRINT_LN,NL,GET_LINE,SKIPSPC,WASTESPC,BUFCHR,BUFCHUP,UNGET,RESINP
          extrn  WRITE_D,WRITE_8,WRITE_16,INHEX,INHEX_2,INHEX_4
          extrn  BRK_HK,SETHIST,GETHIST
          extrn  CMD_B
          extrn  DISASS, SETOFF
          extrn  GET_HEX,GET_DEC,INPTR,INBUF,INITSIO,RXA,TXA,CKINCHAR,SERINT
          extrn  DEC2BIN
          extrn  SD_PRES

          extrn  AP_DISP
          extrn  SW_CFG
          extrn  MAPMPG

          ; And the RTC/i2c library
          extrn  RTC_INI, RTC_GET, RTC_SET

          extrn  FILL,CLRSCR,INPUT,OUTPUT,MODIFY,SDMOD,HELP

          ; Imports from 'drive.asm'
          extrn  SDTXLTD
          extrn  SDMPADD
          extrn  SDMPRAW
          extrn  SDPREP

          extrn  P_RES

          ; Code dispatch in application code
          extrn  AP_ST, END_APP
          extrn  DO_BP_S
          extrn  CNTINUE
          extrn  R_PC_S
          extrn  AND_RUN
          extrn  CHR_ISR
          extrn  RAWGO
          extrn  R_AF_S

          public main,BADPS,OPMODE

          public APP_STK
          public PGADJ,PGMAP,PGMAPX,PGREST
          public SDPAGE
          public PRTERR
          public CONTXT
          public END_RES
          public _IOERR
          public DO_BP
          public SCRATCH
          public ISRCTXT
          public CMD_TABLE
          public RUN_CLI

          public START,_STRTDS,_ENDDS
          public _EISR

; OP Code to use in RST vectors
VEC_CODE    EQU   $C3

; Number of breakpoint locations (in code)
NUM_BK      EQU    64
BP_SIZE     EQU     4

          ASEG
          ORG    0
          DI
          JR     START

          ORG    $08
          JR     TXA

          ORG    $10
          JR     RXA

          ORG    $18
          JR     CKINCHAR

; 30h is the OS entry point. This allows applications running in other pages to request
; a low level OS function (eg page mapping). Installed code can optionally handle all the
; hardware itself or can rely on the OS installing this handler. Note: It has to be one
; or the other!!! If installed then RST 30h invokes the OS services See '_DIS2' for available
; services and parameters.
          ORG    30h           ; OS entry point
          JP     AP_DISP       ; Can jump straight into the dispatcher. No memory changes required.

CONTXT    DEFS    1    ; 0 if running as supervisor, 1 if running as application

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

            BANK  0,RAM_PG_0   ; RAM page 0 now matches Flash page 0 so replace flash with RAM

            ; Flash page 1 is in RAM page 1 and RAM page 1 is mapped to bank 3, end of Z80 memory

RUN_CLI:    ; We're running in supervisor mode
            XOR   A
            LD    (CONTXT),A

            ; Give ourselves a stack at the end of our reserved page.
            LD    SP,SP_STK

            ; Set mode
            LD    A,1
            LD    (OPMODE),A      ; Command mode (don't start in debug)
            LD    HL,CMD_TABLE
            LD    (CMDTAB),HL     ; Search table for commands

            ; Set default DUMP mode to disassemble memory
            LD     A,'I'
            LD     (DUMP_MODE),A

if CSET
            ; Install character set
            CALL   INITCSET
endif
            ; Set up initialisation vectors
            ; LD     A,VEC_CODE     ; Set up RST jump table
            ; LD     ($08),A
            ; LD     ($10),A
            ; LD     ($18),A
            ;
            ; LD     HL,TXA
            ; LD     ($09),HL
            ; LD     HL,RXA
            ; LD     ($11),HL
            ; LD     HL,CKINCHAR
            ; LD     ($19),HL

            ; Initialise drive map. Map logical drives 0-15 to physical blocks 1-16. Need to store
            ; the physical address << 6 (Upper 10 bits of the 32 bit SD card address).
            CALL   SDPREP

            ; Initialise i2c
            CALL   RTC_INI

            ; and read the NV RAM
            CALL   NVLD

            ; Initialise the interrupt vector table. Table will be right at the end of the
            ; memory map which will be in our second RAM page and will be copied for
            ; application context.
            LD     A,VEC_BASE
            LD     I,A

            ; Add entry to vector table for supervisor SIO ISR
            LD     HL,_EISR
            LD     (0xC000+SIO_ARX),HL

            ; Initialise the serial IO module
            XOR    A              ; Initialise SIO without setting up interrupt vectors.
            CALL   INITSIO
            IM     2              ; Using interrupt mode 1 AT THE MOMENT. Need to move to vectored at some point
            EI

if IS_DEVEL
            ; If this is a developement build, reserve the base pages.
            LD    A,20h
            CALL  P_RES
            LD    A,21h
            CALL  P_RES
endif
            LD    A,LD_PAGE
            LD    HL,PAGE_MP
            LD    B,4
_wrn:       LD    (HL),A         ; Initialise the application page map
            CALL  P_RES          ; Reserve this page
            INC   A
            INC   HL
            DJNZ  _wrn

            ; Really simple CLI now. Display
NOSIO:      LD    HL, _INTRO
            CALL  PRINT_LN
            CALL  SH_HW

if !IS_DEVEL
          ; Check the state of the DIL switches and look for autoboot mode
          CALL  SW_CFG
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
E_REC:    LD    HL,_REC_ERR
          JR    _prterr
E_ERROR:  LD    HL,_ERROR
          JR    _prterr
E_UNKWN:  LD    HL,_UNKWN
          JR    _prterr
M_COMP:   LD    HL,_COMPLETE
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

; ------------------- _GDECNUM
; Read two decimal digits into the A register. The high nibble contains the first digit,
; the low nibble the second digit. So the string '23' would return A containing 23h.
; Carry flag set on error.
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
          OR    A       ; Clear carry flag and set Z according to value.
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
DNVRAM:   CALL   NVRD
          LD     C,7         ; 7 rows
_nrnv:    LD     B,8         ; 8 bytes per row
_ncnv:    LD     A,(HL)
          CALL   WRITE_8
          INC    HL
          WRITE_CHR SPC
          DJNZ   _ncnv
          CALL   NL
          DEC    C
          JR     NZ,_nrnv
          JR     main

; ------------------- DUMPM - Block dump memory
DUMPM:    LD     A,'M'
          JR     _dodmp

; ------------------- DUMPI - Disassembler
DUMPI:    LD     A,'I'
          JR     _dodmp

; ------------------- DUMP
DUMP:     LD    A,(DUMP_MODE)     ; Use previous/default dump mode
_dodmp:   LD    B,A               ; Accepted mode in 'B'
          LD    (DUMP_MODE),A     ; Store mode

no_mode:  CALL  GET_HEX           ; Get the address to dump
          JR    NZ,_useval        ; If no value then use previously stored address
cnt_dump: LD    HL,(DUMP_ADDR)

_useval:  LD    (DUMP_ADDR),HL
          LD    A,B               ; Get the mode character back
          CP    'M'
          JR    NZ,decode         ; Mode is not 'M' so it's an instruction level dump

          ; Display 8 blocks of 16 characters
          LD    C,8

dloop2:   CALL  WRITE_16          ; 4 hex digits from HL
          CALL  PGMAPX            ; Translate into an offset and a page number and map application pages into memory

          ; Dump address (start of line)
          CALL  DMP16

          LD    HL,(DUMP_ADDR)
          LD    A,16
          CALL  ADD8T16
          LD    (DUMP_ADDR), HL
          DEC   C
          JR    NZ,dloop2
          CALL  PGRESTX            ; Reset application space registers
          JR    main

; -------------------- DMP16 --------------------
; Dump 16 bytes from content of HL to stdout.
; Input  HL: Address of first byte to display
; Output HL: Points to first byte AFTER the 16 byte display
DMP16:    PUSH  BC
          PUSH  DE
          PUSH  HL
          ; Prepare dump area
          LD     HL,DUMP_CHRS
          LD    (DUMP_CHRS),HL
          LD     B,20h
          LD     A,20h
_clr:     LD     (HL),A
          INC    HL
          DJNZ   _clr;
          XOR    A
          LD     (HL),A
          POP   HL
          WRITE_CHR SPC
          LD    DE,DUMP_CHRS+2
          LD    B,16             ; Number of bytes to display
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
          LD    HL,DUMP_CHRS
          CALL  PRINT_LN
          POP   HL
          POP   DE
          POP   BC
          RET

BOOT:     LD    A,1
          LD    (AUTO_RUN),A       ; Set auto-run mode

BOOTIX:   XOR   A
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

; ---- decode
; DUMP decode (instruction decode and display)
decode:   LD    B,20       ; Number of instructions
_nexti:   CALL  DECINST
          DJNZ  _nexti
          LD    (DUMP_ADDR), HL
          CALL  PGRESTX    ; Undo any application space page damage
          JR    main

; -- DECINST
; Decode and display single instruction. HL points to the sart of the instruction. Displays
; the HEX bytes for this instruction followed by a newline.
; INPUT:  HL - the application space address of the instruction
;          A - Address mode
; OUTPUT: HL - First byte of _next_ instruction
; Registers not saved: A
DECINST:  PUSH  BC         ; DISASS returns instruction information we don't need in BC
          PUSH  DE
          PUSH  HL         ; Application space address we want to display
_appdec2: CALL  WRITE_16   ; Write out the application space address
          CALL  PGMAPX     ; Translate into an offset and a page number and map application pages into memory

          ; A contains the block number of the address, 0-3:
          RRCA
          RRCA             ; Now in top 2 bits
          SUB   40h        ; Gives the top byte of the offset
          LD    D,A
          LD    E,0
          EX    DE,HL
          CALL  LSETOFF
          EX    DE,HL

          WRITE_CHR SPC
          PUSH  HL
          CALL  LDISASS    ; HL now points at the description
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


PREPLD:   LD    HL,100h           ; Default load address for binary data
          LD    (PROGADD),HL
          XOR   A
          LD    (AUTO_RUN),A      ; Cancel auto-run mode
          LD    (LOAD_MODE),A     ; Default to HEX mode
          RET

LDF:      CALL  PREPLD
          JR    cmd_bin

LDH:      CALL  PREPLD
          JR    cmd_load

; ---------------- LOAD
; Process all lines starting with a ':'
LDT:      CALL  PREPLD
          XOR   A
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
          JR    Z,M_COMP
          JR    E_REC             ; Otherwise an error


; Use the CMD_B loader to read blocks of data. Rest of line is the name of the file
cmd_load: LD    HL,_FNAME
          CALL  PRINT
          CALL  GET_LINE
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
          JR    NZ,E_NOTF

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

          ; *************** The following looks WRONG. Should be using the page map *************
          LD    A,(PROGPG)
          INC   A          ; Map the next page into block 1 space
          LD    (PROGPG),A
          BANK  1
          ; *************** ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ************************
          JR    _nxtblk

_fin:     INC   A
          JR    Z,E_ERROR
          CALL  NL
          JR    main

; ------------------- _hex_load
; File is open. Read blocks into our own space and use to fill the input line buffer and call
; the HEX load for each line.
_hexld:   LD    DE,INBUF
          LD    (INPTR),DE
          XOR   A
          LD    (FIN_CODE),A
_hexldn:  LD    A,(FIN_CODE)
          OR    A
          JR    NZ,E_REC
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
          JR    M_COMP

; ------------------- SET_RGS
; CMD: Set register value. R reg=val
; reg is A,B,C,D,E,H,L,BC,DE,HL,IX,IY
; val is an 8 or 16 bit hex value
SET_RGS:  CALL  BUFCHUP    ; Get the name of the register, one or two characters
          JR    Z,SHOW_RGS ; nothing to use
          LD    D,A        ; First character (required)
          LD    E,0
          CALL  BUFCHUP    ; either a space or '=' otherwise use it
          CP    '='
          JR    Z,_getval
          CP    ' '
          JR    Z,_8bit
          LD    E,A
_8bit:    CALL  SKIPSPC    ; Waste characters until '=' or end of line
          JR    Z,E_UNKWN  ; End of line
          CP    '='
          JR    NZ,_8bit
          ; Now get the hex value to write. Don't care about size at this point
_getval:  CALL  GET_HEX    ; Value in HL
          JR    Z,E_UNKWN  ; no value entered
          ;     DE: One or two character register name
          ;     HL: Value to store in register
          CALL  _reg_addr
          JR    C,E_UNKWN  ; Unknown register name
                           ; DE  now contains the address of the register to write. A:0 8 bit, A!=0 16 bit
          EX    DE,HL      ; 8 bit is common between the two options
          LD    (HL),E
          JR    NZ,_rend   ; Z will be set for an 8 bit register
          INC   HL         ; It's 16 bits so write the second byte
          LD    (HL),D
_rend:    JR    SHOW_RGS

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
DO_RGS:   LD    HL,SAVE_POS
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
SSTEP:    CALL  GET_DEC        ; Get optional step count
          JR    C,_stp1
          INC   HL
_stp1:    LD    (STP_CNT),HL   ; By default step once
          LD    HL,0
          LD    E,1            ; A -> !0
          CALL  SSTEP_BP       ; Set single step BP then go
          JR    DO_GO

; ------------------- go
GO:       CALL  WASTESPC
          JR    Z,_noadd

          ; Is there an address?
          CALL  GET_HEX        ; Will be zero if there was no value specified
          JR    Z,_noadd       ; Don't upset the stored PC if nothing entered (go from PC address)

          ; Have an address - put it in the virtual PC
          LD    (R_PC),HL

_noadd:   LD    HL,1
          LD    (STP_CNT),HL ; Make sure we don't keep running when we hit a breakpoint!!
          LD    E,2
          XOR   A            ; Want breakpoints added
DO_GO:    PUSH  DE
          CALL  INSTBP       ; Install all permanent breakpoints
          POP   DE
          LD    A,E
          CP    2
          JR    NZ,_godo
          LD    A,(AUTO_STP)
          OR    A
          JR    NZ,SSTEP     ; Transmute into a single step

_godo:    ; Install the drivers.
          CALL  INSTDRV
          LD    HL,(R_SP)
          PUSH  HL
          CALL  PGMAPX                ; Stack memory now accessible in HL
          LD    DE,(R_AF)             ; Simulate pushing AF onto app stack
          DEC   HL
          LD    (HL),D
          DEC   HL
          LD    (HL),E
          POP   HL                    ; Get the original unmapped stack
          DEC   HL
          DEC   HL                    ; So we can later POP AF
          LD    (R_SP),HL

          ; Map all pages EXCEPT page 0 (where we're running!!!)
          CALL   MAPAPP

          ; Set up the jump address
          LD    HL,(R_PC)
          LD    (R_PC_S),HL           ; Where we want to continue execution from

          ; Restore as many regs as possible here. No longer need our stack so can
          ; discard. Only thing that can't be restored is A which is needed to reset
          ; the application space.
          LD    SP,R_AF_P
          POP   AF
          EX    AF,AF'      ; '
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
          LD    A,(PAGE_MP) ; and the page that needs to go into bank 0

          JR    CNTINUE     ; And......... GO!


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
          CALL  LDISASS     ; Only disassemble to get control flow information
          POP   HL          ; And get the original PC back again (pointing to start of instruction for rel jumps)
          ; A: Instruction length
          ; C: Extended status
          ; HL: Unchanged - start of THIS instruction
          ; DE: Mapped physical address of target instruction (start of)
          CALL  ADD8T16      ; Adjust the application address to start of next opcode
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
          LD    (CTRLCH),A
          LD    (STP_CNT),A

          LD    A,(ISRCTXT)
          OR    A              ; If zero then running in supervisor, non-zero was application
          JR    NZ,_appbrk

          ; ISR was in ZIOS space so real AF is on the stack. Simples.
          POP   HL
          POP   AF

          ; Next value on the stack is the PC
          EX    (SP),HL
          LD    (R_PC),HL
          EX    (SP),HL
          JR    _cnt_bp

          ; Application was running. Will have supervisor stack but data is on app stack.
_appbrk:  PUSH   DE

          LD     HL,(APP_STK)   ; Application stack pointer, AF is at the top of that stack
          CALL   PGMAPX         ; Map stack pages into memory - txlated address in HL
          LD     E,(HL)
          INC    HL
          LD     D,(HL)         ; DE contains what was in AF
          INC    HL
          ; Next 2 bytes are the return address. This will get decremented in the BP handler
          ; because it thinks it's been called via a single byte RST call. To get around this
          ; increment the address on the stack.
          PUSH   DE             ; Original AF
          POP    AF             ; Get back into the AF registers

          LD     HL,(APP_STK)   ; Remove the AF value just retrieved from the stack
          INC    HL
          INC    HL             ; HL now has the old value of the stack pointer
          LD     (R_SP),HL

          ; Everything now is back where we need it to be so can do standard break point processing
          POP    BC
          POP    DE
          POP    HL

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
          PUSH  AF

_appbp:   LD    SP,SP_STK     ; Restore our own SP

          ; Only thing left is the PC. TO get this we need to read the top of the
          ; application's stack, which might not be visible.
          LD    HL,(R_SP)      ; Find the stack
          CALL  PGMAPX         ; Map into sys space, stack memory now accessible in HL
          LD    E,(HL)
          INC   HL
          LD    D,(HL)         ; DE is now one more than the actual PC address (unless it's CTRL-C)
          LD    A,(CTRLCH)     ; DON'T decrement the PC if we're here because someone pressed CTRL-C
          OR    A
          JR    NZ,brkctxt
          DEC   DE             ; To point to the RST reset that got us to the BP
brkctxt:  LD    (R_PC),DE
          LD    HL,(R_SP)
          INC   HL
          INC   HL             ; Effectively pop the RST return address, going to JP to continue
          LD    (R_SP),HL

          ; Disable break processing
_bpc:     LD    HL,0
          LD    (BRK_HK),HL

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

          ; Have an address. Set or clear a BP
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
          JR    NZ,_LISTBP       ; Duplicate so list (no change)
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
INSTDRV:  ; Check configuration to see whether we're meant to be installing drivers
          LD     A,(NVRAM)
if IS_DEVEL
          LD     A,1 ; ALWAYS load drivers and break mode if this is a development build
endif
          RRCA
          RET    NC  ; This application doesn't want drivers (not using core OS)
          RRCA       ; Get bit one from NVRAM flags

          BANK  1,(PAGE_MP)          ; Application page 0 => bank 1

          LD     HL,0
          JR     NC,_nobrk

          ; Install the break driver. Conditional on bit 1 of the flags tested above.
          LD    HL,BRK_HDLR
_nobrk:   LD    (BRK_HK),HL

          ; Set the ISR context and CTRLCH to zero - set as part of application ISR preamble
          ; CTRLCH will be set in the CTRL-C handler and prevents an eroneous decrement of
          ; the programme handler in DO_BP
          LD    HL,0
          LD    (ISRCTXT),HL

          ; And FORCE the CONTXT value to be 1 to identify application context
          INC   A
          LD    (CONTXT+$4000),A

          ; Set up RST handlers. Only ones currently mandated are:
          ;   RST 28h     - Used for debugger breakpoints
          ;   RST 30h     - API entry point

          LD   A,$C3
          LD   (4030h),A             ; API entry point: RST 30h
          LD   (4000h+BRK_HANDLER),A ; Breakpoint handler

          ; Patch the dispatch point for the API
          LD   HL,AP_ST
          LD   (4031h), HL

          ; The breakpoint handler
          LD   HL,DO_BP_S
          LD   (4001h+BRK_HANDLER),HL  ; Breakpoint handler target address

          ; Copy reserved 512 bytes from ZLoader to app space.
          BANK  2,(PAGE_MP+3)          ; Application page 3 => bank 2

          ; Map our second page into memory (should already be there!)
          BANK  3,MN2_PG               ; Supervisor page 2 => bank 3

          ; Copy our driver code into the end of APPLICATION memory. It's
          ; currently at the end of our second bank.
          LD    HL,$FE00               ; Supervisor second page
          LD    DE,$BE00               ; Application last page
          LD    BC,512                 ; Transfer 512 bytes
          LDIR

          ; Patch the application SIO ISR. It's different for application
          ; space because page memory tweaks will be requied.
          LD   HL,CHR_ISR
          LD   (8000h+SIO_ARX),HL

          ; Map all application pages except the first into Z80 memory
          CALL  MAPAPP
          RET

; ------------------- MAPAPP
; Map application pages 1 to 3 into Z80 memory. This leaves only the
; block zero to be mapped back into memory space.
MAPAPP:   LD     HL,PAGE_MP+3
          BANK   3,(HL)
          DEC    HL
          BANK   2,(HL)
          DEC    HL
          BANK   1,(HL)
          RET

; ------------------- RUN
; This initial version loads the first four RAM pages into the four
; banks, reserving the last 16 bytes of page 03 to store an address
; switcher. Once switch it jumps to address 0 to run that code.
RUN:      DI
          CALL  WASTESPC
          JR    Z,PREPRUN
          CALL  GET_HEX    ; Get optional execution address into HL
          JR    NZ,PREPRUN
          LD    HL,0

PREPRUN:  ; Map application page 0 into block 1 so we can initialise the reserved
          ; space. HL contains the execution address.
          PUSH   HL
          CALL   INSTDRV
          POP   HL

          ; Running with or without drivers, but everything is now ready to run. If
          ; the API is loaded then we can use installed drivers to make life easy,
          ; otherwise need to do a raw run to avoid poluting application space.
          LD    A,L
          OR    H
          JR    Z,_RMODE       ; Run from zero if not specified

          BANK  1,(PAGE_MP)    ; Application page 0

          ; Specific address so add a JP operation at address zero
          LD    A,$C3          ; JP
          LD    ($4000),A
          LD    ($4001),HL     ; Start address

_RMODE:   LD    A,(NVRAM)
          RRCA                 ; Services installed?
          JR    C,AND_RUN      ; Drivers installed so don't need to jump through hoops

          ; No seervices. Want to run without restricting any of the application
          ; code. Map ourselves into bank 3, set up banks 1 and 2 then jump to end
          ; of memory. This is running from supervisor page zero.
RAWRUN:   LD    HL,(PAGE_MP)
          LD    DE,(PAGE_MP+2)

          ; Banks 1 and 2 are safe to map now.
          BANK  1,H
          BANK  2,E

          ; HL needs: H: app page 3; L: app page 0. L already set.
          LD    H,D
          JR    RAWGO

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

; ------- SDDIR
; List the set of bootable images
;
; Running from monitor in bank 0. Map ourselves into bank 3.
; Map application into banks 1 and 2, map monitor into
; bank 3 and add init code to the end of page page.

; Record format
; All records are 32 bytes and the 512 byte first block contains 15 records
;  0 - 00 (1): TYPE              : 0: unused, 1: used
;  1 - 01 (1): ID                : Numeric ID for this image. Must be unique and non-zero.
;  2 - 02 (8): NAME              : Printable
; 10 - 0A (1): DEVICE            : Qualifies SD_ADDR by identifying the SDCard device number
; 11 - 0B (4): SD_ADDR           : Byte offset into SD Card (SD card address)
; 15 - 0F (2): LOAD_ADD          : Address in application space to load this image
; 17 - 11 (2): LENGTH            : Number of bytes to load
; 19 - 13 (2): EXEC_ADDR         : Once loaded, execure from this address
; 21 - 15 (1): FLAGS             : Bit 0. If bit 0 set then load libraries, otherwise DON'T
SDDIR:    CALL  BTPREP
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

          ; Usable so display
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

          ; Next byte is the device ID (context for SDAddr)
          LD    A,(HL)
          INC   HL
          CALL  WRITE_8
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
          CALL  ADD8T16
          POP   BC
          DJNZ  _nxsec

          JR    main

; ---- FNDIMG
; Find an image with the ID stored in the C register.
;   B  is not preserved
;   HL returns the address of the start of the description block
;   Z  set if the image IS FOUND
;
; If the image is NOT found then HL returns the address of the
; first free slot.
;
FNDIMG:   LD    HL,SDPAGE
          LD    DE,0
          LD    B,16

          ; Check this block - should countain 01 if it's usable
_again:   LD    A,(HL)
          DEC   A
          JR    Z,_usdbs

          ; Empty slot
          LD    A,D
          OR    A
          JR    NZ,_nxtbl
          LD    E,L
          LD    D,H
          JR    _nxtbl

_usdbs:   INC   HL             ; Usable block - is this the OS we want
          LD    A,(HL)
          DEC   HL
          CP    C              ; The one we want?
          RET   Z              ; Yes. HL points to start of block descriptor
_nxtbl:   LD    A,32
          CALL  ADD8T16
          DJNZ  _again
          LD    L,E
          LD    H,D
          XOR   A              ; HL to available slot
          INC   A
          RET                  ; Return NZ for Not Found

_FREES:   DS    2

; ---- SDLDDEF
; Load SDCard boot image with tag ID 1.
SDLDDEF:  LD    C,1
          JR    _bsload


; ------- SDBREAD
; Load data from SDCard into memory. Handles both absolute and drive relative addresses.
; INPUTS   (SDDRV):   Drive (0 or 1 at the moment)
;          (SDADDR):  Start sector on the SDCard
;          HL:        Address in RAM into which to store data
;          DE:        Number of bytes to load
SDBREAD:  ; Number of whole 512 byte blocks
          LD    A,D
          OR    A
          RRA
          LD    B,A    ; b = number of full blocks to load

          ; Need a partial block at the end?
          LD     A,D
          AND    1
          LD     D,A
          LD     (LASTB),DE

          ; Start loading blocks.
_bxtbl:   PUSH   BC
          PUSH   HL

          ; Tell API where to put data
          LD     C,A_DSKDM
          RST    30h

          ; Load the SDCard sector address
          LD     DE,(SDADDR)
          LD     HL,(SDADDR+2)
          LD     A,(SDDRV)
          LD     B,A

          LD     A,(DDMP_MDE)
          LD     C,A
          RST    30h                    ; Load the next page in to RAM

          ; Increase SD address and memory address by 512
          POP    HL
          INC    H                      ; Add 512
          INC    H
          PUSH   HL

          ; Move to next sector
          LD     HL,(SDADDR)
          INC    HL
          LD     (SDADDR),HL
          LD     A,L
          OR     H
          JR     NZ,_nxt1

          ; Overflow on the low 16 bit address. Inc high 16 bits.
          LD     HL,(SDADDR+2)
          INC    HL
          LD     (SDADDR+2),HL

          ; Any more full blocks to load?
_nxt1:    POP    HL
          POP    BC
          DJNZ   _bxtbl

          ; Full blocks loaded and addresses set up for any partial last block
          LD     DE,(LASTB)
          LD     A,E
          OR     D
          RET    Z

          ; DE contains number of bytes from last partial block. HL
          ; is the target address. Standard buffer and use from there.
          PUSH   HL
          LD     C,S_DSKDM
          RST    30h                  ; Set system DMA buffer

          LD     DE,(SDADDR)
          LD     HL,(SDADDR+2)
          LD     A,(SDDRV)
          LD     B,A
          LD     A,(DDMP_MDE)
          LD     C,A
          RST    30h                  ; Load the next page in to RAM

          ; Copy to destination
          POP    HL                   ; Current load address. Need to know translate into a valid page address
          CALL   PGMAP                ; HL contains the target address

          LD     DE,SDPAGE            ; The partial block
          LD     BC,(LASTB)           ; Number of bytes
          EX     DE,HL
          LDIR                        ; Copy
          RET


; ------- SDLOAD
; Load a bootable image but DON'T run it!
SDLOAD:   XOR   A
          LD    (AUTO_RUN),A   ; Auto-run off
          JR    _sdcont

; ------- SDLOAD
; SD Card Load. Options:
;    No parameters - Load OS type 01 - which is CP/M
;    With a digit (0-9) look for and load a specific type
;    With '?' - display all available options and exit
SDRUN:    ; Default to auto-run
          LD    A,1
          LD    (AUTO_RUN),A

_sdcont:  CALL  BTPREP

          ; Calculate the checksum
          JR    NZ,E_ERROR

          CALL  WASTESPC
          LD    C,1
          JR    Z,_bsload      ; Boot OS with ID 01

_sdauto:  CALL  GET_DEC        ; Get the decimal type number to boot
          JR    C,BADPS
          LD    A,H
          OR    A
          JR    NZ,BADPS
          LD    C,L            ; 'A' contains the image ID to boot

_bsload:  LD    HL,_BSD
          CALL  PRINT
          CALL  FNDIMG
          JR    NZ,E_NOTF

          ; Found the one we want so load
_doload:  INC   HL    ; Over the use flag
          INC   HL    ; Over the ID

          ; At start of name. Print name as we scan past it.
          LD    B,8
_nchr:    LD    A,(HL)
          INC   HL
          OR    A
          JR    Z,_na
          RST   08h
_na:      DJNZ  _nchr
          CALL  NL

          ; Rest of the entry:
          ; 1 byte drive/device number
          ; 4 byte SD card block address
          ; 2 byte load address
          ; 2 byte length
          ; 2 byte exec address
          ; 1 byte flags
          LD    A,(HL)
          INC   HL
          LD    (SDDRV),A

          LD    E,(HL)
          INC   HL
          LD    D,(HL)
          INC   HL
          LD    (SDADDR), DE

          LD    E,(HL)
          INC   HL
          LD    D,(HL)
          INC   HL
          LD    (SDADDR+2), DE   ; SDCard address configured. This is EITHER an
                                 ; absolute sector address or relative to a virtual
                                 ; drive depending on bit 7 of the flag byte.

          LD    E,(HL)           ; Next two bytes are the application space load address
          INC   HL
          LD    D,(HL)
          INC   HL
          LD    (LOADADD),DE

          LD    E,(HL)
          INC   HL
          LD    D,(HL)        ; DE is the length
          INC   HL
          PUSH  DE            ; Store length on the stack

          LD    E,(HL)        ; Store the exec address
          INC   HL
          LD    D,(HL)
          LD    (EXECADD),DE
          INC   HL
          LD    A,(NVRAM)     ; Default flag byte
          AND   $7C           ; Clear the lower two bits and MSB
          OR    A,(HL)        ; Add in our own flags (OS and break handler)
          LD    (NVRAM),A

          ; Stored address is always absolute but if it was specified as relative when
          ; the boot record was created then map the address into logical disk drive A
          RRA
          JR    NC,_absadd

          ; Relative so MAP the logical drive number into virtual drive zero (the A drive). Get
          ; the most significant 16 bits of the address
          LD    HL,(SDADDR+1)   ; Logical drive number

          ; ----------------- TO CHANGE --------------
          ; Boot record needs to include a device number (one more byte!)
          LD    A,(SDDRV)       ; Which SDCard
          LD    D,A             ; must be passed as B to SDMPRAW
          LD    A,0             ; Drive A to be mapped (0)
          CALL  SDMPRAW         ; Map the drive.

_absadd:  ; Need to work out how many whole blocks we need to load
          POP   DE              ; Get the length back from the stack
          LD    HL,(LOADADD)    ; And where to start loading.

          ; Always going to be reading raw for this function
          LD     A,A_DSKRW
          LD     (DDMP_MDE),A

          CALL   SDBREAD

          ; Load complete, auto-execute?
          LD     HL,(EXECADD)

          LD    A,(AUTO_RUN)
          OR    A
          JR    NZ,PREPRUN
          JR    main

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
          LD    HL,_FULL
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




; ------------ PAGE
; Page operations. Syntax:
;   PM - Display or change page map
;   PC - Copy one pge to another
PAGE:     ; Display current application pages or change a page number
          ; Changing is in the form blknum=pagenum. Eg 3=21
          CALL  SKIPSPC
          JR    Z,_shpg
          SUB   '0'
          JR    C,_shpg
          CP    4
          JR    NC,E_ERROR
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
          CALL   ADD8T16

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
          CALL   ADD8T16
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

; ---- _say
; Display a string in the form:     NAME = YN
; Where 'NAME' is the string pointed to by HL
;       'YN' is the 'YES' or 'NO' depending on the Z flag
; Z flag is the state of the flag. HL points to the name
;
; Usse to format the configuration settings.
_say:     PUSH  AF
          PUSH  HL
          JR    NZ,_sayyes
          LD    HL,_no
          JR    _saynow
_sayyes:  LD    HL,_yes
_saynow:  EX    (SP),HL
          CALL  PRINT
          LD    HL,COLSTR     ; Tab alignment
          CALL  PRINT
          POP   HL
          CALL  PRINT         ; The yes/no string
          CALL  NL
          POP   AF
          RET

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
PGRESTX:  BANK  2,(PAGE_MP+2)

; ------------- PGREST
; Restore page 1 to the application space.
PGREST:   BANK  1,(PAGE_MP+1)
          RET

; HL - load address. Translate in a page and offset then load that page into board page 1 and 2
;      return the offset in HL
; A returns the page mapped.
PGMAP:    PUSH  DE
          CALL  PGADJ    ; Returns the target page number in A
          BANK  1        ; which we map to bank 1
          POP   DE
          RET

; ---- PGMAPX
; Same as PGMAP but also places the *next* application space page into bank 2. Use this if
; operations could cross a 16K page boundary.
; INPUT:   HL - address in application space
; OUTPUT:  HL - mapped address to use within supervisor map
PGMAPX:   PUSH  DE
          CALL  _PGCALC         ; HL: Adjusted address, A the 16K application block number to map (0-3)
          PUSH  AF
          LD    DE,PAGE_MP
          ADD   E
          LD    E,A      ; Monitor memory is linked from 2000h so won't cross a 256 byte boundary.

          LD    A,(DE)   ; DE is now the address of the PAGE_MP for the addressed page in application space
          BANK  1

          INC   DE
          LD    A,(DE)
          BANK  2

          POP   AF        ; AF includes the logical bock number (0-3)
          POP   DE
          RET

; ---- BTPREP
; Prepare the way for managing the ZLoader boot menu. Load the first 512 bytes from the SDCard
; and set the DMA buffer.
BTPREP:   ; Always need to load the first 512 byte block that includes the boot information
          LD    C,S_DSKDM
          RST   30h           ; Set system DMA buffer
          LD    HL,0          ; Sector 0
          LD    DE,0
          LD    B,0           ; SDCard 0
          LD    C,A_DSKRW     ; Raw read
          RST   30h
          ; DROP THROUGH TO CALCULATE THE Checksum

; Calculate checksum. Calculated over 15 slots using a simple shift algorithm Result 16 bits.
; Compare calculated value with existing value and set Z if they are the same.
_calc_cs: LD    HL,0         ; the calculated partial CS
          LD    DE,SDPAGE    ; Byte we're working on
          LD    B,240        ; Number of 16 bit words to calculate sum over

_nxt_wd:  ; Add in the next 16 bits
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

_wrdev:  PUSH   HL
         PUSH   AF
         LD     A,':'
         RST    08h
         LD     HL,_SDEV
         CALL   PRINT
         POP    AF
         ADD    A,'0'
         RST    08h
         POP    HL
         RET


; ------- MAPDSK
; Display current mapped drives
MAPDSK:  LD    BC,1000h
         LD    A,'A'
_dmapn:  PUSH  AF
         RST   08h
         LD    A,':'
         RST   08h
         LD    A,' '
         RST   08h
         LD    A,C            ; drive for which we want the map
         INC   C              ; for next time
         CALL  SDTXLTD        ; HL: disk number (0-1023), A: SDCard number
         PUSH  AF
         CALL  WRITE_D        ; Write the virtual disk number (HL)
         POP   AF
         CALL  _wrdev         ; Write SDCard name (from A)
         CALL  NL
         POP   AF
         INC   A
         DJNZ  _dmapn
         JR    main

; ----- SMAP
; Map a logical dic
; SM L PPPP - Map Physical drive (decimal 0-511) to logical drive letter L
SMAP:     JR    Z,MAPDSK
          CALL  BUFCHUP
          CALL  TOUPPER
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

          ; Number from 0-1024
          LD     A,$FC
          AND    H
          JR     NZ,E_NOSD

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
_gotsd:   LD     E,A        ; SDCard (into D) as a letter '0' or '1'
          POP    AF         ; Drive letter
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
          LD     C,A_DSKMP       ; Map drive

          ; D:  Drive slot (0-15)
          ; E:  Physical SDCard (0 or 1)
          ; HL: Virtual disk on the SDCard
          RST    30h

          JR     main

; -------- SADDR
; Parse the SDCard address:
;   NNNNNNNN - 32 bit sector address into the whole SDCard
;   NNNNNNN: - 32 bit sector offset into the currently selected default drive (last mapped or listed)
;   NNNNNN:L - 32 bit offset into the specified logical drive (L becomes the default)
; Results stored in SDMP_MD and SDMP_L
; Returns Carry: True if error, false if ok
;         C:     The *read* command code
;         HLDE:  The offset (either raw or relative to logical drive letter)
SADDR:   LD    C,S_DSKDM   ; Reset the SDCard input buffer
         RST   30h

         CALL  WASTESPC
         JR    NZ,_ginp

         ; No address. Get stored value.
         LD    A,(SDMP_MD)
         LD    C,A
         INC   A
         JR    NZ,_inpok

         ; Have no preset data so error
         OR    A
         SCF       ; Set carry flag
         RET

_inpok:  LD    DE,(SDMP_L)
         LD    HL,(SDMP_L+2)
         RET

         ; Expect a 32 bit number
_ginp:   CALL  INHEX

         RET   C      ; Bad value entered

         ; HLDE is the 512 sector address. This is either absolute OR relative to a specific
         ; mapped logical drive.
         CALL  BUFCHR
         LD    C,A_DSKRD

         JR    Z,_sabs    ; Absolute address

         ; Relative to a specific logical drive.
         CP    ':'
         JR    NZ,_sabs

         ; relative to a specific drive (or default)
         CALL  BUFCHUP
         JR    Z,BADPS    ; Missing drive letter

         ; This needs to be a logical drive number (0-15)
         SUB   'A'
         JR    C,BADPS
         CP    16
         JR    NC,BADPS

_defdrv: ; Get the drive offset which is 13 bits in DE
         LD    H,A       ; Save drive letter
         LD    A,$1F
         AND   D
         LD    D,A       ; H: drive number, L:0, DE: truncated to 13 bits (8192 sectors = 4MB)
         LD    L,0
         JR    _sderes

_sabs:   LD    C,A_DSKRW      ; Use command 8 (raw sector read)
_sderes: LD    A,C            ; Store command and address for writebacks
         LD    (SDMP_MD),A    ; RST 30h command code
         LD    (SDMP_L),DE    ; And the 32bit address
         LD    (SDMP_L+2),HL
         OR    A
         RET

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
         CALL  _calc_cs

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

; -------- SDUMP
; Dump the contents of a 512 byte SD card sector. By default this is an absolute sector
; number on the SDCard. Alternatively you can specify an offset into a logical drive letter.
; Formats for the display are:
;   NNNNNNNN - 32 bit sector address into the whole SDCard
;   NNNNNNN: - 32 bit sector offset into the currently selected default drive (last mapped or listed)
;   NNNNNN:L - 32 bit offset into the specified logical drive (L becomes the default)
SDUMP:   JR    Z,E_NOSD
         LD    C,S_DSKDM
         RST   30h
         CALL  WASTESPC
         JR    NZ,_getsd      ; By default load the next available sector

         ; Load the stored values and add one to the sector number
         LD    HL,(SDMP_L)
         INC   HL
         LD    A,L
         OR    H
         JR    Z,main  ; zero sector - uninitialised
         LD    (SDMP_L),HL
         EX    DE,HL
         LD    HL,(SDMP_L+2)

         LD    A,(SDMP_MD)
         LD    C,A
         JR    _ldsd

_getsd:  CALL  SADDR
         JR    C,E_NOSD

_ldsd:   PUSH  HL
         PUSH  DE
         CALL  WRITE_16
         EX    DE,HL
         LD    A,':'
         RST   08h
         CALL  WRITE_16
         POP   DE
         POP   HL

         ; Read the data into SDPAGE
         RST   30h

_do_dmp: CALL  NL
         ; 512 bytes to dump in blocks of 32 bytes
         LD    DE,0       ; The block offset to display
         LD    HL,SDPAGE  ; Source for data
         LD    B,32       ; Row count
_sdnxt:  ; Display a row
         EX    DE,HL
         CALL  WRITE_16   ; The offset
         EX    DE,HL
         ; Display 16 bytes
         CALL  DMP16

         LD    A,16
         ADD   E
         LD    E,A
         JR    NC,_sdnx3
         INC   D
_sdnx3:  DJNZ  _sdnxt
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


SH_HW:      CALL  SH_DTIME
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

; ---------------- IMG
; Write a memory image to the SDCard virtual disk. Parameters are:
;   WI ssss:d start end
;      ssss:d - location on SDCard
;      start  - 16 bit address for start of image (in application space)
;      end    - last byte to write
;   WB - Write a boot image
;
IMG:        CALL  SADDR      ; Sets up the target write address
            JR    C,BADPS
            CALL  WASTESPC
            CALL  INHEX_4    ; Start address in application RAM
            JR    C,BADPS
            EX    DE,HL
            CALL  WASTESPC
            CALL  INHEX_4    ; End address => HL
            JR    C,BADPS

            ; DE: Start address
            ; HL: End address
            ; How many sectors?
            SBC   HL,DE   ; HL is number of bytes. H/2 is number of sectors. Check for partial segments.
            LD    A,L
            OR    A
            JR    NZ,_rndup
            INC   A
            AND   H
            JR    Z,_nornd

_rndup:     INC   H
            INC   H
            JR    NZ,_nornd

_nornd:     SRL   H               ; H is now the number of 512 blocks to write. Write data...
            JR    NZ,_dowr
            LD    H,80h           ; Trying to write the maximum 64K which causes an overflow. Max is 80h pages.
_dowr:      LD    B,H
            EX    DE,HL           ; DE back to being the start address.
_nxblk:     PUSH  BC
            PUSH  HL
            LD    C,A_DSKDM
            RST   30h             ; Tell API where to get data.
            LD    DE,(SDMP_L)
            LD    HL,(SDMP_L+2)   ; HLDE is the logical address
            LD    A,(SDMP_MD)     ; The read command - turn into write command
            ADD   A_DSKWR-A_DSKRD
            LD    C,A
            RST   30h             ; Write sector

            ; Step forward to next sector
            LD    DE,(SDMP_L)
            INC   DE
            LD    (SDMP_L),DE
            ; Overflow to next page
            LD    A,D
            OR    E
            JR    NZ,_noov

            LD    HL,(SDMP_L+2)
            INC   HL
            LD    (SDMP_L+2),HL

_noov:      POP   HL              ; Move read address forward 512 bytes.
            LD    DE,512          ; sector size
            ADD   HL,DE
            POP   BC
            DJNZ  _nxblk          ; Write the next block

            JR    main

; ---- WBOOT
; Write a boot image to the boot manager. There are two options:
;   WB id DELETE     - Remove boot information with specified ID
;   WB id SDaddr laddr len execaddr MODE "name"
; id:       (dec) The unique boot ID. If there is already an image with this ID it must first be deleted
; DELETE:   If specified, delete any image with this ID
; SDaddr:   Where is this on the SDCard? Absolute or virtual drive relative.
; laddr:    (hex) Where to start loading into application space
; len:      (hex) Number of bytes to load
; execaddr: (hex) Run address
; MODE:     (chr) Set of characters identifying flags:
;                   S:  Standalone - doesn't use Zloader services. Launch and forget
;                   A: Image uses ZLoader API but not break handler
;                   B: Image uses Zloader API and serial I/O break handler should be installed (debugger)

PBUF        EQU   SCRATCH
DFLAG       EQU   SCRATCH+32
IMGPOS      EQU   SCRATCH+34

; WB 7 00006000 D600 2800 ECCD 01 CP/M
; WB 9 0:C D600 2800 ECCD 1 CPM TEST
            ; Prepare/check the boot records
WBOOT:      CALL  BTPREP
            JR    NZ,E_ERROR

            CALL  GET_DEC         ; MUST be an image ID, returned in HL
            JR    C,BADPS

            LD    IX,PBUF
            LD    C,L             ; 'C' contains the ID of the image we want
            LD    (IX+1),C
            LD    A,1
            LD    (IX),A

            CALL  WASTESPC
            LD    HL,(INPTR)
            LD    DE,_DEL
            CALL  STRCMP
            LD    (DFLAG),A       ; True if it's a delete request.

            CALL  FNDIMG
            LD    A,(DFLAG)     ; Get the delete flag back
            JR    Z,_fnd

            ; Not found so can be insert but delete leads to not found.
            OR    A
            JR    Z,E_NOTF

            ; Inserting. Need the data. HL currently points at the start of the new
            ; record. Save this and build a dummy in the SCRATCH buffer.
            LD    (IMGPOS),HL

            ; Get the parameters
            ; 1: SDAddr
            CALL WASTESPC
            CALL SADDR
            JR   C,BADPS

            ; The read command is either relative (for a mapped drive reference) or
            ; raw for an absolute sector address.
            LD   A,(SDMP_MD)
            CP   A_DSKRD        ; Relative read
            LD   A,0
            LD   (IX+10),A      ; Clear drive number (abs can only reference SDCard 1)
            LD   DE,(SDMP_L)
            LD   HL,(SDMP_L+2)
            JR   NZ,_sdabs
            CALL SDMPADD
            LD   (IX+10),A      ; Store drive number
            LD   A,$80          ; It's a logical address so translate to segment
_sdabs:     LD   (IX+11),E      ; SDCard address
            LD   (IX+12),D
            LD   (IX+13),L
            LD   (IX+14),H
            LD   (IX+21),A      ; Flags

            ; 2: Load address
            CALL WASTESPC
            CALL INHEX_4
            JR   C,BADPS
            LD   (IX+15),L
            LD   (IX+16),H

            ; 3: Length
            CALL WASTESPC
            CALL INHEX_4
            JR   C,BADPS
            LD   (IX+17),L
            LD   (IX+18),H

            ; 4: Execute address
            CALL WASTESPC
            CALL INHEX_4
            JR   C,BADPS
            LD   (IX+19),L
            LD   (IX+20),H

            ; 5: Flag
            CALL WASTESPC
            CALL INHEX_2
            JR   C,BADPS

            AND  $7F           ; Clear bit 7 for our flag
            OR   A,(IX+21)     ; Mix in the virtual disk flag
            LD   (IX+21),A     ; and store....

            ; Rest of the line (max 8 characters) is the image name, pad with zeroes
            CALL WASTESPC
            LD   B,8
            LD   HL,PBUF+2
_nname:     CALL BUFCHR
            LD   (HL),A
            INC  HL
            DJNZ _nname

            LD   HL,PBUF+22
            LD   B,10
            XOR  A
_clr1:      LD   (HL),A
            INC  HL
            DJNZ _clr1

_ename:     ; Write into the available slot.
            LD    HL,PBUF
            LD    DE,(IMGPOS)
            LD    BC,32
            LDIR

_wrbos:     CALL  _calc_cs

            ; Write checksum into page buffer
            LD    A,$FF
            LD    (SDPAGE+480),A
            LD    (SDPAGE+481),HL

            ; Patched, calculate checksum and write back to the SDCard
            LD    C,S_DSKDM
            RST   30h
            LD    HL,0
            LD    DE,0
            LD    C,A_DSKWW
            RST   30h

            JR    _dumpbs

            ; Image exists. Allowed to be a delete. Error if insert
_fnd:       OR    A

            ; Zero means it's a delete request
            JR    NZ,E_NEMPT

            ; It's a delete request
_dosdel:    PUSH  HL
            LD    HL,_TODEL
            CALL  PRINT
            POP   HL
            PUSH  HL
            INC   HL
            INC   HL
            LD    B,8
_nc1:       LD    A,(HL)
            RST   08h
            INC   HL
            DJNZ  _nc1
            CALL  NL
            ; Clear the flag
            POP   HL
            XOR   A
            LD    (HL),A
            JR    _wrbos


; ---- LDISASS
; Call DISASS, but the code resides in the extended page. Map this into page 3 and then call the
; code. Don't need to map the page back again afterwards.
LDISASS:    PUSH  DE
            LD    DE,$0300 | MN2_PG
            CALL  MAPMPG
            POP   DE
            JP    DISASS

; ---- LDISASS
; Call DISASS, but the code resides in the extended page. Map this into page 3 and then call the
; code. Don't need to map the page back again afterwards.
LSETOFF:    PUSH  DE
            LD    DE,$0300 | MN2_PG
            CALL  MAPMPG
            POP   DE
            JP    SETOFF


; ---- _EISR
; Serial port rx ISR
_EISR:    CALL    SERINT
          EI
          RETI





; --------------------- STRINGS
_INTRO:   DEFB "Z80 ZIOS 1.18.9",NULL
; _INTRO:   DEFB ESC,"[2J",ESC,"[H",ESC,"[J",ESC,"[1;50rZ80 ZIOS 1.18.8",NULL
_CLRSCR:  DEFB ESC,"[2J",ESC,"[1;50r",NULL

; Set scroll area for debug
_DBMD:    DEFB ESC,"[2J",ESC,"[1m",ESC,"[1;6HSTACK",ESC,"[m",ESC,"[11;50r",ESC,"[9;1H  >",ESC,"[12,1H",NULL

_PROMPT:  DEFB "> ",0
_UNKWN:   DEFB "unknown",0
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
_ERROR       DEFB "Error",0
_nxtblk$     DEFB CR,"Block: ",0
_BOOTHEX     DEFB "boot.ihx",0
_BSD         DEFB "Image: ", 0
_FULL:       DEFB "Full",NULL
_SDINFO:     DEFB "ID NAME     DV SDADDR   LOAD  LEN EXEC FL",NULL
_APPPG:      DEFB "App pages: ", NULL
_yes:        DEFB "YES", NULL
_no:         DEFB "NO", NULL
_NODRV       DEFB "No OS", NULL
_TMPBC       DEFB "Temp: @", NULL
_PRMBC       DEFB "BP:   @", NULL
_BRK         DEFB "BREAK...", NULL
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
_DEL:        DEFB "DELETE", NULL
_SFND:       DEFB "Found", NULL
_NOTF:       DEFB "Not found", NULL
_NOTEMP:     DEFB "Not empty", NULL
_TODEL:      DEFB "Delete image: ", NULL

; Alternate command table format: LETTER:ADDRESS
BDG_TABLE:      DB       'B'+80h
                DW        BP
                DB       'N'+80h
                DW        NSTEP
                DB       'R'+80h
                DW        SET_RGS
                DB       'S'+80h
                DW        SSTEP
                DB       'G'+80h
                DW        GO

CMD_TABLE:      DB       'B','O','S','?'+80h
                DW        SDDIR
                DB       'B','O','S','-'+80h
                DW        SDLOAD
                DB       'B','O','S'+80h
                DW        SDRUN
                DB       'B','O','-'+80h
                DW        BOOTIX
                DB       'B','O'+80h
                DW        BOOT
                DB       'C'+80h
                DW        CONFIG
                DB       'D','M'+80h
                DW        DUMPM
                DB       'D','I'+80h
                DW        DUMPI
                DB       'D','N'+80h
                DW        DNVRAM
                DB       'D','T'+80h
                DW        DTIME
                DB       'D'+80h
                DW        DUMP
                DB       'F'+80h
                DW        FILL
                DB       'H'+80h
                DW        CLS
                DB       'I'+80h
                DW        INPUT
                DB       'L','F'+80h
                DW        LDF
                DB       'L','H'+80h
                DW        LDH
                DB       'L'+80h
                DW        LDT
                DB       'M','S'+80h
                DW        SDMOD
                DB       'M'+80h
                DW        MODIFY
                DB       'N'+80h
                DW        NSTEP
                DB       'O'+80h
                DW        OUTPUT
                DB       'P'+80h
                DW        PAGE              ; Display/change application page assignment
                DB       'Q'+80h
                DW        DECCHR
                DB       'S','D'+80h
                DW        SDUMP             ; Display SDCard sector contents
                DB       'S','M'+80h
                DW        SMAP              ; Map a logical to physical SD card.
                DB       'S','W'+80h
                DW        SWRITE            ; Write to an SDCard sesctor
                DB       'S'+80h
                DW        MAPDSK            ; Map a logical to physical SD card.
                DB       'T'+80h
                DW        EXDBG
                DB       '.'+80h
                DW        SHWHIST
                DB       'G'+80h
                DW        RUN
                DB       'W','B'+80h
                DW       WBOOT
                DB       'W','I'+80h
                DW       IMG
                DB       '?'+80h
                DW        HELP
                DB        0

; ---------- CFG_TAB
; Set of boolean flags that can be configured in NVRAM. Format is:
; MASK | ByteOffset | Desc | 0
CFG_TAB:        DEFB      00000001b                ; Bit 0
                DEFB      0
                DEFB      "Install OS: ",0

                DEFB      00000010b                ; Bit 1
                DEFB      0
                DEFB      "Break handler: ",0

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

; Names for Z80 flag register
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
LOAD_CNT   DEFW    0
APP_STK:   DEFS    2              ; Place to store the applications stack pointer before an IOS call
LOADADD:   DEFS    2              ; For the load command, where to start loading binary data
INITD:     DEFS    1              ; Set to '1' once OS initialissation for an application has been done.
OPMODE:    DEFS    1              ; Operational mode. 1=normal. 2=debug
CMDTAB:    DEFS    2              ; Operational mode. 1=normal. 2=debug
LAST_CMD:  DEFS    1
DUMP_ADDR: DEFS    2
DUMP_MODE: DEFS    1
ISRCTXT:   DEFS    1              ; True if the current ISR was called when in application space
CTRLCH:    DEFS    1
; DUMP_CHRS: DEFS    2
           ; DEFS   16
           ; DEFS    1






; Define a 256 byte RAM block to receive data from the Pi file system
FIN_CODE:  DEFB    0
AUTO_RUN:  DEFB    0

FULBLKS    EQU     SCRATCH
LASTB      EQU     SCRATCH+1
SDDRV      EQU     SCRATCH+3
SDADDR     EQU     SCRATCH+4
EXECADD    EQU     SCRATCH+8
SLDMODE    EQU     SCRATCH+10
PROGADD    DEFS    2
PROGPG     DEFS    1

SDPAGE     DEFS    512

; Record the LAST SD sector to be dumped. These need to persist over commands. The
; address (SDML_L) is either a virtual drive address OR a raw address. The content
; of SDMP_MD indicates which. If the address is raw/absolute hen it's simply a
; 32 bit sector (512 byte block) address. If it's virtual then it's in the standard
; virtual format which includes a logical drive number.
SDMP_L     DEFW    $FFFF, $FFFF
SDMP_MD    DEFB    $FF   ; Read mode. This is the API command either A_DSKRD (virtual) or A_DSKRW(raw)

DDMP_MDE:  DEFS    $FF

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
SCRATCH    DEFS  256      ; 256 bytes scratch pad area for working commands

; Aliases for the scratch area
FILE_BUF   EQU   SCRATCH
DUMP_CHRS  EQU   SCRATCH


; Single Step Count...
STP_CNT    DEFS    2      ; Up to 64K steps!!!
STP_IN:    DEFS    1      ; Temp store for setting a BP. True means step into and CALL ops.


_ENDDS     EQU     $
.END
