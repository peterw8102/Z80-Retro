import ../zlib/defs.asm


; import libsio.asm

; External dependancies
; RST 08h - Write a single character to the terminal, character in A
; RST 10h - Read one character from the terminal. Block until there is a character. Returned in A
; RST 18h - Check whether there is a character available. Non blocking. Z flag set if there is NOT a character waiting.

          extrn  PRINT,PRINT_LN,GET_LINE,SKIPSPC,WASTESPC,BUFCHR,WRITE_8,WRITE_16,INHEX_2,INHEX_4,GET_HEX,INPTR,INBUF,INITSIO
          public START,PAGE_MASK

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
           XOR   A
           OUT   ($60),A
           LD    A,$03
           OUT   ($64),A
           ; And go to paged mode.
           ; 1. TEST FOR. Map page 1F to bank 3. if writeable then RAM LOW
           ; LD    A,$1F
           ; OUT   ($63),A
           ; Check to see if BANK 0 is writable
           LD    A,$55
           LD    ($3F00),A
           LD    A,($3F00)
           CP    $55
           JR    NZ,FLASH_LOW
           LD    A,$AA
           LD    ($3F00), A
           LD    A,($3F00)
           CP    $AA
           ; If we read 0xAA then RAM is in the low 32 pages
           JR    NZ, FLASH_LOW

SET_RAM:   ; Store 0x00 as the mask to use for RAM page selection
           XOR   A
           LD    (PAGE_MASK), A

           ; And ready to go...
           JR    RUN_CLI

FLASH_LOW: ; 4. We're running from Flash. Copy to RAM page zero and map that to block zero.
            LD    A,$55
            LD    ($3F00), A
            LD    A,$AA
            LD    ($3F00), A
            LD    A,($3F00)
            CP    $AA
            ; If we read back AA then we're running from RAM so ready to go
            JR    Z,SET_FLASH
            ; 5. Copy BANK 0 to PAGE 20 (start RAM)
            LD    A,$20
            OUT   ($63),A
            LD    HL,0
            LD    DE,$C000
            LD    BC,$4000
            LDIR
            ; Switch page 1F to page zero (which should now be the same content)
            LD    A,$20
            OUT   ($60),A

SET_FLASH:  LD    A,$20
            LD    (PAGE_MASK), A


RUN_CLI:    ; Give ourselves a stack at the end of our reserved page.
            LD    HL,$3FF8
            LD    SP,HL
            ; JR    NOSIO

            ; Initialise the serial IO module
            CALL  INITSIO

; Flash the LEDs a few times to show we're working.
            LD   B,32
            LD   C,$A0
            LD   A,$00
NEXT_L:     OUT  (C),A
            LD H,03fH
loop2:      LD L,0ffH
loop3:      DEC L
            JR NZ,loop3
            DEC H
            JR NZ,loop2
            INC  A
            DJNZ NEXT_L

          ; Really simple CLI now. Display
NOSIO:    LD    HL, _INTRO
          CALL  PRINT_LN

main:     LD    HL, _PROMPT
          CALL  PRINT
          CALL  GET_LINE

          WRITE_CRLF
          CALL  SKIPSPC

          ; Which command? Only support L(oad) and R(un)
          LD    C,A
          LD    HL,CMD_TABLE
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
          JR    main

; ------------------- Port output
OUTPUT:   CALL  WASTESPC
          CALL  INHEX_2
          JR    C,main
          LD    C,A
          PUSH  BC
          CALL  WASTESPC
          CALL  INHEX_2
          JR    C,main
          PUSH  AF
          LD    HL,_OUTMSG
          CALL  PRINT
          LD    A,C
          CALL  WRITE_8
          WRITE_CHR '='
          POP   AF
          PUSH  AF
          CALL  WRITE_8
          POP   AF
          POP   BC
          OUT   (C),A
          WRITE_CRLF
          JR    main

; ------------------- fill
FILL:     CALL  GET_HEX          ; Address
          JR    Z, f_err
          LD    D,H
          LD    E,L              ; DE: Address
          CALL  WASTESPC
          CALL  GET_HEX          ; Length
          JR    Z,f_err
          LD    B,H              ; BC: Count
          LD    C,L
          CALL  WASTESPC
          CALL  INHEX_2
          PUSH  AF
          LD    HL,_fill_msg
          CALL  PRINT
          LD    H,D
          LD    L,E
          CALL  WRITE_16
          LD    HL,_fill_sz
          CALL  PRINT
          LD    H,B
          LD    L,C
          CALL  WRITE_16
          LD    HL,_fill_wt
          CALL  PRINT
          POP   AF
          PUSH  AF
          CALL  WRITE_8
          WRITE_CRLF
          ; Fill memory...
          ; DE: address
          ; BC: count - Limit to 1K
          ; A:  fill value
          POP   AF

          LD    H,D
          LD    L,E
          INC   DE          ; HL = DE+1
          LD    (HL),A      ; Write the first byte

          ; MUST be a value > 1 otherwise the whole of memory is wiped.
          DEC   BC
          LD    A,B
          OR    C
          JR    Z,_fone
          LDIR
_fone:    JR    main;

f_err:    LD    HL,_fill_err
          CALL  PRINT_LN
          JR    main

; ------------------- run
DUMP:     LD    HL, (DUMP_ADDR)
          CALL  WASTESPC
          JR    Z,cnt_dump

no_mode:  CALL  GET_HEX           ; OFFSET to apply to hex records. Z flag set if 'break' pressed.
cnt_dump: ; Display 8 blocks of 16 characters
          LD    C,8

          ; Dump address (start of line)
dloop2:   CALL  WRITE_16          ; 4 hex digits from HL
          WRITE_CHR SPC
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
          LD    A, (HL)
writeout: INC   DE
          INC   HL
          CALL  WRITE_8
          WRITE_CHR SPC
          DJNZ  dloop
          PUSH  HL
          LD    HL, DUMP_CHRS
          CALL  PRINT
          POP   HL
          WRITE_CRLF
          DEC   C
          JR    NZ,dloop2
          LD    (DUMP_ADDR), HL
          JR    main


; ----- Load a hex file (from the console input)
invalid:  LD    HL, _NOSTART
          JR    _prterr
impeof:   LD    HL,_COMPLETE
          JR    _prterr
rec_err:  LD    HL,_REC_ERR
_prterr:  CALL  PRINT_LN
          JR    main

; Process all lines starting with a ':'
LOAD:     LD    HL, _WAITING
          CALL  PRINT_LN
          LD    A,(PAGE_MASK)
          LD    D,A
nextline: CALL  GET_LINE
          JR    Z, nextline
          CALL  BUFCHR
          CP    ':'
          JR    NZ,invalid
          ; Accept this line. Format: [LEN 1][ADDR 2][REC_TYPE 1][DATA 2]+[CHKSM 1]
          CALL  INHEX_2            ; Length -> A
          JR    C, rec_err
          LD    B,A                ; Length into B
          CALL  INHEX_4            ; Address - HL
          JR    C, rec_err
          ; CALL  here
          CALL  INHEX_2            ; Command - which should be 00. If not then EOF so end.
          JR    C, rec_err
          OR    A
          JR    NZ, impeof
          LD    A,B                ; Check for zero length
          OR    A
          JR    Z,rec_err

          ; The upper two bits of the address identifies the page block.
          LD    A,H
          RLCA
          RLCA
          AND   03h
          INC   A     ; Using page 1-4 (zero is reserved for the monitor!)
          ; This is the logical RAM page. Need to add in the page offset for the current configuration
          ADD   A,D
          ; Map this page into block 1
          OUT   ($61),A
          ; Also write to the LEDs as a vidual check
          OUT   ($A0),A
          ; Write out the selected page.
          PUSH  HL
          PUSH  AF
          LD    HL,_page
          CALL  PRINT
          POP  AF
          PUSH  AF
          CALL  WRITE_8
          LD    HL,_effaddr
          CALL  PRINT
          POP   AF
          POP   HL
          ; And map the address in HL so it's in BLOCK 1.
          LD    A,3Fh
          AND   H
          ; Set bit 6
          OR    A,40h
          ; And move back to the address
          LD    H,A
          CALL  WRITE_16
          WRITE_CRLF

          ; And write these bytes into that page
next_b:   CALL  INHEX_2            ; Get a hex byte
          JR    C, rec_err
          LD   (HL), A
          INC   HL
          DJNZ  next_b
          ; END of that record (ignoring checksum)
          WRITE_CRLF
          JR    nextline

; ------------------- RUN
; This initial version loads the first four RAM pages into the four banks, reserving the last 16 bytes of
; page 03 to store an address switcher. Once switch it jumps to address 0 to run that code.
RUN:      LD    C,$61       ; Start with block 1, we're running from block 0!
          LD    A,(PAGE_MASK)
          ADD   $02
          OUT   (C),A
          INC   C
          INC   A
          OUT   (C),A
          INC   C
          INC   A
          OUT   (C),A
          ; Running from page 0 so we have to transfer execution to the end of page 3.
          LD    HL,DO_START
          LD    DE,$FFF0
          LD    BC,STARTEND-DO_START+1
          LDIR
          LD    A,(PAGE_MASK)
          JR    $FFF0

DO_START: LD    C,$60
          ADD   1
          OUT   (C),A
          RST   00h
STARTEND:

; -- _map_flsh
; Map FLASH pages 0 and 1 to baks 2 and 3 (top 32K)
_map_flsh: LD   A,(PAGE_MASK)
           XOR  $20
           OUT  ($62),A
           INC  A
           OUT  ($63),A
           RET

; ------ ROUTINES TO COPY MONITOR TO FLASH --------
FLASH_OP: CALL  SKIPSPC
          JR    Z,flsh_id
          CP    'M'
          JR    Z,flsh_m
          CP    'I'
          JR    Z,flsh_id
          CP    'P'
          JR    Z,flsh_prg
          CP    'Z'
          JR    Z,flsh_clr

; ------------------- Read Flash Info
flsh_id:  WRITE_CRLF
          CALL  _map_flsh
          ; --
          LD    A,90h
          CALL  FLSH_CMD
          ;--
          LD    HL,8000h
          LD    A,(HL)
          CALL  WRITE_8
          WRITE_CHR '-'
          LD    HL,8001h
          LD    A,(HL)
          CALL  WRITE_8
          WRITE_CRLF
          ; -- EXIT
          LD    A,0F0h
          CALL  FLSH_CMD
          JR    main

flsh_m:   CALL  _map_flsh
          JR    main
; ------------------- flsh_clr
flsh_clr: CALL  _map_flsh
          CALL  _flsh_clr
          JR    main

; ----------------------------------- flsh_prg
; Expect a start address in page zero and a length. Writes data to the high page
flsh_prg: LD    HL,_FLSH_PRG
          CALL  PRINT_LN
          CALL  _map_flsh

          ; FROM address is always 4000 - map source page to bank 1
          LD    DE,$4000
          ; And it's always written to 8000 (single page only)
          LD    HL,$8000
          LD    (FL_TO_ADDR),HL  ; TO ADDRESS saved
          ; The length is always a single page ($4000)
          LD    BC,$4000
          LD    HL,_prg_msg      ; "FROM ADDRESS: "
          CALL  PRINT
          LD    H,D
          LD    L,E
          CALL  WRITE_16
          LD    HL,_prg_msgto
          CALL  PRINT
          LD    HL,(FL_TO_ADDR)   ; Get the TO address to display
          CALL  WRITE_16
          LD    HL,_prg_msglen
          CALL  PRINT
          LD    H,B
          LD    L,C
          CALL  WRITE_16
          WRITE_CRLF
          ; Delete page 0 in flash
          XOR   A
          ; CALL  _flsh_clr
          ; Write data
          LD    HL,(FL_TO_ADDR)   ; Get the TO address to display
loopp:    LD    A,(DE)            ; Get a source bytes
          CALL  _flsh_bt          ; And programme it into address (HL)
          INC   HL
          INC   DE
          DEC   BC
          LD    A,B
          OR    C
          JR    NZ,loopp          ; Keep going until there are no bytes to send
          JR    main;
; ----------------------------------- _flsh_clr
; Erase page 0 (mapped to 8000 - 4K page)
_flsh_clr: CALL _map_flsh
           LD   A,80h
           CALL FLSH_CMD
           LD   A,30h
           LD   DE,8000h
           CALL _flsh_cmd
           CALL _flsh_poll
           RET
; ----------------------------------- _flsh_poll
; Poll for write operation complete
_flsh_poll:PUSH  HL
           LD    HL,8000h
_flsh_lp1: LD    A,(HL)
           XOR   (HL)
           BIT   6,A
           JR    NZ,_flsh_lp1
           POP   HL
           RET

; ----------------------------------- _flsh_bt
; Write a single byte to flash. HL: Address, A: data
_flsh_bt: PUSH  AF
          LD    A,0A0h
          CALL  FLSH_CMD  ; Flash CMD
          POP   AF
          LD    (HL),A
          CALL  _flsh_poll
          RET

FLSH_CMD:  PUSH  DE
           LD    DE,0D555h
           CALL  _flsh_cmd
           POP   DE
           RET
; ----------------------------------- _flsh_cmd
; Write a command sequence to FLASH. The flash page must be preselected in page 2. The command
; byte to be written is in the accumulator. No registers overwritten except A.
_flsh_cmd: PUSH  HL
           ; --
           LD    HL, 0D555h
           LD   (HL),0AAh
           ; --
           LD    HL, 0AAAAh
           LD   (HL),55h
           ; --
           LD    H, D
           LD    L, E
           LD   (HL),A
           POP   HL
           RET




; --------------------- STRINGS
_INTRO:   DEFB ESC,"[2J",ESC,"[H",ESC,"[J",ESC,"[1;50rZ80 Loader 1.0.5",CR,LF,"Ready...",CR,LF,NULL
_PROMPT:  DEFB "> ",0
_ERROR:   DEFB "unknown",0
_NOSTART: DEFB CR,LF,"Missing ':'",NULL
_OUTMSG: DEFB CR,LF,"Out: ",NULL
_REC_ERR: DEFB CR,LF,"Bad rec",NULL
_WAITING: DEFB "Waiting...",NULL
_COMPLETE:DEFB CR,LF,"Complete",0

; VT100 sequences
SAVE_POS:  DEFB ESC,"7",NULL
REST_POS:  DEFB ESC,"8",NULL
CURS_UP:   DEFB ESC,"[A",NULL
HOME:      DEFB ESC,"[H",NULL
STK_NXT:   DEFB CR,LF,ESC,"[3C",NULL ; Down one line then to character 2
COLSTR:    DEFB CR,ESC,'[25C',NULL

FLAGS_DESC:  DEFB "SZ5H3VNC",NULL

_fill_err:   DEFB "Bad param",NULL
_fill_msg:   DEFB "Fill: ADDR: ",NULL
_fill_sz:    DEFB " LEN:",NULL
_fill_wt:    DEFB " WITH:",NULL
_prg_msg:    DEFB "FROM ADDRESS: ",NULL
_prg_msgto:  DEFB ", TO: ",NULL
_prg_msglen: DEFB ", LEN: ",NULL
_nobpavail:  DEFB "Full",NULL
_page        DEFB CR,LF,"Page: ",NULL
_effaddr     DEFB ", Addr: ",NULL
_FLSH_PRG:   DEFB "Flash prog",0

; Alternate command table format: LETTER:ADDRESS
CMD_TABLE:      DB       'L'
                DW        LOAD
                DB       'R'
                DW        RUN
                DB       'O'
                DW        OUTPUT
                DB       'F'
                DW        FILL
                DB       'D'
                DW        DUMP
                DB       'P'
                DW        FLASH_OP          ; Copy RAM monitor to flash
                DB        0


           DSEG
; ---------------------- VARIABLES
; If you want the monitor code in ROM then add an ORG here to locate variables somewhere in RAM
LAST_CMD:  DEFB    'R'
DUMP_ADDR: DEFW    0
DUMP_MODE: DEFB    'I'
DUMP_CHRS: DEFB  "  "
           .DS   16
           DEFB    0

; JP_RUN - C3 is the JP opcode. By jumping to JP_RUN execution will
; continue from the current value of the PC. This avoids us having to
; push values onto the applications stack.
JP_RUN:    DEFB    0

LAST_ADDR: DEFB    0
PAGE_MASK: DEFB    0
FL_TO_ADDR DEFW    0

.END
