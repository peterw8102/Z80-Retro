import ../zlib/defs.asm
import config.asm

          extrn  PRINT,PRINT_LN,GET_LINE,SKIPSPC,WASTESPC,BUFCHR,WRITE_8,WRITE_16,INHEX_2,INHEX_4,MAPCASE
          extrn  BRK_HK
          extrn  CMD_B
          extrn  DISASS, SETOFF
          extrn  GET_HEX,GET_DEC,INPTR,INBUF,INITSIO,MAPCASE,RXA,TXA,CKINCHAR,SERINT

          ; Include SPI/SDCard library
          extrn  SD_INIT, SD_RBLK, SD_WBLK

          ; From the i2c library
          extrn  I2C_WT

          ; And the RTC/i2c library
          extrn  RTC_INI, RTC_MRD, RTC_MWR

          extrn  PGMAPX,main,BADPS,NL,PAGE_MASK,OPMODE,PGMAPX

          public FILL,INPUT,OUTPUT,MODIFY,FLASH_OP,HELP

; ------------------- _bfill
; Fill from HL, length BC value A
_bfill:   PUSH  DE
          PUSH  AF
          CALL  PGMAPX
          LD    D,H
          LD    E,L
          INC   DE
          POP   AF
          LD    (HL),A
          LDIR
          POP   DE
          RET

; ------------------- fill
FILL:     CALL  GET_HEX          ; Address
          JR    Z, BADPS
          LD    D,H
          LD    E,L              ; DE: Address
          CALL  WASTESPC
          JR    Z,BADPS
          CALL  GET_HEX          ; Length
          JR    Z,BADPS
          LD    B,H              ; BC: Count
          LD    C,L
          CALL  WASTESPC
          JR    Z,BADPS
          CALL  INHEX_2
          JR    C,BADPS
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
          CALL  NL

          ; Save values
          POP   AF
          LD    (FILL_WT),A

          ; Fill memory...
          ; DE: address
          ; BC: count - Limit up to 64K - any size
          ; A:  fill value
          DEC   BC         ; To get a sensible range

_nfblk:   LD    A,B
          OR    C
          JR    Z,main      ; No bytes left to fill so exit
          PUSH  BC          ; Save total count
          LD    A,B
          AND   $C0
          JR    Z,_fsmblk   ; Block size is < 16K

          ; Dealing with a block > 16K so limit to 16K
          LD    BC,$4000

_fsmblk:  ; Decrease the total count by the number of bytes in this block.
          POP   HL          ; Get the original total remaining count
          LD    A,L         ; Will always be 0 after
          SUB   C
          LD    L,A
          LD    A,H
          SBC   B
          LD    H,A
          PUSH  HL          ; Save adjusted count on stack

          ; Add the block start address ready for the next block (if any)
          LD    H,D
          LD    L,E         ; Fill address -> HL
          LD    A,L
          ADD   C
          LD    L,A
          LD    A,H
          ADC   B
          LD    H,A
          PUSH  HL          ; Save the start of the next block. Stack: Ptr next block, remianing fill size

          ; Fill this block
          LD    A,(FILL_WT) ; Fill value
          EX    DE,HL       ; Target address into HL
          CALL _bfill

          ; Get ready for the next block
          POP   DE
          POP   BC

_fone:    JR    _nfblk;

; ------------------- Port output
OUTPUT:  CALL  WASTESPC
         CALL  INHEX_2
         JR    C,BADPS
         LD    C,A
         PUSH  BC
         CALL  WASTESPC
         CALL  INHEX_2
         JR    C,BADPS
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
         CALL  NL
         JR    main

; ------------------- Port input
INPUT:   CALL  WASTESPC
         CALL  INHEX_2
         JR    C,BADPS
         LD    C,A        ; Port number
         LD    HL,_INMSG
         CALL  PRINT
         LD    A,C
         CALL  WRITE_8
         WRITE_CHR '='
         IN    A,(C)
         CALL  WRITE_8
         CALL  NL
         JR    main

;
; ------------------- MODIFY
MODIFY:  CALL  GET_HEX          ; Start address (in application space)
         JR    Z, BADPS
         ; Sit in a loop processing lines.
         ; LD    (DUMP_ADDR),HL

         ; Make a copy in DE
         LD    D,H
         LD    E,L
         CALL  PGMAPX     ; Translate HL into an offset and a page number and map application pages into memory

         ; HL: Mapped address to write to
         ; DE: The original address that we should display
_nextln: EX    DE,HL
         CALL  WRITE_16   ; Display the application space address
         EX    DE,HL      ; Back to mapped address
         WRITE_CHR ':'
         WRITE_CHR ' '
         CALL  GET_LINE
         LD    B,0
_nexthx: CALL  WASTESPC
_skhx:   JR    Z, _eoln
         CP    '$'         ; Ignore $ and ','
         JR    Z,_skdol
         CP    ','
         JR    Z,_skdol
         CALL  INHEX_2
         JR    C, _eoln    ; Carry: error, no hex digits
         ; Have a value to write...
         LD    (HL),A      ; Storing in mapped address location
         INC   HL
         INC   DE          ; Keep raw address in sync
         INC   B
         JR    _nexthx
_eoln:   WRITE_CRLF
         LD    A,B
         OR    A       ; Did we get any bytes?
         JR    NZ,_nextln
         JR    main

_skdol:  CALL  BUFCHR
         JR    _nexthx
;
; -- _map_flsh
; Map FLASH pages 0 and 1 to banks 2 and 3 (top 32K)
_map_flsh: LD   A,(PAGE_MASK)
           XOR  RAM_PG_0     ; Toggling this bit switches A to FLASH page 0
           OUT  (PG_PORT0+2),A
           INC  A            ; And flash page 1
           OUT  (PG_PORT0+3),A
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
flsh_id:  CALL  NL
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
          CALL  NL
          ; -- EXIT
          LD    A,0F0h
          CALL  FLSH_CMD
          JR    main

flsh_m:   CALL  _map_flsh
          JR    main
; ------------------- flsh_clr
flsh_clr: CALL  _flsh_clr
          JR    main


; Programme first 8K into flash.
flsh_prg:  LD    HL,_FLSH_PG
           CALL  PRINT_LN
           CALL  _map_flsh  ; FLASH page 0 and 1 in top 32K
           LD    DE,4000h   ; From address
           LD    HL,8000h   ; To address
           CALL  _flsh_prg
           CALL  _wait
           LD    DE,5000h   ; From address
           LD    HL,9000h   ; To address
           CALL  _flsh_prg
           LD    DE,6000h   ; From address
           LD    HL,0A000h   ; To address
           CALL  _flsh_prg
           LD    DE,7000h   ; From address
           LD    HL,0B000h   ; To address
           CALL  _flsh_prg
           JR    main

; ----------------------------------- flsh_prg
; Programme a 4K page into Flash
; DE: Fromm address (RAM, 4000h or 5000h)
; HL: To address (Flash, 8000h or 9000h)
; Page size is ALWAYS 4K
_flsh_prg: LD    (FL_TO_ADDR),HL  ; TO ADDRESS saved
           ; The length is always a single page ($1000 or 4K)
           LD    BC,$1000
           LD    HL,_prg_msg      ; "FROM ADDRESS: "
           CALL  PRINT
           LD    H,D
           LD    L,E              ; From address from DE
           CALL  WRITE_16
           LD    HL,_prg_msgto
           CALL  PRINT
           LD    HL,(FL_TO_ADDR)   ; Get the TO address to display
           CALL  WRITE_16
           LD    HL,_prg_msglen
           CALL  PRINT
           LD    H,B               ; Length - always 4K
           LD    L,C
           CALL  WRITE_16
           CALL  NL
           ; Delete page 0 in flash - Not done here because page erase polling never works
           ; XOR   A
           ; CALL  _flsh_clr
           ; Write data
           LD    HL,(FL_TO_ADDR)   ; Get the TO address to display
loopp:     LD    A,(DE)            ; Get a source byte
           CALL  _flsh_bt          ; And programme it into address (HL)
           INC   HL
           INC   DE
           DEC   BC
           LD    A,B
           OR    C
           JR    NZ,loopp          ; Keep going until there are no bytes to send
           RET;
; ----------------------------------- _flsh_clr
; Erase page 0 and 1 (mapped to 8000 - 4K page x 2)
_flsh_clr: CALL _map_flsh
           LD   A,80h
           CALL FLSH_CMD
           LD   A,30h
           LD   DE,8000h         ; First 4K page to erase
           CALL _flsh_cmd
           CALL _flsh_poll
           CALL _wait
           LD   A,80h
           CALL FLSH_CMD
           LD   A,30h
           LD   DE,9000h         ; Second 4K page to erase
           CALL _flsh_cmd
           CALL _flsh_poll
           ; wait a while to be sure
           CALL _wait
           LD   A,80h
           CALL FLSH_CMD
           LD   A,30h
           LD   DE,0A000h         ; Third 4K page to erase
           CALL _flsh_cmd
           CALL _flsh_poll
           ; wait a while to be sure
           CALL _wait
           LD   A,80h
           CALL FLSH_CMD
           LD   A,30h
           LD   DE,0B000h         ; Last 4K page to erase (16K total)
           CALL _flsh_cmd
           CALL _flsh_poll
           ; wait a while to be sure
           CALL _wait
           RET

_wait:    PUSH  HL
          PUSH  BC
          LD    B,10h
_wl2:     LD    H,0
_wl1:     LD    L,0
_wl0:     DEC   L
          JR    NZ,_wl0
          DEC   H
          JR    NZ,_wl1
          DJNZ  _wl2
          POP   BC
          POP   HL
          RET
; ----------------------------------- _flsh_poll
; Poll for write operation complete
_flsh_poll:PUSH  HL
           LD    HL,8000h
_flsh_lp1: LD    A,(HL)
           XOR   (HL)
           RLCA
           RLCA
           JR    C,_flsh_lp1
           POP   HL
           RET

; ----------------------------------- _flsh_bt
; Write a single byte to flash. HL: Address, A: data
_flsh_bt:  PUSH  AF
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

;
; ------------------- HELP
HELP:     LD    HL,_HTEXT
          LD    A,(OPMODE)
          LD    B,A
_nline:   LD    A,(HL)
          OR    A
          JR    Z,main
          AND   B
          JR    Z,_skp2
          CALL  PRINT_LN
_nhlp:    INC   HL
          JR    _nline
_skp2:    INC   HL
          LD    A,(HL)
          OR    A
          JR    NZ,_skp2
          JR    _nhlp

; ---- TEXT MESSAGES ----
_OUTMSG:  DEFB CR,LF,"Out: ",NULL
_INMSG:   DEFB CR,LF,"In: ",NULL
_FLSH_PG: DEFB "Flash: ",0

_fill_msg:   DEFB "Fill: ADDR: ",NULL
_fill_sz:    DEFB " LEN:",NULL
_fill_wt:    DEFB " WITH:",NULL
_prg_msg:    DEFB "FROM ADDRESS: ",NULL
_prg_msgto:  DEFB ", TO: ",NULL
_prg_msglen: DEFB ", LEN: ",NULL

_HTEXT:         DEFB 2,"B XXXX      set BP",0
                DEFB 1,"BO-         load default block protocol file (no exec)",0
                DEFB 1,"BO          as BO- but auto-run the loaded file",0
                DEFB 1,"BOS [n]     SDCard boot",0
                DEFB 1,"BOS- [n]    SDCard load image (no exec)",0
                DEFB 1,"BOS?        List SDCard images",0
                DEFB 3,"C           View/set Config parameters",0
                DEFB 3,"DI XXXX     Disassemble mem",0
                DEFB 3,"DM XXXX     Dump mem",0
                DEFB 3,"DN          Dump NVRAM (56 bytes)",0
                DEFB 3,"DT          Dump date/time",0
                DEFB 3,"D           Dump more",0
                DEFB 3,"F ADD LN VV Fill LN bytes from ADD with VV",0
                DEFB 2,"G           Run until breakpoint",0
                DEFB 3,"H           Clear screen",0
                DEFB 3,"I XX        Read input port XX",0
                DEFB 1,"L           Load Intel hex from Stdin",0
                DEFB 1,"LF          Load binary file via the block protocol",0
                DEFB 1,"LH          Load hex file via the block protocol",0
                DEFB 3,"M XXXX      Modify memory",0
                DEFB 2,"N           Step over",0
                DEFB 3,"O XX YY     Output YY to port XX",0
                DEFB 3,"P bb=pp     Map page pp into bank bb",0
                DEFB 2,"R RR=VV[VV] Set register (8 or 16) to VVVV",0
                DEFB 2,"S           Step into next instruction",0
                DEFB 1,"SD XXXX     Display SDCard sector. Use XXXX:XXXX for 32 bit sectors",0
                DEFB 1,"SBI         Save Bootable Image to SDCard",0
                DEFB 3,"T           Toggle debugger",0
                DEFB 1,"X [ADDR]    eXecute application [from address]",0
                DEFB 0

          DSEG

FILL_WT    DEFS    1
FL_TO_ADDR DEFW    0
