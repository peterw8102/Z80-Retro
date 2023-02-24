import defs.asm
import config.asm
import zios.asm
import zlib.asm
import zload.asm


          ; Imports from 'zloader'
          extrn  main,BADPS,OPMODE,SDPAGE

          ; Exports
          public FILL,INPUT,OUTPUT,MODIFY,SDMOD,HELP

; ------------------- _bfill
; Fill from HL, length BC value A
_bfill:   PUSH  DE
          PUSH  AF
          CALL  P_MAPX
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
          LD    (SCRATCH),A

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
          LD    A,(SCRATCH) ; Fill value
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
; ------------------- SDMOD
; Modify data in the SDCard buffer. The parameter is the offset into the SDCard buffer
; with a base of zero.
SDMOD:   CALL  WASTESPC
         CALL  GET_HEX          ; Offset into the SDCard buffer
         JR    Z, BADPS

         ; Sector size is 512 buyes so anthing greater than 0x200 is out of range.
         LD    A,$FE
         AND   H
         JR    NZ,BADPS

         ; Set up the addresses to display and use
         LD    DE,SDPAGE        ; Calculate the address in the buffer (SDPAGE+offset)
         EX    DE,HL
         ADD   HL,DE            ; HL now actual address, DE is display offset
         JR    _nextln

;
; ------------------- MODIFY
MODIFY:  CALL  WASTESPC
         CALL  GET_HEX          ; Start address (in application space)
         JR    Z, BADPS
         ; Sit in a loop processing lines.
         ; LD    (DUMP_ADDR),HL

         ; Make a copy in DE
         LD    D,H
         LD    E,L
         CALL  P_MAPX     ; Translate HL into an offset and a page number and map application pages into memory

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
_eoln:   CALL  NL
         LD    A,B
         OR    A       ; Did we get any bytes?
         JR    NZ,_nextln
         JR    main

_skdol:  CALL  BUFCHR
         JR    _nexthx

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

_fill_msg:   DEFB "Fill: ADDR: ",NULL
_fill_sz:    DEFB " LEN:",NULL
_fill_wt:    DEFB " WITH:",NULL

_HTEXT:         DEFB 2,"B XXXX      set BP",0
                DEFB 1,"BO-         load block protocol file (no exec)",0
                DEFB 1,"BO          as BO- but run the loaded file",0
                DEFB 1,"BOS [n]     SDCard boot",0
                DEFB 1,"BOS- [n]    SDCard load image",0
                DEFB 1,"BOS?        List boot images",0
                DEFB 3,"C           Config parameters",0
                DEFB 3,"DI XXXX     Disassemble",0
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
                DEFB 1,"SD          Display SDCard drive map",0
                DEFB 1,"SD XXXX     Display SDCard sector",0
                DEFB 1,"SB          Save Bootable Image to SDCard",0
                DEFB 1,"SW          Write SDCard sector",0
                DEFB 3,"WB          Add bootable image",0
                DEFB 3,"WI          Write image to SD",0
                DEFB 0
