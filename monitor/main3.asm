import config.asm
import defs.asm

import libsio.asm

; External dependancies
; RST 08h - Write a single character to the terminal, character in A
; RST 10h - Read one character from the terminal. Block until there is a character. Returned in A
; RST 18h - Check whether there is a character available. Non blocking. Z flag set if there is NOT a character waiting.

          .ORG    BRK_HANDLER
BP_RST     JP      DO_BP

; Number of breakpoint locations (in code)
NUM_BK    .EQU    64
BP_SIZE   .EQU     4

          .ORG    LOAD_ADDR

START:
if STACK != 0
          LD    HL,STACK
          LD    SP,HL
endif
          LD    HL, _INTRO
          CALL  PRINT
          LD    A,$C3
          LD    (JP_RUN),A
          LD    HL,INBUF
          LD    (INPTR),HL

          ; Clear all breakpoints
          LD    HL,BPOINTS
          LD    DE,BPOINTS+1
          LD    BC,NUM_BK*BP_SIZE
          XOR   A
          LD    (HL),A
clrn:     LDIR
          ; Display initial registers
          JP    SHOW_RGS
main:     LD    HL, _PROMPT
          CALL  PRINT
          CALL  GET_LINE

          WRITE_CRLF
          CALL  SKIPSPC
          OR    A
          JR    NZ, _newcmd
          ; Move to previous line
          LD    HL,CURS_UP
          CALL  PRINT
          LD    A,(LAST_CMD)
_newcmd:  LD    (LAST_CMD),A
          ; Is it a letter?
          CP    'Z'+1
          JR    NC,err    ; Greater than a 'Z'
          SUB   'A'
          JR    C,err     ; Less than an 'A'
          ADD   A,A
          ; Load from jump table
          LD    HL,CMD_TABLE
          LD    E,A
          LD    D,0
          ADD   HL,DE
          LD    E,(HL)
          LD    A,E
          INC   HL
          LD    D,(HL)
          OR    D
          JR    Z,err     ; null entry in jump table
          EX    DE,HL
          JP    (HL)
err:      LD    HL, _ERROR
          CALL  PRINT
          JR    main

; ------------------- copy - TBD
COPY:     JP    main
; ------------------- upgrade
; New image at 1000. Copy to zero as a single operation
UPGRADE:  LD    HL,DO_COPY
          LD    DE,LOADER
          LD    BC,COPYEND-DO_COPY+1
          LDIR
          JP    LOADER

DO_COPY:  LD    HL,MON_COPY
          LD    DE,0
          LD    BC,LAST_ADDR+1000h
          LDIR
          RST   00H  ; Probably won't get here!
COPYEND:
; ------------------- SET_RGS
; CMD: Set register value. R reg=val
; reg is A,B,C,D,E,H,L,BC,DE,HL,IX,IY
; val is an 8 or 16 bit hex value
SET_RGS:  ; Get the name of the register, one or two characters
          CALL  SKIPSPC
          JR    Z,_rend    ; nothing to use
          LD    D,A        ; First character (required)
          LD    E,0
          CALL  BUFCHR     ; either a space or '=' otherwise use it
          CP    '='
          JR    Z,_getval
          CP    ' '
          JR    Z,_8bit
          LD    E,A
_8bit:    ; Waste characters until '=' or end of line
          CALL  SKIPSPC
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
          EX    DE,HL
          JR    NZ,_set8
          ; 16 bit
          LD    (HL),E
          INC   HL
          LD    (HL),D
_rend:    JP    SHOW_RGS
_set8:    LD    (HL),E
          JR    _rend
_rerr:    LD    HL,_BAD_REG
          CALL  PRINT
          JP    main
_reg_addr:;DE contains the name of the register
          PUSH  HL
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
          OR    A        ; Clear Z flag
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
; ------------------- SHOW_RGS
SHOW_RGS: LD    HL,SAVE_POS
          CALL  PRINT
          LD    HL,R_PC_DESC
          CALL  PRINT
          LD    HL,(R_PC)
          CALL  WRITE_16

          LD    HL,R_SP_DESC
          CALL  PRINT
          LD    HL,(R_SP)
          CALL  WRITE_16

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

          LD    HL,R_A_DESC
          CALL  PRINT
          LD    A,(R_AF+1)
          CALL  WRITE_8

          LD    HL,R_BC_DESC
          CALL  PRINT
          LD    HL,(R_BC)
          CALL  WRITE_16

          LD    HL,R_DE_DESC
          CALL  PRINT
          LD    HL,(R_DE)
          CALL  WRITE_16

          LD    HL,R_HL_DESC
          CALL  PRINT
          LD    HL,(R_HL)
          CALL  WRITE_16

          LD    HL,R_IX_DESC
          CALL  PRINT
          LD    HL,(R_IX)
          CALL  WRITE_16

          LD    HL,R_IY_DESC
          CALL  PRINT
          LD    HL,(R_IY)
          CALL  WRITE_16

          ; UPDATE STACK CONTENT. Show one lower then the previous 7
          LD    HL,HOME
          CALL  PRINT
          LD    HL,(R_SP)
          LD    DE,15
          ADD   HL,DE
          EX    DE,HL
          LD    B,8
_nextstk: LD    HL,STK_NXT
          CALL  PRINT
          LD    H,D
          LD    L,E
          DEC   HL
          CALL  WRITE_16
          WRITE_CHR ':'
          WRITE_CHR ' '
          EX    DE,HL
          LD    D,(HL)
          DEC   HL
          LD    E,(HL)
          DEC   HL
          EX    DE,HL
          CALL  WRITE_16
          DJNZ  _nextstk

          LD    HL,REST_POS
          CALL  PRINT

          JP    main

; ------------------- bank
BANK:     CALL  IN_HEX_2          ; OFFSET to apply to hex records. Z flag set if 'break' pressed.
          JP    Z,main
          CALL  WRITE_8
          PUSH  AF
          LD    HL,_BANKMSG
          CALL  PRINT
          POP   AF
          CALL  WRITE_8
          OUT   (PAGE_REG),A
          WRITE_CRLF
          JP    main
; ------------------- BP: Set breakpoint
; Get address from command line. If not specfied then BP at the current PC value
BP:       CALL  GET_HEX          ; Address for breakpoint
          JP    Z, f_err
          ; Set a BP at the address in HL
          LD    A,2              ; BP type
          CALL  SETBP
          JP    main
; ------------------- MODIFY
MODIFY:   CALL  GET_HEX          ; Start address
          JP    Z, f_err
          ; Sit in a loop processing lines.
_nextln:  CALL  WRITE_16
          WRITE_CHR ':'
          WRITE_CHR ' '
          CALL  GET_LINE
          LD    B,0
_nexthx:  CALL  WASTESPC
          JR    Z, _eoln
          CALL  IN_HEX_2
          JR    Z, _eoln
          ; Have a value to write...
          LD    (HL),A
          INC   HL
          INC   B
          JR    _nexthx
_eoln:    WRITE_CRLF
          LD    A,B
          OR    A       ; Did we get any bytes?
          JR    NZ,_nextln
          JP    main

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
          CALL  IN_HEX_2
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
          LD    D,A
loop:     LD    (HL),D
          INC   HL
          DEC   BC
          LD    A,B
          OR    C
          JR    NZ,loop
          JP    main;
f_err:    LD    HL,_fill_err
          CALL  PRINT
          JP    main

; -- DECINST
; Decode and display single instruction. HL points to the sart of the instruction. Displays
; the HEX bytes for this instruction followed by a newline.
; INPUT:  HL - the address of the instruction
; OUTPUT: HL - First byte of _next_ instruction
; Registers not saved: A
DECINST:  PUSH  BC        ; INST_LEN returns instruction information we don't need in BC
          CALL  INST_LEN
          LD    B,A        ; A includes the number of bytes in this op
          CALL  WRITE_16   ; Write out the address
          WRITE_CHR SPC
_nextb:   LD    A,(HL)
          CALL  WRITE_8
          WRITE_CHR SPC
          INC   HL
          DJNZ  _nextb
          ; Row complete
          WRITE_CRLF
          POP   BC
          RET

; --------------------------- DUMP decode (instruction decode and display) MIGHT become
; a disassembler at some point.
decode:   LD    B,20       ; Number of instructions
_nexti:   CALL  DECINST
          DJNZ  _nexti
          LD    (DUMP_ADDR), HL
          LD    A,'I'
          LD    (DUMP_MODE), A
          JP    main
; ------------------- run
DUMP:     LD    A,(DUMP_MODE)
          LD    B,A               ; Accepted mode in 'B'
          LD    HL, (DUMP_ADDR)
          CALL  WASTESPC
          JR    Z,cont_dump

          ; Accept an 'M' or 'I' to set the mode.
          CP    'G'
          JR    C,no_mode
          CALL  SKIPSPC
          LD    B,A

no_mode:  CALL  GET_HEX           ; OFFSET to apply to hex records. Z flag set if 'break' pressed.
          JR    Z, cont_dump

cont_dump:LD    A,B
          CP    'M'
          JR    NZ,decode
          CALL  BUFCHR
          ; Display 5 blocks of 16 characters
          LD    C,8

          ; Dump address (start of line)
dloop2    WRITE_CRLF
          CALL  WRITE_16          ; 4 hex digits from HL
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
          DEC   C
          JR    NZ,dloop2
          LD    (DUMP_ADDR), HL
          LD    A,'M'
          LD    (DUMP_MODE), A
          WRITE_CHR CR
          WRITE_CHR LF
          JP    main

; ------------------- FLASH_OP
; ZI - Flash information (also default)
; ZP AAAA LEN
FLASH_OP: CALL  SKIPSPC
          JR    Z,flsh_id
          CP    'I'
          JR    Z,flsh_id
          CP    'P'
          JR    Z,flsh_prg
          CP    'Z'
          JR    Z,flsh_clr

; ------------------- Read Flash Info
flsh_id:  WRITE_CRLF
          LD    A,SEL_FLSH
          OUT   (PAGE_REG),A
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
          JP    main

; ------------------- flsh_clr
flsh_clr: LD    A,SEL_FLSH
          OUT   (PAGE_REG),A
          CALL  _flsh_clr
          JP    main

; ----------------------------------- flsh_prg
; Expect a start address in page zero and a length. Writes data to the high page
flsh_prg: LD    HL,_FLSH_PRG
          CALL  PRINT
          LD    A,SEL_FLSH       ; Make sure we have flash page 0 mapped to page 2.
          OUT   (PAGE_REG),A
          ; Get parameters
          CALL  GET_HEX          ; From Address
          JP    Z, f_err
          LD    D,H
          LD    E,L              ; DE: Address FROM address
          CALL  GET_HEX          ; The TO ADDRESS - top bit set
          JP    Z,f_err
          LD    (FL_TO_ADDR),HL  ; TO ADDRESS saved
          CALL  GET_HEX          ; The Length
          JP    Z,f_err
          LD    B,H              ; BC: Count
          LD    C,L
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
          SET   7,H
loopp:    LD    A,(DE)
          CALL  _flsh_bt
          INC   HL
          INC   DE
          DEC   BC
          LD    A,B
          OR    C
          JR    NZ,loopp
          JP    main;
; ----------------------------------- _flsh_clr
; Erase page 0 (mapped to 8000 - 4K page)
_flsh_clr: LD   A,80h
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
_flsh_cmd:  PUSH  HL
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

; ----- Load a hex file (from the console input)
invalid:  LD    HL, _NOSTART
          CALL  PRINT
          JP    main
impeof:   LD    HL,_COMPLETE
          CALL  PRINT
          JP    main
rec_err:  LD    HL,_REC_ERR
          CALL  PRINT
          JP    main

; Process all lines starting with a ':'
LOAD:     CALL  GET_HEX           ; OFFSET to apply to hex records. Z flag set if 'break' pressed.
          LD    D,H               ; Save HL
          LD    E,L
          LD    HL, _WAITING
          CALL  PRINT
          WRITE_CRLF
          LD    H,D
          LD    L,E
nextline: CALL  GET_LINE
          JR    Z, nextline
          CALL  BUFCHR
          CP    ':'
          JP    NZ,invalid
          ; Accept this line. Format: [LEN 1][ADDR 2][REC_TYPE 1][DATA 2]+[CHKSM 1]
          CALL  IN_HEX_2  ; Length -> A
          JR    C, rec_err
          LD    B,A       ; Length
          CALL  IN_HEX_4  ; Address - HL
          JR    C, rec_err
          ; CALL  here
          CALL  IN_HEX_2  ; Command - which should be 00. If not then EOF so end.
          JR    C, rec_err
          OR    A
          JR    NZ, impeof
          LD    A,B
          OR    A
          JR    Z, rec_err ; Length can't be zero.
          ; Modify the address using the offset in DE
          ADD   HL,DE
next_b:   CALL  IN_HEX_2
          JR    C, rec_err
          LD   (HL), A
          INC   HL
          DJNZ  next_b
          ; END of that record (ignoring checksum)
          WRITE_CRLF
          JR    nextline
; ----- SETBP - set breakpoint
; HL: The address at which to set a BP
; A:  The type code for the BP. Must be >0.
;   1: single step BP (reserved)
;   2: standard BP - permanent until cleared
; Find an available breakpoint slot.
SETBP:    PUSH  BC
          PUSH  DE
          PUSH  HL
          LD    C,A             ; A holds the BP type to set - need to keep this
          LD    A,(HL)          ; If there's already a BP at this location do nothing
          CP    BRK_OPCODE
          LD    A,C
          JR    Z,_dupbp
          PUSH  AF
          LD    D,H             ; Save HL (BP address)
          LD    E,L
          LD    HL,BPOINTS
          LD    C,0
          LD    B,NUM_BK
nextbp:   LD    A,(HL)
          OR    A
          JR    Z,fndbp         ; Not available so skip
          INC   HL
          INC   HL
          INC   HL
          INC   HL
          INC   C
          DJNZ  nextbp
          ; If we get here then can't set a BP - error
          LD    HL,_nobpavail
          CALL  PRINT
          POP   AF
          POP   HL
          POP   DE
          POP   BC
          POP   BC               ; Lose return address
          JP    main
fndbp:    POP   AF
          LD    (HL),A           ; HL points at the BP record to be used and A is the BP type
          INC   HL
          LD    (HL),E           ; Next 2 bytes are the address at which thi BP is set
          INC   HL
          LD    (HL),D
          INC   HL
          ; If 'type' is 1 then set the BP. Any other value and the BP
          ; is set at runtime
          DEC   A
          JR    NZ,_dupbp
          LD    A,(DE)           ; DE is the address at which we're setting the BP. We need the op-code stored there (in A)
          LD    (HL),A           ; And sore that in the BP record
          LD    A,BRK_OPCODE     ; The opcode is replaced by our single byte RST BP handler code
          LD    (DE),A
_dupbp:   POP   HL
          POP   DE
          POP   BC
          RET


; ------ FINDBP - Find the BP at the address pointed to by HL. Result returned in DE. HL unchanged.
; DE - Address of BP (IN)
; HL - Address of matching BP record if found (OUT)
; Z  - Set if BP not found
FINDBP:   LD    HL,BPOINTS
          PUSH  BC
          LD    B,NUM_BK
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
; is being set at the current PC location then it's skipped.
; A:  0 - set the breakpoints
; A: !0 - remove the breakpoints
INSTBP:   LD    HL,BPOINTS
          LD    C,A
          LD    B,NUM_BK
_inextbp: LD    A,(HL)
          CP    2
          CALL  Z,_insbp
          ; Not available so skip
          INC   HL
          INC   HL
          INC   HL
          INC   HL
          DJNZ  _inextbp
          RET
_insbp:   PUSH  HL
          INC   HL
          LD    E,(HL)
          INC   HL
          LD    D,(HL)
          INC   HL
          LD    A,C
          OR    A
          JR    NZ,_insrm
          ; Install the breakpoint
          ; Check whether this BP is at the current PC location
          LD    A,(R_PC)
          CP    E
          JR    NZ,_insok
          LD    A,(R_PC+1)
          CP    D
          JR    Z,_iskip    ; Can't set a BP at the current PC location
_insok:   LD    A,(DE)      ; BP already installed?
          CP    BRK_OPCODE
          JR    Z,_iskip
          LD    (HL),A
          LD    A,BRK_OPCODE
          LD    (DE),A
_iskip:   POP   HL
          RET
_insrm:   LD    A,(DE)
          CP    BRK_OPCODE
          JR    NZ,_noclr
          LD    A,(HL)      ; The opcode that needs to be reset into the code
          LD    (DE),A
_noclr:   POP   HL
          RET
; ------ SUSBP
; Suspend breakpoint. Remove from code but not from the BP table.
; HL: Address of the BP record.
SUSBP:    PUSH  HL
          PUSH  DE
          JR    _rembp

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

_clrbp:   PUSH  HL          ; HL points to the BP descriptor
          PUSH  DE
          XOR   A           ; Clear the 'type' field to free this slot
          LD    (HL),A
_rembp    INC   HL          ; Next two bytes are the address of this BP
          LD    E,(HL)
          INC   HL
          LD    D,(HL)
          INC   HL          ; And the last byte is the one we replaced with the BP trigger RST opcode
          LD    A,(DE)
          CP    BRK_OPCODE
          JR    NZ,_nobp
          LD    A,(HL)
          LD    (DE),A      ; Restore the byte we overwrote
_nobp:    POP   DE
          POP   HL
          RET

; ----- Step Over
NSTEP:    XOR   A            ; A -> zero
          LD    E,A          ; In E
          CALL  SSTEP_BP
          JR    GO

; ----- Single Step
SSTEP_T:  LD    E,1         ; A -> !0
          CALL  SSTEP_BP    ; Set single step BP then go
          JP    main
; ----- Single Step
SSTEP:    LD    E,1         ; A -> !0
          CALL  SSTEP_BP    ; Set single step BP then go
; ------------------- go
GO:       XOR   A
          CALL  INSTBP      ; Install all permanent breakpoints
          LD    HL,DO_BP
          LD    (13h),HL
          CALL  REST_RGS
          LD    (MON_SP),SP
          LD    SP,(R_SP)
          JP    JP_RUN

SSTEP_BP: LD    HL, (R_PC)
          CALL  INST_LEN
          ; A: Instruction length
          ; C: Extended status
          ; HL: Unchanged - start of THIS instruction
          ADD   A,L
          JR    NC,_nc1
          INC   H
_nc1:     LD    L,A
          XOR   A
          INC   A
          CALL  SETBP      ; HL point to next instruction
          LD    A,C
          AND   7
          RET   Z          ; No change of control

          ; C determines the type of reference:
          ; 01 - relative : last byte of instruction is PC relative offset
          ; 02 - absolute : last two bytes of inststruction is an absolute address
          ; 03 - return   : look at the stack to find the next instruction
          ; 04 - rst      : RST xxH call
          ; 05 - special  : JR (HL)(IX)(IY) - special processing required.
          ; 06 - call     : Call to absolute address
          LD    A,C
          DEC   A
          JR    NZ,_type2

          ; It's relative. HL points to the NEXT instruction
          DEC   HL
          LD    A,(HL)
          INC   HL
          LD    E,A
          ADD   A,A
          SBC   A,A
          LD    D,A
          ADD   HL,DE
_setbp:   XOR   A
          INC   A
          CALL  SETBP
          RET
_type2:   DEC   A
          JR    NZ,_type3
          ; Absolute address - use last 2 bytes of the instruction
          DEC   HL
          LD    D,(HL)
          DEC   HL
          LD    E,(HL)
          EX    DE,HL
          JR    _setbp
_type3:   DEC   A
          JR    NZ,_type4
          ; It's a RET instruction or variant. Return address is on the stack.
          LD    HL,(R_SP)
          LD    E,(HL)
          INC   HL
          LD    D,(HL)
          EX    DE,HL
          JR    _setbp
_type4:   DEC   A
          JR    NZ,_type5
          ; It's an RSTxx instruction. We don't try to step into these right now. They are generally system calls
          ; and it's not good to tamper!
          JR    GO
_type5:   DEC   A
          JR    NZ,_type6
          ; JP (XX) - HL, IX, IY. Instruction determines which
          LD    HL, (R_PC)
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
          LD    A,E
          OR    A
          RET   Z
          JR    _type2   ; Treat it as an absolute jump

; ------------------- DO_BP
DO_BP:    ; Save main registers
          DI
          LD   (R_BC),BC
          LD   (R_DE),DE
          LD   (R_HL),HL
          LD   (R_IX),IX
          LD   (R_IY),IY

          ; Disable break processing
          LD    HL,0
          LD    (13h),HL
          EI

          POP   DE            ; DE will be 1 more than the RST instruction - the return address
          DEC   DE            ; Points at the RST instruction which we overwrote with the RST instruction
          LD   (R_PC),DE      ; DE is now the start of the next instruction
          LD   (R_SP),SP

          LD   SP,(MON_SP)    ; Restore our own SP

          PUSH AF             ; Grab the AF pair through OUR stack - don't tamper with the apps stack
          POP  HL
          LD   (R_AF),HL

          XOR   A             ; Coming from execution not from a clear BP CLI op
          CALL  FINDBP        ; Find the BP. Want to know what type it is.
          JR    Z,_bp_nf
          LD    A,(HL)        ; The type of the BP. 1: single step, 2: hard breakpoint
          DEC   A             ; Single step?
          JR    NZ, _permbp   ; No - permanent BP
          ; Single step - so clear all
          CALL  CLRBP2        ; DE points at the BP address
          JR    _bp_nf

_permbp:  CALL  SUSBP         ; Remove this BP from code but not from the BP table.


_bp_nf:   EX    DE,HL         ; Put the current address into HL so we can display it
          CALL  DECINST       ; Display the next instruction
          JP    SHOW_RGS

; --------- INST_LEN
; Given a pointer to an opcode return the number of bytes in the referenced instruction. All
; registers saved EXCEPT A which holds the return count and C which includes the instruction code
; from the first byte.
INST_LEN: PUSH  DE
          PUSH  HL
          LD    D,0
          LD    E,(HL)  ; OPCODE THING
          LD    B,0
          LD    HL,_opcodes
          ADD   HL,DE
          ADD   HL,DE
          LD    A,(HL)   ; Descriptive byte
          AND   03h      ; If the lower 2 bits are zero then this is a prefix that needs more decoding.
          JR    NZ,_noext

          ; Processing a prefix so need the next opcode byte
          LD    A,(HL)   ; Upper bit 0: Use bits 2-3, 1: use bits 4-5
          BIT   7,A      ; Carry bit dictates how we decode count bits
          POP   HL
          PUSH  HL
          INC   HL
          LD    E,(HL)   ; Second byte of opcode - look this up
          LD    HL,_opcodes
          ADD   HL,DE
          ADD   HL,DE
          LD    A,(HL)   ; Descriptive byte for next byte
          JR    NZ,_is_dd
          AND   0Ch
          SRL   A
          SRL   A
          JR    _got2nd
_is_dd:   AND   30h
          SRL   A
          SRL   A
          SRL   A
          SRL   A
_got2nd:  INC   A
_noext:   INC   HL
          LD    C,(HL)   ; Return the extended descriptor in C
          POP   HL
          POP   DE
          RET

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

import libconsole.asm

; --------------------- STRINGS
_INTRO:   DEFB ESC,"[2J",ESC,"[1m",ESC,"[1;6HSTACK",ESC,"[m",ESC,"[11;50r",ESC,"[9;1H>",ESC,"[12,1HZ80 CLM 1.3",CR,LF,"Ready...",CR,LF,NULL
_PROMPT:  DEFB "> ",0
_ERROR:   DEFB "Unknown command",CR,LF,0
_NOSTART: DEFB CR,LF,"No record start character ':'",CR,LF,NULL
_BANKMSG: DEFB CR,LF,"Switching bank register to: ",NULL
_REC_ERR: DEFB CR,LF,"Bad record",CR,LF,0
_COMPLETE:DEFB CR,LF,"Download complete",CR,LF,0
_HEXERR:  DEFB CR,LF,"Bad hex character: ",CR,LF,0
_BAD_REG: DEFB "Bad register",CR,LF,0
_FLSH_PRG:DEFB "Flash prog",CR,LF,0
_HEX_CHRS: DEFB  "0123456789ABCDEF"
_WAITING: DEFB "Waiting...",0


; Register labels
R_PC_DESC DEFB ESC,"[11;50r",ESC,"[2;40H  PC: ",NULL
R_SP_DESC DEFB ESC,"[3;40H  SP: ",NULL
R_A_DESC  DEFB ESC,"[4;40H  A:  ",NULL
R_BC_DESC DEFB ESC,"[2;60H  BC: ",NULL
R_DE_DESC DEFB ESC,"[3;60H  DE: ",NULL
R_HL_DESC DEFB ESC,"[4;60H  HL: ",NULL
R_F_DESC  DEFB ESC,"[5;60H  F:  ",NULL
R_IX_DESC DEFB ESC,"[5;40H  IX: ",NULL
R_IY_DESC DEFB ESC,"[6;40H  IY: ",NULL
R_WIN_TOP DEFB ESC,"[0;12r",NULL
R_WIN_BOT DEFB ESC,"[13;50r",NULL

; VT100 sequences
SAVE_POS  DEFB ESC,"7",NULL
REST_POS  DEFB ESC,"8",NULL
CURS_UP   DEFB ESC,"[A",NULL
HOME:     DEFB ESC,"[H",NULL
STK_NXT   DEFB CR,LF,ESC,"[3C",NULL ; Down one line then to character 2


FLAGS_DESC:  DEFB "SZ5H3VNC",NULL

_fill_err:   DEFB "Bad parameters",CR,LF,NULL
_fill_msg:   DEFB "Fill: ADDR: ",NULL
_fill_sz:    DEFB " LEN:",NULL
_fill_wt:    DEFB " WITH:",NULL
_prg_msg:    DEFB "FROM ADDRESS: ",NULL
_prg_msgto:  DEFB ", TO: ",NULL
_prg_msglen: DEFB ", LEN: ",NULL
_nobpavail:  DEFB "No BP available",CR,LF,NULL

; _opcodes
;   - lower 8 bits - instruction length
;   -  XX XX XX XX
;      |   |  |  +---> Length of no prefix
;      |   |  +------> Length for ED prefix
;      |   +---------> Length for DD,FD prefix
;      +-------------> If a prefix then clr for ED, set for DD
; CB prefix instructions are all 2 bytes so don't need any more deconding
;
; The MSB contains enhanced information. Current bits are
;   -  XX XXX AAA
;   AAA - decribes change of for for this instruction:
;   000 - Normal instruction, no change of control
;   001 - Single byte relative jump (eg JR NZ xx)
;   010 - Two byte absolute jump (eg JP C xxxx)
;   011 - Return from subroutine
;   100 - A RST call
;   101 - JR (reg) - HL, IX, IY depending on prefix
;   110 - A 'call' instruction. Absolute address but step over this is the 'next' command is used.
_opcodes        DEFW      0001h, 0003h, 0001h, 0001h, 0001h, 0001h, 0002h, 0001h,   0001h, 0011h, 0001h, 0001h, 0001h, 0001h, 0002h, 0001h ; 0
                DEFW      0102h, 0033h, 0031h, 0011h, 0011h, 0011h, 0022h, 0001h,   0102h, 0011h, 0031h, 0011h, 0011h, 0011h, 0022h, 0001h ; 1
                DEFW      0102h, 003Fh, 000Fh, 0005h, 0021h, 0021h, 0032h, 0001h,   0102h, 0011h, 0033h, 0011h, 0011h, 0011h, 0022h, 0001h ; 2
                DEFW      0102h, 0003h, 0003h, 0001h, 0021h, 0021h, 0032h, 0001h,   0102h, 0011h, 0003h, 0001h, 0001h, 0001h, 0002h, 0001h ; 3
                DEFW      0005h, 0005h, 0005h, 000Dh, 0015h, 0015h, 0025h, 0005h,   0005h, 0005h, 0005h, 000Dh, 0015h, 0015h, 0025h, 0005h ; 4
                DEFW      0005h, 0005h, 0005h, 000Dh, 0015h, 0015h, 0025h, 0005h,   0005h, 0005h, 0005h, 000Dh, 0015h, 0015h, 0025h, 0005h ; 5
                DEFW      0015h, 0015h, 0015h, 001Dh, 0015h, 0015h, 0025h, 0015h,   0015h, 0015h, 0015h, 001Dh, 0015h, 0015h, 0025h, 0015h ; 6
                DEFW      0025h, 0025h, 0025h, 002Dh, 0025h, 0025h, 0005h, 0025h,   0005h, 0005h, 0005h, 000Dh, 0015h, 0015h, 0025h, 0005h ; 7

                DEFW      0001h, 0001h, 0001h, 0001h, 0011h, 0011h, 0021h, 0001h,   0001h, 0001h, 0001h, 0001h, 0011h, 0011h, 0021h, 0001h ; 8
                DEFW      0001h, 0001h, 0001h, 0001h, 0011h, 0011h, 0021h, 0001h,   0001h, 0001h, 0001h, 0001h, 0011h, 0011h, 0021h, 0001h ; 9
                DEFW      0005h, 0005h, 0005h, 000Dh, 0015h, 0015h, 0025h, 0005h,   0005h, 0005h, 0005h, 000Dh, 0015h, 0015h, 0025h, 0005h ; A
                DEFW      0005h, 0005h, 0005h, 000Dh, 0015h, 0015h, 0025h, 0005h,   0005h, 0005h, 0005h, 000Dh, 0015h, 0015h, 0025h, 0005h ; B
                DEFW      0301h, 0201h, 0203h, 0203h, 0603h, 0001h, 0002h, 0401h,   0301h, 0301h, 0203h, 0032h, 0603h, 0603h, 0002h, 0401h ; C - note CB: All 2 bytes so no prefix decode
                DEFW      0301h, 0001h, 0203h, 0002h, 0603h, 0001h, 0002h, 0401h,   0301h, 0001h, 0203h, 0002h, 0603h, 0080h, 0002h, 0401h ; D
                DEFW      0301h, 0011h, 0203h, 0011h, 0603h, 0011h, 0002h, 0401h,   0301h, 0511h, 0203h, 0001h, 0603h, 0000h, 0002h, 0401h ; E
                DEFW      0301h, 0001h, 0203h, 0201h, 0603h, 0001h, 0002h, 0401h,   0301h, 0011h, 0203h, 0001h, 0603h, 0080h, 0002h, 0401h ; F

; Cmd jump table. 26 entries one for each letter. Every command starts with a letter. Each entry is the address of the handler.
CMD_TABLE:      DEFW      0         ; A
                DEFW      BP        ; B
                DEFW      COPY      ; C
                DEFW      DUMP      ; D
                DEFW      0         ; E
                DEFW      FILL      ; F
                DEFW      GO        ; G
                DEFW      0         ; H
                DEFW      0         ; I
                DEFW      0         ; J
                DEFW      0         ; K
                DEFW      LOAD      ; L hhhh    : hex offset to add to all addresses
                DEFW      MODIFY    ; M hhhh    : start writing bytes at specified address
                DEFW      NSTEP     ; N         ; single step but over subroutine calls
                DEFW      0         ; O
                DEFW      0         ; P
                DEFW      0         ; Q
                DEFW      SET_RGS   ; R         : show register values. R NAME=VALUE - set specific register values
                DEFW      SSTEP     ; S         : step one instruction
                DEFW      SSTEP_T   ; T
                DEFW      UPGRADE   ; U         : upgrade monitor image from memory @2
                DEFW      0         ; V
                DEFW      0         ; W
                DEFW      0         ; X
                DEFW      BANK      ; Y
                DEFW      FLASH_OP  ; Z

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

; ---------------------- VARIABLES
; If you want the monitor code in ROM then add an ORG here to locate variables somewhere in RAM
if FLASH_MON=YES
           .ORG   VARS_ADDR
endif
LAST_CMD:  DEFB    'R'
DUMP_ADDR: DEFW    0
DUMP_MODE: DEFB    'I'
DUMP_CHRS: DEFB  "  "
           .DS   16
           DEFB    0
INPTR      DEFW  INBUF
INBUF      .DS   80
END_INBUF  DEFB    0

; Temporary storage for Flash programming
FL_TO_ADDR DEFW   0

; JP_RUN - C3 is the JP opcode. By jumping to JP_RUN execution will
; continue from the current value of the PC. This avoids us having to
; push values onto the applications stack.
JP_RUN:    DEFB    $C3
; Storage area for working registers
R_PC       DEFW    2000h
R_SP       DEFW    6FF0h ; Initial application stack is NOT the same as ours
R_AF       DEFW    0
R_BC       DEFW    0
R_DE       DEFW    0
R_HL       DEFW    0
R_IX       DEFW    0
R_IY       DEFW    0

MON_SP:    DEFW    0    ; Monitors SP is stored here before running client code.

; Breakpoints. Each entry is 4 bytes:
; Type|AddrX2|Opcode
; A breakpint in placed in memory by replacing an instruction with a single byte RST 20h (or replacement) opcode
; Type: 0 - free slot - can be used to store a new breakpoint
;       1 - single shot - removed once hit
;       2 - permanent breakpoint (WIP)
BPOINTS:   .DS    NUM_BK*BP_SIZE

LAST_ADDR: DEFB    0

.END
