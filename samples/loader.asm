#define WRITE_CHR(c) LD A,c \ RST 08H
#define WRITE_CRLF   LD A,CR \ RST 08H \ LD A,LF \ RST 08H

; External dependancies
; RST 08h - Write a single character to the terminal, character in A
; RST 10h - Read one character from the terminal. Block until there is a character. Returned in A
; RST 18h - Check whether there is a character available. Non blocking. Z flag set if there is NOT a character waiting.

; Configurable parameters
LOAD_ADDR   .EQU    01C0h     ; Start address for the main code
PAGE_REG    .EQU     0E0h     ; I/O address for 8 bit memory page select register (B command)

; Breakpoints in code are handled by replacing the opcode at the break location with a RST instruction. The
; default is RST 20h. You can change this if your system is using RST 20 for something else.
BRK_OPCODE  .EQU     0E7h     ; The instruction to use to cause a breakpoint. This must be a RST xx single byte op. Default RST 20h
BRK_HANDLER .EQU      20h     ; The address at which to origin the break point handler Must match BRK_OPCODE

; The 'U' command replaces the current running monitor with a modified copy at MON_COPY. This
; is generally used for developing the monitor and doesn't have a lot of use in a running
; environment. To make the copy a small copy loop is written to unused memory at LOADER.
MON_COPY    .EQU    2000h
LOADER      .EQU    7800h

; Character constants
CR        .EQU     0DH
LF        .EQU     0AH
FF        .EQU     0CH
BS        .EQU     08H             ; Backspace
TAB       .EQU     09H             ; Tab
DEL       .EQU     7fH             ; Delete
CS        .EQU     0CH             ; Clear screen
SPC       .EQU     20H

STACK     .EQU     07ff0h

          .ORG    BRK_HANDLER
BP:       JP      DO_BP

; Number of breakpoint locations (in code)
NUM_BK    .EQU    64

          .ORG    LOAD_ADDR

START:    LD    HL,STACK
          LD    SP,HL
          LD    HL, INTRO
          CALL  PRINT

          ; Clear all breakpoints
          LD    HL,BPOINTS
          LD    BC,NUM_BK*5
          XOR   A
          LD    D,A
clrn:     LD    (HL),D
          DEC   BC
          INC   HL
          LD    A,B
          OR    C
          JR    NZ,clrn
          ; Display initial registers
          JP    SHOW_RGS
main:     LD    HL, PROMPT
          CALL  PRINT
          CALL  GET_LINE

          WRITE_CHR(CR)
          WRITE_CHR(LF)
          ; CALL  PRINT

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
err:      LD    HL, ERROR
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
; ------------------- run
SHOW_RGS: LD    HL,SAVE_POS
          CALL  PRINT
          LD    HL,R_WIN_TOP
          CALL  PRINT
          LD    HL,R_PC_DESC
          CALL  PRINT
          LD    HL,(R_PC)
          CALL  WRITE_16
          LD    HL,R_EOLN
          CALL  PRINT

          LD    HL,R_SP_DESC
          CALL  PRINT
          LD    HL,(R_SP)
          CALL  WRITE_16
          LD    HL,R_EOLN
          CALL  PRINT

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
          LD    HL,R_EOLN
          CALL  PRINT

          LD    HL,R_BC_DESC
          CALL  PRINT
          LD    HL,(R_BC)
          CALL  WRITE_16
          LD    HL,R_EOLN
          CALL  PRINT

          LD    HL,R_DE_DESC
          CALL  PRINT
          LD    HL,(R_DE)
          CALL  WRITE_16
          LD    HL,R_EOLN
          CALL  PRINT

          LD    HL,R_HL_DESC
          CALL  PRINT
          LD    HL,(R_HL)
          CALL  WRITE_16
          LD    HL,R_EOLN
          CALL  PRINT

          LD    HL,R_IX_DESC
          CALL  PRINT
          LD    HL,(R_IX)
          CALL  WRITE_16
          LD    HL,R_EOLN
          CALL  PRINT

          LD    HL,R_IY_DESC
          CALL  PRINT
          LD    HL,(R_IY)
          CALL  WRITE_16
          LD    HL,R_EOLN
          CALL  PRINT

          LD    HL,R_WIN_BOT
          CALL  PRINT
          LD    HL,REST_POS
          CALL  PRINT
          JP    main
; ------------------- run
SET_PC:   CALL  GET_HEX           ; OFFSET to apply to hex records. Z flag set if 'break' pressed.
          LD    (R_PC),HL
          JP    SHOW_RGS

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
; ------------------- MODIFY
MODIFY:   CALL  GET_HEX          ; Start address
          JP    Z, f_err
          ; Sit in a loop processing lines.
_nextln:  CALL  WRITE_16
          WRITE_CHR(':')
          WRITE_CHR(' ')
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
          WRITE_CHR(SPC)
_nextb:   LD    A,(HL)
          CALL  WRITE_8
          WRITE_CHR(SPC)
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
dloop2    WRITE_CHR(CR)
          WRITE_CHR(LF)
          CALL  WRITE_16          ; 4 hex digits from HL
          WRITE_CHR(SPC)
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
          WRITE_CHR(SPC)
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
          WRITE_CHR(CR)
          WRITE_CHR(LF)
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

SEL_FLSH  .EQU  08h
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
          WRITE_CHR('-')
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
flsh_prg: LD    A,SEL_FLSH       ; Make sure we have flash page 0 mapped to page 2.
          OUT   (PAGE_REG),A
          ; Get parameters
          CALL  GET_HEX          ; Address
          JP    Z, f_err
          PUSH  HL
          LD    D,H
          LD    E,L              ; DE: Address
          CALL  WASTESPC
          CALL  GET_HEX          ; Length
          JP    Z,f_err
          PUSH  HL
          LD    B,H              ; BC: Count
          LD    C,L
          LD    HL,_prg_msg
          CALL  PRINT
          LD    H,D
          LD    L,E
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
          POP   BC
          POP   HL
loopp:    LD    A,(HL)
          SET   7,H
          CALL  _flsh_bt
          RES   7,H
          INC   HL
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
_waiting: .TEXT "Waiting..." \ .DB 0

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
          LD    HL, _waiting
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
; ----- SETBP
; Set breakpoint at address in HL. If A=0 then single hit - erased once matched. Find
; an available breakpoint slot
SETBP:    PUSH  BC
          PUSH  DE
          PUSH  HL
          PUSH  AF
          LD    D,H
          LD    E,L
          LD    HL,BPOINTS
          LD    C,0
          LD    B,NUM_BK
nextbp:   LD    A,(HL)
          OR    A
          JR    Z,fndbp
          ; Not available so skip
          INC   HL
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
          POP   BC       ; Lose return address
          JP    main
fndbp:    POP   AF
          LD    (HL),A   ; HL points at the BP record to be used
          INC   HL
          LD    (HL),E
          INC   HL
          LD    (HL),D
          INC   HL
          LD    A,(DE)
          LD    (HL),A
          LD    A,BRK_OPCODE
          LD    (DE),A
          INC   DE
          INC   HL
          LD    A,(DE)
          LD    (HL),A
          LD    A,C
          LD    (DE),A
          POP   HL
          POP   DE
          POP   BC
          RET

; ------ CLRBP
; BP number in A
CLRBP:    PUSH  HL
          PUSH  DE
          LD    L,A
          LD    H,0
          LD    D,H
          LD    E,L
          ADD   HL,HL       ; Index x 5
          ADD   HL,HL
          ADD   HL,DE
          LD    DE,BPOINTS
          ADD   HL,DE       ; HL points to BP descriptor
          LD    A,(HL)
          ; Type 1 means clear all of this type (single step)
          DEC   A
          JR    Z,_clrsstp
          ; Just a single break point to clear
          CALL  _clrbp
          JR    _cbpfin
_clrsstp: ; Find all breakpoints type 1 and clear
          LD    HL,BPOINTS
          LD    B,0
_nextbp:  LD    A,(HL)
          DEC   A
          CALL  Z,_clrbp
          ; Not available so skip
          INC   HL
          INC   HL
          INC   HL
          INC   HL
          INC   HL
          DJNZ  _nextbp
_cbpfin:  POP   DE
          POP   HL
          RET

_clrbp:   ; HL points to the BP descriptor
          PUSH  HL
          PUSH  DE
          LD    A,(HL)
          OR    A
          JR    Z,_nobp
          XOR   A
          LD    (HL),A
          INC   HL          ; Next two bytes are the address of this BP
          LD    E,(HL)
          INC   HL
          LD    D,(HL)
          INC   HL          ; And the last two bytes are the ones we saved to overwrite with the BP trigger code
          LD    A,(HL)
          LD    (DE),A      ; Restore the bytes we overwrote
          INC   DE
          INC   HL
          LD    A,(HL)
          LD    (DE),A      ; Restore the bytes we overwrote
_nobp:    POP   DE
          POP   HL
          RET

; ----- Set single-step BP then return
SSTEP_S:  CALL  SSTEP_BP
          JP    main
; ----- Single Step
SSTEP:    CALL  SSTEP_BP    ; Set single step BP then go
; ------------------- go
GO:       LD    SP,(R_SP)
          LD    HL,(R_PC)
          PUSH  HL
          CALL  REST_RGS
          RET

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
          ; 04 - special  : JR (HL)(IX)(IY) - special processing required.
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
          JR    _setbp
_type4:   DEC   A
          JR    NZ,_type5
          ; It's an RSTxx instruction. Deal with this later :/
          JR    GO
_type5:   ; JP (XX) - HL, IX, IY. Instruction determines which
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
          ; And run

; ------------------- DO_BP
DO_BP:    CALL  SAVE_RGS   ; Save normal registers
          POP   HL         ; HL will be 1 more than the RST instruction
          LD    A,(HL)     ; Points to the BP index - load into A
          DEC   HL         ; Points at the RST instruction which we overwrote with the RST instruction
          LD   (R_PC),HL
          LD   (R_SP),SP
          CALL  CLRBP
          CALL  DECINST
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

; SAVE_RGS - Get the caller from SP+4
SAVE_RGS: LD   (R_BC),BC
          LD   (R_DE),DE
          LD   (R_HL),HL
          LD   (R_IX),IX
          LD   (R_IY),IY
          PUSH AF
          POP  HL
          LD   (R_AF),HL
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

GET_CHR:  XOR   A
          RST   10H                ; Read character
          CP    'a'                ; Lower case -> upper case
          RET    C
          SUB   'a'
          ADD   A,'A'
          RET

; ----------------------------------- Ouput the 16 bit value in HL
WRITE_16:  PUSH AF
           LD   A,H
           CALL WRITE_8
           LD   A,L
           CALL WRITE_8
           POP  AF
           RET
WRITE_8:   PUSH AF
           PUSH BC
           PUSH DE
           PUSH HL
           LD   DE, HEX_CHRS
           LD   B,A
           SRA  A
           SRA  A
           SRA  A
           SRA  A
           AND  0Fh
           LD   L,A
           XOR  A
           LD   H,A
           ADD  HL,DE
           LD   A,(HL)
           RST  08H
           ; And the second nibble
           LD   A,B
           AND  0Fh
           LD   L,A
           XOR  A
           LD   H,A
           ADD  HL,DE
           LD   A,(HL)
           RST  08H
           POP  HL
           POP  DE
           POP  BC
           POP  AF
           RET

; ------------------- GET_HEX - read in up to 4 hex digits, returned in HL
GET_HEX:  PUSH  BC
          LD    B,A  ; Save A
          XOR   A
          LD    H,A
          LD    L,A
          LD    C,A
          CALL  SKIPSPC
          ; End of input?
next_hc:  OR    A
          JR    NZ, cont_hc
fin:      LD    A,C
          OR    A
          LD    A,B
          POP   BC
          RET
cont_hc:  CALL  HEX_TO_BIN
          JR    C,fin
          ; Is it between 0 and 9?
add_chr:  ADD   HL, HL
          ADD   HL, HL
          ADD   HL, HL
          ADD   HL, HL
          ADD   A, L
          LD    L, A
          INC   C
          CALL  BUFCHR
          JR    next_hc

; -------- IN_HEX_2 - Return 2 hex digit value in A. Set C flag on error
IN_HEX_2: PUSH  HL
          LD    L,0     ; Value being built
          CALL  BUFCHR  ; A -> character
          CALL  HEX_TO_BIN
          JR    C,errhex2
          LD    L,A     ; First byte
          CALL  BUFCHR  ; A -> character
          CALL  HEX_TO_BIN
          JR    C,errhex2
          LD    H,A     ; Tmp store
          LD    A,L     ; Current val
          ADD   A,A
          ADD   A,A
          ADD   A,A
          ADD   A,A
          ADD   A,H     ; Which will leave carry clear
          POP   HL
          OR    A
          RET

errhex2:  LD    HL,_HEXERR
          CALL  PRINT
          RST   08h
          POP   HL
          SCF
          RET

IN_HEX_4: CALL  IN_HEX_2
          RET   C
          LD    H,A
          CALL  IN_HEX_2
          LD    L,A
          RET

; -------- HEX_TO_BIN Char in A - 0-15. 255 if not valid char
HEX_TO_BIN: CP    '0'
            JR    C, inv      ; Less than zero so invalid
            CP    'F'+1
            JR    NC, inv     ; > 'F' so ignore
            CP    '9'+1
            JR    NC, letter_hc
            SUB   '0'
            AND   0fh
            RET
letter_hc:  CP    'A'
            JR    C, inv
            SUB   'A'-10
            OR    A
            RET
inv:        SCF
            RET

; --------------------- GET_LINE
GET_LINE: PUSH     HL
          PUSH     BC
          LD       HL, INBUF
          LD      (INPTR), HL
          LD       BC, 0

getc:     CALL     GET_CHR
          CP       CR
          JR       Z, eol
          CP       BS
          JR       Z, bspc

          ; Store in buffer
          LD      (HL), A

          ; At end of buffer?
          LD      C,A
          LD      A,80
          CP      B
          JR      Z, getc       ; buffer full
          INC     HL
          INC     B
          LD      A,C
          RST     08H
          JR      getc

eol:      XOR     A
          LD      (HL), A
          LD      A, B
          POP     BC
          POP     HL
          OR      A     ; Z flag set if no characters entered in line
          RET

bspc:     XOR      A
          CP       B
          JR       Z, getc

          ; Delete character
          DEC      HL
          LD      (HL), A
          DEC      B
          WRITE_CHR(BS);
          ; WRITE_CHR(DEL);
          JR       getc

BUFCHR:   PUSH     HL
          LD       HL, (INPTR)
          LD       A, (HL)
          OR       A
          JR       Z, eb
          INC      HL
          LD       (INPTR), HL
          POP      HL
          RET

; -------- SKIPSPC
; Step over spaces and return first non-space character.
SKIPSPC:  PUSH     HL
          LD       HL, (INPTR)
skip      LD       A, (HL)
          OR       A
          JR       Z, eb
          INC      HL
          CP       SPC
          JR       Z,skip
          CP       TAB
          JR       Z,skip
          LD       (INPTR), HL
eb:       POP      HL
          RET
WASTESPC: PUSH     HL
          LD       HL, (INPTR)
skip2:    LD       A, (HL)
          OR       A
          JR       Z, eb
          INC      HL
          CP       SPC
          JR       Z,skip2
          CP       TAB
          JR       Z,skip2
          DEC      HL
          LD       (INPTR), HL
          JR       eb


; --------- GET_HEX_2 - read 2 byte HEX value from input line to A
; Z flag set on OK

; --------------------- PRINT - write a string to the terminal
PRINT:    LD       A,(HL)          ; Get character
          OR       A               ; Is it $00 ?
          RET      Z               ; Then RETurn on terminator
          RST      08H             ; Print it
          INC      HL              ; Next Character
          JR       PRINT           ; Continue until $00

wait:     PUSH HL
          LD H,0ffH
loop2:    LD L,0ffH
loop3:    DEC L
          JR NZ,loop3
          DEC H
          JR NZ,loop2
          POP HL
          RET

; --------------------- STRINGS
INTRO:    .TEXT "\033[2J\033[13;50r\033[14,1HZ80 CLM 2.5\r\nReady...\r\n\000"
PROMPT:   .TEXT "> \000"
DONE:     .TEXT "\r\ndone.\r\n\000"
WHERE:    .TEXT "\rADDR? \000"
ERROR:    .TEXT "Unknown command\r\n\000"
_NOSTART: .TEXT "\r\nNo record start character ':'\r\n\000"
_BANKMSG: .TEXT "\r\nSwitching bank register to: \000"
_REC_ERR: .TEXT "\r\nBad record\r\n\000"
_COMPLETE:.TEXT "\r\nDownload complete\r\n\000"
_HEXERR:  .TEXT "\r\nBad hex character: \r\n\000"
HEX_CHRS: .TEXT  "0123456789ABCDEF"

; Register labels
R_PC_DESC .TEXT "\033[2;40H  PC: \000"
R_SP_DESC .TEXT "\033[3;40H  SP: \000"
R_A_DESC  .TEXT "\033[4;40H  A:  \000"
R_BC_DESC .TEXT "\033[2;60H  BC: \000"
R_DE_DESC .TEXT "\033[3;60H  DE: \000"
R_HL_DESC .TEXT "\033[4;60H  HL: \000"
R_F_DESC  .TEXT "\033[5;60H  F:  \000"
R_IX_DESC .TEXT "\033[5;40H  IX: \000"
R_IY_DESC .TEXT "\033[6;40H  IY: \000"
R_EOLN    .TEXT "\000";\033[K\000"
R_WIN_TOP .TEXT "\033[0;12r\000"
R_WIN_BOT .TEXT "\033[13;50r\000"
SAVE_POS  .TEXT "\0337\000"
REST_POS  .TEXT "\0338\000"
CURS_UP   .TEXT "\033[A\000"

FLAGS_DESC:  .TEXT "SZ5H3VNC\000"

_fill_err:   .TEXT "Bad parameters\r\n\000"
_fill_msg:   .TEXT "Fill: ADDR: \000"
_fill_sz:    .TEXT " LEN:\000"
_fill_wt:    .TEXT " WITH:\000"
_prg_msg:    .TEXT "PROG ADDRESS: \000"
_prg_msglen: .TEXT ", LEN: \000"
_nobpavail:  .TEXT "No BP available\r\n\000"

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
;   011 - Return from subrouting
;   100 - A RST call
;   101 - JR (reg) - HL, IX, IY depending on prefix
_opcodes        .DW      0001h, 0003h, 0001h, 0001h, 0001h, 0001h, 0002h, 0001h,   0001h, 0011h, 0001h, 0001h, 0001h, 0001h, 0002h, 0001h ; 0
                .DW      0102h, 0033h, 0031h, 0011h, 0011h, 0011h, 0022h, 0001h,   0102h, 0011h, 0031h, 0011h, 0011h, 0011h, 0022h, 0001h ; 1
                .DW      0102h, 003Fh, 000Fh, 0005h, 0021h, 0021h, 0032h, 0001h,   0102h, 0011h, 0033h, 0011h, 0011h, 0011h, 0022h, 0001h ; 2
                .DW      0102h, 0003h, 0003h, 0001h, 0021h, 0021h, 0032h, 0001h,   0102h, 0011h, 0003h, 0001h, 0001h, 0001h, 0002h, 0001h ; 3
                .DW      0005h, 0005h, 0005h, 000Dh, 0015h, 0015h, 0025h, 0005h,   0005h, 0005h, 0005h, 000Dh, 0015h, 0015h, 0025h, 0005h ; 4
                .DW      0005h, 0005h, 0005h, 000Dh, 0015h, 0015h, 0025h, 0005h,   0005h, 0005h, 0005h, 000Dh, 0015h, 0015h, 0025h, 0005h ; 5
                .DW      0015h, 0015h, 0015h, 001Dh, 0015h, 0015h, 0025h, 0015h,   0015h, 0015h, 0015h, 001Dh, 0015h, 0015h, 0025h, 0015h ; 6
                .DW      0025h, 0025h, 0025h, 002Dh, 0025h, 0025h, 0005h, 0025h,   0005h, 0005h, 0005h, 000Dh, 0015h, 0015h, 0025h, 0005h ; 7

                .DW      0001h, 0001h, 0001h, 0001h, 0011h, 0011h, 0021h, 0001h,   0001h, 0001h, 0001h, 0001h, 0011h, 0011h, 0021h, 0001h ; 8
                .DW      0001h, 0001h, 0001h, 0001h, 0011h, 0011h, 0021h, 0001h,   0001h, 0001h, 0001h, 0001h, 0011h, 0011h, 0021h, 0001h ; 9
                .DW      0005h, 0005h, 0005h, 000Dh, 0015h, 0015h, 0025h, 0005h,   0005h, 0005h, 0005h, 000Dh, 0015h, 0015h, 0025h, 0005h ; A
                .DW      0005h, 0005h, 0005h, 000Dh, 0015h, 0015h, 0025h, 0005h,   0005h, 0005h, 0005h, 000Dh, 0015h, 0015h, 0025h, 0005h ; B
                .DW      0301h, 0201h, 0203h, 0203h, 0003h, 0001h, 0002h, 0401h,   0301h, 0301h, 0203h, 0032h, 0203h, 0203h, 0002h, 0401h ; C - note CB: All 2 bytes so no prefix decode
                .DW      0301h, 0001h, 0203h, 0002h, 0203h, 0001h, 0002h, 0401h,   0301h, 0001h, 0203h, 0002h, 0203h, 0080h, 0002h, 0401h ; D
                .DW      0301h, 0011h, 0203h, 0011h, 0203h, 0011h, 0002h, 0401h,   0301h, 0511h, 0203h, 0001h, 0203h, 0000h, 0002h, 0401h ; E
                .DW      0301h, 0001h, 0203h, 0201h, 0003h, 0001h, 0002h, 0401h,   0301h, 0011h, 0203h, 0001h, 0203h, 0080h, 0002h, 0401h ; F

; Cmd jump table. 26 entries one for each letter. Every command starts with a letter. Each entry is the address of the handler.
CMD_TABLE:      .DW      0         ; A
                .DW      BANK      ; B
                .DW      COPY      ; C
                .DW      DUMP      ; D
                .DW      0         ; E
                .DW      FILL      ; F
                .DW      GO        ; G
                .DW      0         ; H
                .DW      0         ; I
                .DW      0         ; J
                .DW      0         ; K
                .DW      LOAD      ; L hhhh    : hex offset to add to all addresses
                .DW      MODIFY    ; M hhhh    : start writing bytes at specified address
                .DW      0         ; N
                .DW      0         ; O
                .DW      SET_PC    ; P hhhh    : set PC to this address. 'G' and 'S' will then use this address
                .DW      0         ; Q
                .DW      SHOW_RGS  ; R         : show register values. R NAME=VALUE - set specific register values
                .DW      SSTEP     ; S         : step one instruction
                .DW      SSTEP_S   ; T
                .DW      UPGRADE   ; U         : upgrade monitor image from memory @2
                .DW      0         ; V
                .DW      0         ; W
                .DW      0         ; X
                .DW      0         ; Y
                .DW      FLASH_OP  ; Z


; ---------------------- VARIABLES
LAST_CMD:  .DB    0
DUMP_ADDR: .DW    0
DUMP_MODE: .DB    'I'
DUMP_CHRS: .TEXT  "  "
           .DS   16
           .DB    0
INPTR      .DW  INBUF
INBUF      .DS   80
END_INBUF  .DB    0

; Storage area for working registers
R_SP       .DW    6ff0h
R_PC       .DW    2000h
R_AF       .DW    0
R_BC       .DW    0
R_DE       .DW    0
R_HL       .DW    0
R_IX       .DW    0
R_IY       .DW    0

; Breakpoints. Each entry is 6 bytes:
; Type|X|AddrX2|B1|B2
; A breakpint in placed in memory by replacing an instruction with two bytes RST 20h ; 'idx' where 'idx'
; is the index into the breakpoint table.
; Type: 0 - unused slot
;       1 - single shot - removed once hit
;       2 - permanent breakpoint (TBD)

BPOINTS:   .DS    NUM_BK*5

LAST_ADDR: .DB    0

.END
