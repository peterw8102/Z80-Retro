import config.asm
import defs.asm


; import libsio.asm

; External dependancies
; RST 08h - Write a single character to the terminal, character in A
; RST 10h - Read one character from the terminal. Block until there is a character. Returned in A
; RST 18h - Check whether there is a character available. Non blocking. Z flag set if there is NOT a character waiting.

           extrn  PRINT,PRINT_LN,GET_LINE,SKIPSPC,WASTESPC,BUFCHR,WRITE_8,WRITE_16,INHEX_2,INHEX_4,GET_HEX,INPTR,INBUF
           extrn  DISASS,DISBUF
           public START,LAST_CMD,BP_RST

           ASEG
           ORG     20h
BP_RST:    JR      DO_BP

; Number of breakpoint locations (in code)
NUM_BK:   .EQU    64
BP_SIZE   .EQU     4

          CSEG

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
          JR    SHOW_RGS
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
          ; Load from jump table
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

; ------------------- copy - TBD
COPY:     JR    main
; ------------------- upgrade
; New image at 1000. Copy to zero as a single operation
if PLATFORM

UPGRADE:  LD    HL,DO_COPY
          LD    DE,LOADER
          LD    BC,COPYEND-DO_COPY+1
          LDIR
          JR    LOADER

DO_COPY:  LD    HL,MON_COPY
          LD    DE,0
          LD    BC,LAST_ADDR+1000h
          LDIR
          RST   00H  ; Probably won't get here!
COPYEND:

endif
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
          CALL  PRINT
          JR    main


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
; ------------------- SHOW_RGS
SHOW_RGS: LD    HL,SAVE_POS
          CALL  PRINT

          ; Builty a text representation of the Flags in the 'INBUF' ready to be displayed.
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

          ; Write out the 7 16 bit registers
          LD    HL,REG_DESC
          LD    DE,R_PC
          LD    B,7

          ; Output description
_nextreq: PUSH  DE
          LD    E,(HL)
          INC   HL
          LD    D,(HL)
          INC   HL
          EX    DE,HL
          CALL  PRINT
          POP   HL
          PUSH  DE
          LD    E,(HL)
          INC   HL
          LD    D,(HL)
          INC   HL
          EX    DE,HL
          CALL  WRITE_16
          POP   HL
          DJNZ  _nextreq

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

; ------------------- BP: Set breakpoint
; Get address from command line. If not specfied then BP at the current PC value
BP:       CALL  GET_HEX          ; Address for breakpoint
          JR    Z, f_err
          ; Set a BP at the address in HL
          LD    A,2              ; BP type
          CALL  SETBP
          JR    main
; ------------------- MODIFY
MODIFY:   CALL  GET_HEX          ; Start address
          JR    Z, f_err
          ; Sit in a loop processing lines.
_nextln:  CALL  WRITE_16
          WRITE_CHR ':'
          WRITE_CHR ' '
          CALL  GET_LINE
          LD    B,0
_nexthx:  CALL  WASTESPC
          JR    Z, _eoln
          CALL  INHEX_2
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

; -- DECINST
; Decode and display single instruction. HL points to the sart of the instruction. Displays
; the HEX bytes for this instruction followed by a newline.
; INPUT:  HL - the address of the instruction
; OUTPUT: HL - First byte of _next_ instruction
; Registers not saved: A
DECINST:  PUSH  BC         ; DISASS returns instruction information we don't need in BC
          PUSH  DE
          CALL  WRITE_16   ; Write out the address
          WRITE_CHR SPC
          PUSH  HL
          CALL  DISASS     ; HL now points at the description
          LD    B,A        ; The length of the instruction
          LD    D,H        ; Save pointer to disassembled string
          LD    E,L
          POP   HL         ; Back to pointing to the start of the instruction
_nextb:   LD    A,(HL)
          CALL  WRITE_8
          WRITE_CHR SPC
          INC   HL
          DJNZ  _nextb

          ; Output disassembler description
          PUSH  HL         ; Save instruction pointer
          LD    HL,COLSTR
          CALL  PRINT
          LD    H,D
          LD    L,E
          CALL  PRINT
          POP   HL

          ; Row complete
          WRITE_CRLF
          POP   DE
          POP   BC
          RET

; --------------------------- DUMP decode (instruction decode and display) MIGHT become
; a disassembler at some point.
decode:   LD    B,20       ; Number of instructions
_nexti:   CALL  DECINST
          DJNZ  _nexti
          LD    (DUMP_ADDR), HL
          JR    main
; ------------------- run
DUMP:     LD    A,(DUMP_MODE)
          LD    B,A               ; Accepted mode in 'B'
          LD    HL, (DUMP_ADDR)
          CALL  WASTESPC
          JR    Z,cnt_dump

          ; Accept an 'M' or 'I' to set the mode.
          CP    'G'               ; If >= 'G' then it can't be the start of a hex number so it must be mode.
          JR    C,no_mode
          CALL  SKIPSPC
          LD    B,A               ; Whatever the character is, we store this as the mode
          LD    (DUMP_MODE), A

no_mode:  CALL  GET_HEX           ; OFFSET to apply to hex records. Z flag set if 'break' pressed.
cnt_dump: LD    A,B
          CP    'M'
          JR    NZ,decode         ; Mode is not 'M' so it's an instruction level dump

          ; Display 8 blocks of 16 characters
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
LOAD:     CALL  GET_HEX           ; OFFSET to apply to hex records. Z flag set if 'break' pressed.
          LD    D,H               ; Save HL
          LD    E,L
          LD    HL, _WAITING
          CALL  PRINT_LN
          LD    H,D
          LD    L,E
nextline: CALL  GET_LINE
          JR    Z, nextline
          CALL  BUFCHR
          CP    ':'
          JR    NZ,invalid
          ; Accept this line. Format: [LEN 1][ADDR 2][REC_TYPE 1][DATA 2]+[CHKSM 1]
          CALL  INHEX_2            ; Length -> A
          JR    C, rec_err
          OR    A
          JR    Z,rec_err
          LD    B,A                ; Length into B
          CALL  INHEX_4            ; Address - HL
          JR    C, rec_err
          ; CALL  here
          CALL  INHEX_2            ; Command - which should be 00. If not then EOF so end.
          JR    C, rec_err
          OR    A
          JR    NZ, impeof
          LD    A,B                ; Check for zero length
          ADD   HL,DE              ; Modify the address using the offset in DE
next_b:   CALL  INHEX_2            ; Get a hex byte
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
          LD    D,H             ; Save HL (BP address)
          LD    E,L
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
          ; If we get here then can't set a BP - error
          LD    HL,_nobpavail
          CALL  PRINT_LN
          POP   HL
          POP   DE
          POP   BC
          POP   BC               ; Lose return address
          JR    main
fndbp:    LD    (HL),C           ; HL points at the BP record to be used and A is the BP type
          INC   HL
          LD    (HL),E           ; Next 2 bytes are the address at which thi BP is set
          INC   HL
          LD    (HL),D
          INC   HL               ; HL now points at the place to save the breakpoint
          ; If 'type' is 1 then set the BP. Any other value and the BP
          ; is set at runtime
          DEC   C
          JR    NZ,_dupbp
          LD    A,(DE)           ; DE is the address at which we're setting the BP. We need the op-code stored there (in A)
          LD    (HL),A           ; And sore that in the BP record
          EX    DE,HL
          LD    (HL),BRK_OPCODE
_dupbp:   POP   HL
          POP   DE
          POP   BC
          RET

; ------ FINDBP - Find the BP at the address pointed to by HL. Result returned in DE.
; DE - Address of BP (IN)
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
; is being set at the current PC location then it's skipped.
; A:  0 - set the breakpoints
; A: !0 - remove the breakpoints
INSTBP:   LD    C,A
          LD    HL,BPOINTS
          LD    B,NUM_BK                ; Check whether a BP exists at this location
_inextbp: LD    A,(HL)
          CP    2                       ; Only interested in permanent BPs
          CALL  Z,_insbp
          INC   HL                      ; Move on to the next one
          INC   HL
          INC   HL
          INC   HL
          DJNZ  _inextbp
          RET                           ; Can't swt a BP here

_insbp:   PUSH  HL                      ; Store a BP at this location.
          INC   HL
          LD    E,(HL)
          INC   HL
          LD    D,(HL)                  ; DE is the address in code to set the BP
          INC   HL                      ; HL points to where to store the code we replace
          LD    A,C                     ; Flag - NZ: remove, Z: set
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
          EX    DE,HL
          LD    (HL),BRK_OPCODE
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
          LD    (HL),0      ; Clear the 'type' field to free this slot
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
SSTEP:    LD    E,1         ; A -> !0
          CALL  SSTEP_BP    ; Set single step BP then go
; ------------------- go
GO:       XOR   A
          CALL  INSTBP      ; Install all permanent breakpoints
          LD    HL,DO_BP    ; Write the DP_BP handler into the low level break handler.
          LD    (13h),HL
          CALL  REST_RGS    ; Restore registers
          LD    (MON_SP),SP ; Save our stack pointer
          LD    SP,(R_SP)   ; and load the application SP
          JR    JP_RUN      ; And......... GO!

SSTEP_BP: LD    HL, (R_PC)  ; HL is the current application SP
          PUSH  HL
          CALL  DISASS      ; Only disassemble to get control flow information
          POP   HL          ; And get the PC back again (WHY?)
          ; A: Instruction length
          ; C: Extended status
          ; HL: Unchanged - start of THIS instruction
          ADD   A,L
          JR    NC,_nc1
          INC   H
_nc1:     LD    L,A         ; Step past the current instruction
          LD    A,1         ; BP type
          CALL  SETBP       ; HL point to next instruction
          LD    A,C         ; Control flow from this instruction
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

          DEC   HL         ; It's relative. HL points to the NEXT instruction and want the previous byte
          LD    A,(HL)     ; A now contains the relative offset (2s comp)
          INC   HL
          LD    E,A
          ADD   A,A        ; Sets the carry flag if bit 7 set
          SBC   A,A        ; Zero or FF depending on C flag - ie
          LD    D,A        ; DE = sign extended 16 bit offset
          ADD   HL,DE      ; Work out the target of the jump
_setbp:   LD    A,1
          CALL  SETBP      ; And set the BP
          RET
_type2:   DEC   A
          JR    NZ,_type3

_absjp:   DEC   HL         ; Absolute address - use last 2 bytes of the instruction
          LD    D,(HL)
          DEC   HL
          LD    E,(HL)
          EX    DE,HL      ; HL is now the target of the hump
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
          ; It's an RSTxx instruction. We don't try to step into these right now. They are generally system calls
          ; and it's not good to tamper!
          RET   Z

_type5:   DEC   A
          JR    NZ,_type6
          LD    HL,(R_PC)  ; JP (XX) - HL, IX, IY. Instruction determines which
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
          JR    _absjp   ; Treat it as an absolute jump

; ------------------- DO_BP
; This is the target of the BP handler
DO_BP:    DI
          LD   (R_BC),BC      ; Save main registers
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
          JR    SHOW_RGS

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

; --------------------- STRINGS
_INTRO:   DEFB ESC,"[2J",ESC,"[1m",ESC,"[1;6HSTACK",ESC,"[m",ESC,"[11;50r",ESC,"[9;1H>",ESC,"[12,1HZ80 CLM 1.3",CR,LF,"Ready...",CR,LF,NULL
_PROMPT:  DEFB "> ",0
_ERROR:   DEFB "unknown",0
_NOSTART: DEFB CR,LF,"Missing ':'",NULL
_OUTMSG:  DEFB CR,LF,"Out: ",NULL
_REC_ERR: DEFB CR,LF,"Bad rec",NULL
_WAITING: DEFB "Waiting...",NULL
_COMPLETE:DEFB CR,LF,"Complete",0

if PLATFORM
_FLSH_PRG:DEFB "Flash prog",0
endif

; Register labels
R_PC_DESC: DEFB ESC,"[11;50r",ESC,"[2;40H  PC: ",NULL
R_SP_DESC: DEFB ESC,"[3;40H  SP: ",NULL
R_A_DESC:  DEFB ESC,"[4;40H  A:  ",NULL
R_BC_DESC: DEFB ESC,"[2;60H  BC: ",NULL
R_DE_DESC: DEFB ESC,"[3;60H  DE: ",NULL
R_HL_DESC: DEFB ESC,"[4;60H  HL: ",NULL
R_F_DESC:  DEFB ESC,"[5;60H  F:  ",NULL
R_IX_DESC: DEFB ESC,"[5;40H  IX: ",NULL
R_IY_DESC: DEFB ESC,"[6;40H  IY: ",NULL
R_WIN_TOP: DEFB ESC,"[0;12r",NULL
R_WIN_BOT: DEFB ESC,"[13;50r",NULL


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

; Alternate command table format: LETTER:ADDRESS
CMD_TABLE:      DB       'B'
                DW        BP
                DB       'C'
                DW        COPY
                DB       'D'
                DW        DUMP
                DB       'F'
                DW        FILL
                DB       'G'
                DW        GO
                DB       'L'
                DW        LOAD
                DB       'M'
                DW        MODIFY
                DB       'N'
                DW        NSTEP
                DB       'O'
                DW        OUTPUT
                DB       'R'
                DW        SET_RGS
                DB       'S'
                DW        SSTEP
                DB        0

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

 ; Table driven the register display. A is the only one this doesn't work for
 REG_DESC: DEFW    R_PC_DESC
           DEFW    R_SP_DESC
           DEFW    R_BC_DESC
           DEFW    R_DE_DESC
           DEFW    R_HL_DESC
           DEFW    R_IX_DESC
           DEFW    R_IY_DESC

           DSEG
; ---------------------- VARIABLES
; If you want the monitor code in ROM then add an ORG here to locate variables somewhere in RAM
LAST_CMD:  DEFB    'R'
DUMP_ADDR: DEFW    0
DUMP_MODE: DEFB    'I'
DUMP_CHRS: DEFB  "  "
           .DS   16
           DEFB    0

; Temporary storage for Flash programming
FL_TO_ADDR DEFW   0

; JP_RUN - C3 is the JP opcode. By jumping to JP_RUN execution will
; continue from the current value of the PC. This avoids us having to
; push values onto the applications stack.
JP_RUN:    DEFB    0
; Storage area for working registers
R_PC       DEFW    2000h
R_SP       DEFW    6FF0h ; Initial application stack is NOT the same as ours
R_BC       DEFW    0
R_DE       DEFW    0
R_HL       DEFW    0
R_IX       DEFW    0
R_IY       DEFW    0
R_AF       DEFW    0

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
