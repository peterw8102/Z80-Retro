; import config.asm
import ../zlib/defs.asm


          extrn  WRITE_8,HEX_FROM_A
          extrn  SCRATCH
          public DISASS,SETOFF

          ASEG
          ORG     $4000
          PHASE   $C000



; Experimental simple Z80 disassembler which *might* replace the sime line dump DI operation
; ---- DISASS
; Disassembler the instruction pointed to by HL. All registers saved EXCEPT HL and AF.
; INPUT:  HL - pointer to instruction
; OUTPUT: HL - Pointer to description
;          A - The number of bytes in this instruction
;          C - Opcode meta data. Lower 3 bits:
;              001 - relative : last byte of instruction is PC relative offset
;              010 - absolute : last two bytes of inststruction is an absolute address
;              011 - return   : look at the stack to find the next instruction
;              100 - rst      : RST xxH call
;              101 - special  : JR (HL)(IX)(IY) - special processing required.
;              110 - call     : Call to absolute address

DISASS:   PUSH  DE
          PUSH  HL
          ; Reset output buffer
          EX    DE,HL
          LD    HL,DISBUF
          LD    (DISPTR),HL
          EX    DE,HL

          ; Set prefix length to zero
          XOR   A
          LD    (PREFLEN),A
          LD    (CTRLFLOW),A

          ; And start disassembling from HL
          CALL  SPLIT_OP

         ; Check for prefix
          CP    $DD
          LD    DE,REG_IX
          JR    Z,_pref_xy
          CP    $FD
          LD    DE,REG_IY
          JR    Z,_pref_xy
          CP    $ED
          JR    Z,_pref_ed
          CP    $CB
          JR    NZ,_no_pref

          ; CB pref so it's a rotate or a bit operation. Get next byte
          POP   HL
          INC   HL
          CALL  SPLIT_OP
          PUSH  HL
          AND   $C0
          JR    NZ,_bit
          ; Bit operation - 'y' selects the operation
          LD    A,B
          LD    HL,_mapShift
          CALL  PRT_LU_T
          ; And the register to operate on
          LD    A,C
_prtret2: CALL  DESCREG8
_ret2:    LD    A,2       ; All these are two bytes
          JR    _ret1a

_bit:     LD    A,C
          RLCA
          RLCA
          DEC   A
          LD    HL,_mapBits
          CALL  PRT_LU_T
          LD    A,B
          ADD   '0'
          CALL  PRTCHR
          CALL  PRTCOM
          LD    A,C
          RRCA
          RRCA
          RRCA
          JR    _prtret2

; ------ _pre_xy - the last byte was DD or FD so an IX or IY operation. However
; multiple prefix bytes cancel each other. Check for multiple sequences of prefix
_pref_xy: LD    (EXT_REG),DE
          LD    B,0              ; Number of prefix bytes
          POP   HL
_nxtpref: INC   B
          INC   HL
          LD    C,A              ; This is the last prefix that matched.
          LD    A,(HL)
          LD    D,A              ; This will be the first byte AFTER the prefix
          CP    $DD
          JR    Z,_nxtpref
          CP    $FD
          JR    Z,_nxtpref
          CP    $ED
          JR    Z,_nxtpref

          LD    A,B
          LD    (PREFLEN),A      ; Number of extra prefix bytes

          ; Byte in C is the effective prefix
          ; Byte in A is the prefixed opcode
          ; HL points to the opcode held in A
          PUSH  HL
          LD    A,C                ; Take a look at the prefix
          CP    $ED
          JR    Z,_pref_ed

          LD    A,D                ; First byte after opcode

          ; It's an IX/IY operation. They are the same but have the right register stored in EXT_REG
          CALL  SPLIT_OP
          ;     A, C: Still opcode
          ;     B:    Y
          AND   $C0
          JR    NZ,_ddx1
          ; x=0
          LD    A,C
          AND   $0F
          CP    9
          JR    NZ,_ddx0_1
          ; ADD into IDX register
          LD    HL,OP_ADD
          CALL  PRTBUF_T
          LD    HL,(EXT_REG)
          CALL  PRTBUF
          LD    A,B
          RRCA
          LD    HL,_reg16_a
          CALL  TAB_LU
          CALL  PRTCOM
          CALL  PRTBUF
          JR    _ret1

_ddx0_1:  LD    A,B                ; Look for Y=6
          CP    6
          JR    NZ,_not6
          ; Only 3 possible instructions here with opcodes 34, 35, 36. All have an offset byte
          ; Decide on the operation first
          LD    A,C
          LD    HL,_idx_alu
          CALL  PRT_LU_T

          ; All of these operations have (reg+offset) as the first argument
          POP   HL
          CALL  IDX_INDIR
          LD    A,C
          CP    $36
          JR    Z,_imidx          ; No immediate value required
          LD    A,2
          JR    _ret1b

_imidx:   CALL  PRTCOM
          CALL  ABS_8
          LD    A,3
          JR    _ret1b

_not6:    CCF
          RRA
          CP    2                 ; Y=10x
          JR    NZ,_noty10x
          LD    A,C
          CP    $21
          JR    NZ,_not21         ; LD idx,**
          LD    HL,OP_LD
          CALL  PRTBUF_T
          LD    HL,(EXT_REG)
          CALL  PRTBUF_C
          POP   HL
          CALL  ABS_16
_ret3:    LD    A,3
          JR    _ret1b

_not21:   CP    $22               ; LD (**),idx
          JR    NZ,_not22
          LD    HL,OP_LD
          CALL  PRTBUF_T
          POP   HL
          CALL  INDIR_16
          CALL  PRTCOM
          LD    HL,(EXT_REG)
          CALL  PRTBUF
          JR    _ret3

_not22:   CP    $2A
          JR    NZ,_not2a         ; LD idx,(**)
          LD    HL,OP_LD
          CALL  PRTBUF_T
          LD    HL,(EXT_REG)
          CALL  PRTBUF_C
          POP   HL
          CALL  INDIR_16
          JR    _ret3

_not2a:   LD    A,C
          AND   $F7
          CP    $23
          JR    NZ,_not23b
          LD    HL,OP_INC
          BIT   3,C
          JR    Z,_isinc
          LD    HL,OP_DEC
_isinc:   CALL  PRTBUF_T
          LD    HL,(EXT_REG)
          CALL  PRTBUF
          JR    _ret1

_not23b:  LD    A,C
          LD    HL,_idx_alu
          CALL  PRT_LU_T
          LD    HL,(EXT_REG)
          CALL  PRTBUF
          LD    A,'H'
          BIT   3,C             ; H or L
          JR    Z,_high2
          LD    A,'L'
_high2:   CALL  PRTCHR
          LD    A,C
          AND   7
          CP    6
          JR    NZ,_ret1
          CALL  PRTCOM
          POP   HL
          CALL  ABS_8
          LD    A,2
          JR    _ret1b

_noty10x: CALL  WRITE_8
          JR    _ret1a;

_ddx1:    CP    $40
          JR    NZ,_ddx2
          ; All LD instructions although many invalid. y and z still identify the src/dst
          LD    D,1               ; Length of this instruction
          LD    HL,OP_LD
          CALL  PRTBUF_T
          LD    A,C
          AND   7
          CP    6                 ; If this is '6' then the second op must be a reg
          LD    A,B
          POP   HL
          JR    NZ,_ind_1
          ; It's a 6 so the destination is a normal reg
          CALL  DESCREG8
          INC   D
          JR    _dodest
_ind_1:   CALL  DESCREGX
          ; Add an extra byte to the op length
_dodest:  CALL  PRTCOM
          LD    A,B
          CP    6
          LD    A,C
          JR    NZ,_ind_2
          CALL  DESCREG8
          INC   D
          JR    _lddn
_ind_2:   CALL  DESCREGX
          ; Add an extra byte to the op length
_lddn:    LD    A,D
          JR    _ret1b

_ddx2:    CP    $80
          JR    NZ,_ddx3
          ; y determines the operation
          LD    A,B
          LD    HL,_mapArith
          CALL  PRT_LU
          ; The argument is determined by the bottom two bits:
          ; 00:  IDH
          ; 01:  IDL
          ; 10: (ID+X)
          LD    A,C
          LD    D,1
          AND   3
          CP    2
          JR    NZ,_ddx2a
          INC   D
_ddx2a:   OR    4
          POP   HL
          CALL  DESCREGX
          LD    A,D
          JR    _ret1b

_ddx3:    LD    A,C                ; $C0 - this is the only option left
          CP    $CB
          JR    Z,_idxbits

          CP    $E9
          JR    NZ,_notidjp
          ; It's JP (IDX)
          LD    HL,OP_JP
          CALL  PRTBUF_T
          LD    A,'('
          CALL  PRTCHR
          LD    HL,(EXT_REG)
          CALL  PRTBUF
          LD    A,')'
          CALL  PRTCHR
          LD    A,CT_IND          ; It's a jump through a register
          LD    (CTRLFLOW),A
          JR    _ret1

_notidjp: LD    HL,_desc_2
          CALL  SRCH_TAB
          LD    HL,(EXT_REG)
          CALL  PRTBUF
          JR    _ret1

; Processing FD/DD CB - bit ops using IDX registers
_idxbits: POP   HL
          INC   HL
          CALL  SPLIT_OP
          PUSH  HL
          AND   $C0
          JR    NZ,_bitx
          LD    A,B
          LD    HL,_mapShift
          CALL  PRT_LU_T
          ; And now an index+offset indirection
          POP   HL
          CALL  IDX_INDIR
          ; Unless 'z'=6 then need to output ,reg
_opreg:   LD    A,C
          AND   7
          CP    6
          JR    Z,_noreg
          LD    HL,_reg8a
          CALL  TAB_LU           ; Result in HL
          CALL  PRTCOM
          CALL  PRTBUF

          ; Finished -
_noreg:   LD    A,3
          JR    _ret1b

_bitx:    RLCA       ; Move Z to lower two bits
          RLCA
          DEC   A    ; Get range 0-2
          LD    HL,_mapBits
          CALL  PRT_LU_T
          ; Now the bit number, which
          LD    A,B
          ADD   '0'
          CALL  PRTCHR
          CALL  PRTCOM
          ; Now the index offset
          POP   HL
          CALL  IDX_INDIR
          ; And if Y is not 6 then also need to output the register
          JR    _opreg





_pref_ed: POP   HL                 ; Byte after ED is the opcode to decode
          INC   HL
          CALL  SPLIT_OP           ; C: opcode, B: bits 3-5 >> 3, A: opcode
          PUSH  HL
          AND   A,$C0              ; X=1
          JR    Z,_ret2            ; X=0 is invalid but counts as a 2 byte NOP
          CP    $C0
          JR    Z,_ret2            ; X=3 is also invalid but counts as a 2 byte NOP

          ; That leaves X=1 or 2
          CP    $40
          JR    NZ,_edx2
_edx1:    LD    A,C                ; Top two bits are 01. Get opcode back
          AND   7                  ; Decode Z
          JR    NZ,_edz1
          LD    HL,OP_IN_R
_outc:    CALL  PRTBUF_T
          ; Value in B (which is y) decides on which operation we're looking at
          LD    A,B
          CP    6
          JR    Z,_inflgs
          CALL  DESCREG8
          CALL  PRTCOM
_inflgs:  LD    HL,IND_C
_prt2:    CALL  PRTBUF
          JR    _ret2

_edz1:    DEC   A
          JR    NZ,_edz2
          LD    HL,OP_OUT_R       ; It's an OUT via C. Two options OUT (C),0 or OUT (C),reg8
          CALL  PRTBUF_T
          LD    HL,IND_C
          CALL  PRTBUF
          CALL  PRTCOM
          LD    A,B
          CP    6
          JR    Z,_outz
          CALL  DESCREG8
          JR    _ret2
_outz:    LD    A,'0'
          CALL  PRTCHR
          JR    _ret2

_edz2:    DEC   A
          JR    NZ,_edz3
          ; SBC/ADC HL,reg
          LD    HL,OP_SBC
          BIT   0,B
          JR    Z,_sbchl
          LD    HL,OP_ADC
_sbchl:   CALL  PRTBUF_T
          LD    HL,HL_1ST
          CALL  PRTBUF
          LD    A,B
          RRCA
          LD    HL,_reg16_a
          CALL  PRT_LU
          JR    _ret2

_edz3:    DEC   A
          JR    NZ,_edz4
          LD    HL,OP_LD
          CALL  PRTBUF_T
          LD    A,B
          RRCA                    ; Bit 0 determines direction
          JR    C,_ld_16in
          POP   HL
          CALL  INDIR_16
          PUSH  HL
          LD    B,A
          CALL  PRTCOM
          LD    A,B
          LD    HL,_reg16_a
          CALL  PRT_LU
_ret4:    LD    A,4               ; 4 byte instruction
          JR    _ret1a

_ld_16in: LD    HL,_reg16_a
          CALL  PRT_LU
          CALL  PRTCOM
          POP   HL
          CALL  INDIR_16
          PUSH  HL
          JR    _ret4


_edz4:    DEC   A
          JR    NZ,_edz5
          LD    HL,OP_NEG
          CALL  PRTBUF
          JR    _ret4

_edz5:    DEC   A
          JR    NZ,_edz6
          LD    A,B
          DEC   A
          LD    HL,OP_RETI
          JR    Z,_prtret
          LD    HL,OP_RETN
_prtret:  CALL  PRTBUF
          LD    A,CT_RET
          LD    (CTRLFLOW),A
          JR    _ret2

_edz6:    DEC   A
          JR    NZ,_edz7
          LD    HL,OP_IM
          CALL  PRTBUF_T
          ; And the mode is taken from the 'y' bits held in B
          LD    HL,_immode
          LD    A,B
          JR    _luz7

_edz7:    LD    A,B
          LD    HL,_map_ed_z7
_luz7:    CALL  PRT_LU
          JR    _ret2


_edx2:    BIT   2,B
          JR    Z,_inved
          LD    A,B

          ; It's a block operation
          LD    A,C                ; Bottom 2 bits of the opcode
          AND   3
          ADD   B                  ; And 4x bits 3-5
          ADD   B
          ADD   B
          ADD   B
          LD    HL,_map_blk
          CALL  PRT_LU
          JR    _ret2

_inved:   LD    HL,OP_NOP
          CALL  PRTBUF
          JR    _ret2

_no_pref: LD    HL,_desc_1
          CALL  SRCH_TAB
          JR    Z,_notfnd
_ret1:    LD    A,1                 ; Length of the instruction - 1
_ret1a:   POP   DE                  ; Don't need the save HL here - waste it
_ret1b:   LD    D,A
          LD    A,(CTRLFLOW)
          LD    C,A
          ; CALL  WRITE_8
          LD    A,(PREFLEN)
          ADD   D                   ; Add any accumulated prefix bytes
          POP   DE
          LD    HL,DISBUF
          RET

          ; At the end of the exception table so move on to the next test.
          ; The following tests break out the opcode into the top 2 bits
_notfnd:  AND   $C0
          JR    NZ,_next_1

          ; Decode the bottom 3 bits
          LD    A,C
          AND   7
          JR    NZ, _x0z1
          LD    A,C
          BIT   5,A
          JR    NZ,_jrcond
          ; It's either JR or DJNZ. NOP and EX AF.AF already dealt with as an exception
          BIT   3,A
          JR    Z,_dnjz
          LD    HL,OP_JR
          JR    _jrnoc
_dnjz:    LD    HL,OP_DJNZ
_jrnoc:   CALL  PRTBUF_T
          JR    _offs_r
_jrcond:  LD    HL,OP_JR
          CALL  PRTBUF_T
          ; Decode the condition
          LD    A,B        ; Bits 3-5 (Y)
          AND   3
          LD    HL,_mapFlag
          CALL  PRT_LU
          CALL  PRTCOM
_offs_r:  POP   HL         ; Instruction pointer
          INC   HL
          PUSH  HL
          LD    A,(HL)
          LD    E,A
          CALL  PRTHEX
          LD    D,0
          LD    A,E
          RLCA
          JR    NC,_fwd
          DEC   D
_fwd:     INC   HL
          ADD   HL,DE       ; Local address in now in DE. Scope in the current adjustment
          LD    DE,(OFFSET) ; Offset specified.
          ADD   HL,DE
          PUSH  HL
          LD    HL,_toaddr
          CALL  PRTBUF
          POP   HL
          LD    A,H
          CALL  PRTHEX
          LD    A,L
          CALL  PRTHEX
          LD    A,'h'
          CALL  PRTCHR

          LD    A,CT_REL
          LD    (CTRLFLOW),A
          LD    A,2        ; All these instructions are 2 bytes
          JR    _ret1a

_x0z1:    DEC   A
          JR    NZ, _x0z2
          ; Load immediate or ADD HL depending on bit 3
          LD    A,B        ; Bits 3-5 (Y)
          RRCA             ; Test bit 0 of y (q)
          JR    C,_addhl
          ; It's a load immediate so next two bytes are the 16 bit value
          LD    HL,OP_LD
          CALL  PRTBUF_T
          ; The 16 bit register should still be in A
          LD    HL,_reg16_a
          CALL  PRT_LU
          CALL  PRTCOM
          ; Now the 16 bit value - next 2 bytes of the instruction
          POP   HL
          INC   HL
          LD    E,(HL)
          INC   HL
          LD    D,(HL)
          INC   HL
          PUSH  HL
          LD    A,D
          CALL  PRTHEX
          LD    A,E
          CALL  PRTHEX
          LD    A,'h'
          CALL  PRTCHR
          LD    A,3             ; 3 byte instruction
          JR    _ret1a

_addhl:   LD    HL,OP_ADDHL
          CALL  PRTBUF
          LD    HL,_reg16_a
          CALL  PRT_LU
          JR    _ret1           ; Still a single byte operation

          JR    _next_2

_x0z2:    DEC   A
          JR    NZ, _x0z3
          LD    A,B             ; Bits 3-5 (Y)
          BIT   2,A
          JR    NZ,_ind1
          ; Load A indirect through BC/DE - All single byte
          LD    HL,_map000xx010
          CALL  PRT_LU
          JR    _ret1
_ind1     ; Ld A/HL from 16 bit address. 3 byte instructions - next 2 bytes are the address
          LD    HL,OP_LD
          CALL  PRTBUF_T
          RRA                    ; Determines the direction 0: (addr) -> reg
          JR    C,_frmmem1
          ; Saving register TO memory
          POP   HL
          CALL  INDIR_16
          ; Work out the register now
          LD    HL,_regAcc
          CALL  TAB_LU           ; Result in HL
          CALL  PRTCOM
          CALL  PRTBUF           ; The register name we cached
          LD    A,3
          JR    _ret1b
_frmmem1: LD    HL,_regAcc       ; Determines the register
          CALL  PRT_LU
          CALL  PRTCOM
          POP   HL
          CALL  INDIR_16
          PUSH  HL
          LD    A,3
          JR    _ret1a

_x0z3:    DEC   A
          JR    NZ, _x0z4
          ; 16 bit register INC/DEC
          LD    A,B             ; Bits 3-5 (Y)
          RRCA
          JR    C,_dec
          LD    HL,OP_INC
          JR    _inc
_dec:     LD    HL,OP_DEC
_inc:     CALL  PRTBUF_T
          LD    HL,_reg16_a
          CALL  PRT_LU
          JR    _ret1

_x0z4:    DEC   A
          JR    NZ, _x0z5
          LD    HL,OP_INC
          JR    _opreg8

_x0z5:    DEC   A
          JR    NZ, _x0z6
          LD    HL,OP_DEC
_opreg8:  CALL  PRTBUF
          LD    A,TAB
          CALL  PRTCHR

          ; This is an 8 bit register - describe it
          LD    A,B                 ; Bits 3-5 (Y)
          CALL  DESCREG8
          JR    _ret1

_x0z6:    DEC   A                   ; 8bit load immediate
          JR    NZ, _x0z7
          LD    HL,OP_LD
          CALL  PRTBUF              ; HL points to next available character
          LD    A,TAB
          CALL  PRTCHR
          LD    A,B                 ; Bits 3-5 (Y)
          CALL  DESCREG8
          CALL  PRTCOM
          POP   DE
          INC   DE
          PUSH  DE
          LD    A,(DE)
          CALL  PRTHEX
          LD    A,'h'
          CALL  PRTCHR
          LD    A,2                 ; Return instruction length as 2
          JR    _ret1a

_x0z7:    LD    A,B                 ; Bits 3-5 (Y)
          LD    HL,_map00xxx111
          CALL  PRT_LU
          JR    _ret1

_next_1:  CP    $40                 ; Top two bits = '01'? If so then it's a load instruction.
          JR    NZ,_next_2          ; Nope - not this section either

          ; It's an 8 bit load instruction. The remaining lower 6 bits identify the source and destinations
          LD    HL,OP_LD
          CALL  PRTBUF              ; Copy 'LD ' to the output buffer and get the next output position
          LD    A,TAB
          CALL  PRTCHR
          LD    A,B                 ; Dest register bits xx000xxx
          CALL  DESCREG8            ; Output the destination register
          CALL  PRTCOM
          LD    A,C
          CALL  DESCREG8
          LD    HL,DISBUF
          JR    _ret1

_next_2:  CP    80h                 ; ALU operation. Use 'y' to lookup the operation and z to find the register
          JR    NZ,_next_3
          LD    A,B                 ; Dest register bits xx000xxx
          LD    HL,_mapArith
          CALL  PRT_LU
          LD    A,C
          CALL  DESCREG8
          JR    _ret1

_next_3:  LD    A,C                 ; Random collection of operations
          AND   7                   ; Decode further based on bottom 3 bits
          JR    nz,_stk
          ; Z=0 => Single byte conditional return
          LD    HL,OP_RET
          CALL  PRTBUF_T
          ; Set the ret control flow
          LD    A,CT_RET
          LD    (CTRLFLOW),A
          ; Work out the condition
          LD    A,B                 ; Dest register bits xx000xxx
          LD    HL,_mapFlag
          CALL  PRT_LU
          JR    _ret1

_stk:     LD    A,C
          ; Deal with ctrl flow changes first (C9: Ret, E9: JP HL)
          CP    $C9
          JR    NZ,_testjp
          LD    A,CT_RET
          LD    HL,OP_RET
_opct:    LD    (CTRLFLOW),A
          CALL  PRTBUF
          JR    _ret1

_testjp:  CP    $E9
          LD    HL,OP_JPHL
          LD    A,CT_IND
          JR    Z,_opct
          LD    A,C

_itsstk:  AND   11001011b
          CP    11000001b
          JR    NZ,_x3z2
          ; It's EITHER a PUSH or a POP depending on bit 2
          LD    HL,OP_POP
          BIT   2,C
          JR    Z,_pop
          LD    HL,OP_PUSH
_pop:     CALL  PRTBUF_T
          LD    A,B                 ; Dest register bits xx000xxx
          RRCA
          LD    HL,_reg16_b
          CALL  PRT_LU
          JR    _ret1
_x3z2:    LD    A,C
          AND   7
          CP    2
          JR    NZ,_x3z3
          ; It's a conditional JUMP. Exactly the same processing as CALL with a different opcode
          LD    A,CT_ABS            ; Store control flow
          LD    (CTRLFLOW),A

          LD    HL,OP_JP
          JR    _cond_jp

_x3z3:    CP    3
          JR    NZ,_x3z4
          ; Three special cases...
          LD    A,B                 ; Dest register bits xx000xxx
          OR    A
          JR    NZ,_x3z3y1
          ; Absolute, unconditional jump
          LD    HL,OP_JP
          CALL  PRTBUF_T
          ; And the address we're jumping to
          POP   HL
          CALL  ABS_16
          LD    A,CT_ABS            ; Store control flow
          LD    (CTRLFLOW),A
          LD    A,3
          JR    _ret1b
_x3z3y1:  DEC    A                 ; CB prefix - ignore here
          DEC    A
          LD     HL,OP_OUT_R
          JR     Z,_out
          LD     HL,OP_IN
          CALL   PRTBUF
          JR     _nxtout
_out:     CALL   PRTBUF_T
          ; Next byte is the offset
_nxtout:  POP   HL         ; Instruction pointer
          CALL  INDIR_8
          ; If it was an OUT then need to also O/P ',A'
          LD    A,B
          RRCA             ; Test bit 0 of A (y)
          LD    A,2
          JR    C,_ret1b
          LD    HL,OP_A
          CALL  PRTBUF
          JR    _ret1b

_x3z4:    LD    A,C
          AND   7
          CP    4
          JR    NZ,_x3z5
          ; It's a conditional call
          LD    HL,OP_CALL
          LD    A,CT_CALL
          LD    (CTRLFLOW),A

_cond_jp: CALL  PRTBUF_T
          LD    A,B                 ; Dest register bits xx000xxx
          LD    HL,_mapFlag
          CALL  PRT_LU
          CALL  PRTCOM
          ; Next the target absolute address
          POP   HL
          CALL  ABS_16
          PUSH  HL
          LD    A,3                 ; 3 bytess in this instruction...
          JR    _ret1a
_x3z5:    CP    5
          JR    NZ,_x3z6
          ; Only opcode NOT decoded for this pattern is an unconditional CALL
          LD    HL,OP_CALL
          CALL  PRTBUF_T
          POP   HL
          CALL  ABS_16
          LD    A,CT_CALL           ; Save control flow change information for a CALL
          LD    (CTRLFLOW),A
          LD    A,3                 ; Instruction length
          JR    _ret1b

_x3z6:    CP    6
          JR    NZ,_x3z7
          ; ALU immediate data operation - all 2 bytes
          LD    HL,_mapArith
          LD    A,B
          CALL  PRT_LU
          ; And an immediate operation
          POP   HL
          CALL  ABS_8
          LD    A,2
          JR    _ret1b

_x3z7:    LD    HL,OP_RST            ; RST xx
          CALL  PRTBUF_T
          ; Vector is in bits 3-5
          LD    A,C
          AND   38h
          CALL  PRTHEX
          LD    A,'h'
          CALL  PRTCHR
          LD    A,CT_RST            ; Save control flow change information for a CALL
          LD    (CTRLFLOW),A
          JR    _ret1


          LD    HL,_FAIL
          JR    _ret1a

; ----------- ABS_8
; Next byte from HL are an 8 bit number.
; Output NUMh. HL Moved forard 1 bytes. no other registers
; changed
ABS_8:    PUSH  AF
          PUSH  DE
          INC   HL
          LD    A,(HL)
          CALL  PRTHEX
          LD    A,'h'
          CALL  PRTCHR
          POP   DE
          POP   AF
          RET

; ----------- ABS_16
; Next two bytes from HL are a 16 bit absolute address.
; Output ADDR. HL Moved forard 2 bytes. no other registers
; changed
ABS_16:   PUSH  AF
          PUSH  DE
          INC   HL
          LD    E,(HL)
          INC   HL
          LD    A,(HL)
          CALL  PRTHEX
          LD    A,E
          CALL  PRTHEX
          LD    A,'h'
          CALL  PRTCHR
          POP   DE
          POP   AF
          RET

; ----------- IDX_INDIR
; Next byte from HL is an offset byte from an index register. Output
; (reg+offset) and move HL forward
IDX_INDIR: PUSH AF
           PUSH DE
           INC  HL
           LD   E,(HL)   ; E contains the offset to apply
           LD   A,'('
           CALL PRTCHR
           PUSH HL
           LD   HL,(EXT_REG)
           CALL PRTBUF
           LD   A,'+'
           CALL PRTCHR
           LD   A,E
           CALL PRTHEX
           LD   A,'h'
           CALL PRTCHR
           LD   A,')'
           CALL PRTCHR
           POP  HL
           POP  DE
           POP  AF
           RET
; ----------- INDIR_8
; Next two bytes from HL are a 16 bit address. Output (addr).
; HL Moved forard 2 bytes. no other registers changed
INDIR_8:  PUSH  AF
          LD    A,'('
          CALL  PRTCHR
          CALL  ABS_8
          LD    A,')'
          CALL  PRTCHR
          POP   AF
          RET

; ----------- INDIR_16
; Next two bytes from HL are a 16 bit address. Output (addr).
; HL Moved forard 2 bytes. no other registers changed
INDIR_16: PUSH  AF
          LD    A,'('
          CALL  PRTCHR
          CALL  ABS_16
          LD    A,')'
          CALL  PRTCHR
          POP   AF
          RET

; ----------- SPLIT_OP
; Opcode in A. Save in C. Move bits 3-5 to B and divide by 8 (>>3)
SPLIT_OP: LD    A,(HL)
          LD    C,A
          RRCA
          RRCA
          RRCA
          AND   7;
          LD    B,A
          LD    A,C
          RET

; ----------- DESCREGX
; A: Bits 0-2 contains an 8 bit register reference in the
; index register set. In this table if the result is H
; then make this IXH, L -> IXL but where IX is the actual
; index stored in EXT_REG. If the first character is
; '(' then this becomes (IX+*). In this call HL must
; point to the current PC to get the offset.
; HL: Current PC
DESCREGX: PUSH  AF
          PUSH  HL
          LD    HL,_reg8a
          CALL  TAB_LU
          ; HL points to the string so get the character and look
          ; for exceptions.
          LD    A,(HL)
          CP    A,'H'+80h
          JR    Z,_descih
          CP    A,'L'+80h
          JR    Z,_descih
          CP    A,'('
          JR    Z,_idrct
          CALL  PRTBUF
          POP   HL
          POP   AF
          RET
_idrct:   POP   HL     ; Need the PC to work out the offset
          CALL  IDX_INDIR
          POP   AF
          RET
_descih:  LD    HL,(EXT_REG)
          CALL  PRTBUF
          CALL  PRTCHR
          POP   HL
          POP   AF
          RET

; ----------- DESCREG8
; A: Bits 0-2 contains an 8 bit register reference
; Returns HL pointing to next byte in output buffer
DESCREG8: LD    HL,_reg8a

; ------- PRT_LU
; Do a TAB_LU then write the result to the output buffer
PRT_LU:   CALL  TAB_LU ; Result in HL

; -------- PRTBUF
; Print to the output buffer. String  pointed to by HL
; HL string to 'print' - it's an ASCII string with MSB of last character set (HL NOT PRESERVED).
PRTBUF:   PUSH AF
          PUSH DE
          LD   DE,(DISPTR)
_contout: LD   A,(HL)
          LD   (DE),A
          INC  DE
          INC  HL
          BIT  7,A
          JR   Z,_contout
          ; Clear the upper bit of the last write.
          RES  7,A
          EX   DE,HL
          LD   (DISPTR),HL
          LD   (HL),NULL          ; NULL terminator
          DEC  HL
          LD   (HL),A
          POP  DE
          POP  AF
          RET

PRTCHR:   PUSH HL
          LD   HL,(DISPTR)
          LD   (HL),A
          INC  HL
          LD   (HL),NULL
          LD   (DISPTR),HL
          POP  HL
          RET

PRTCOM:   LD    A,','
          JR    PRTCHR

; ---------- PRTBUF_X
; Print to buf from (HL) then append the character in A. No registers saved
PRTBUF_X: CALL PRTBUF
          JR   PRTCHR

PRTHEX:   PUSH  HL
          CALL  HEX_FROM_A        ; A contains the 8 bit value to print to the buffer
          LD    A,H               ; Result in HL
          CALL  PRTCHR
          LD    A,L
          CALL  PRTCHR
          POP   HL
          RET

PRTBUF_T: PUSH AF
          LD   A,TAB
          CALL PRTBUF_X
          POP  AF
          RET

PRTBUF_C: PUSH AF
          LD   A,','
          CALL PRTBUF_X
          POP  AF
          RET

SKIPEND:  LD    A,(HL)
          BIT   7,A
          INC   HL
          JR    Z,SKIPEND
          RET

; ------- TABLU
; HL points to the base of the table.
; A includes the index into the table
; Returns with HL pointing to result if found (which is will be)
TAB_LU:   AND   (HL)   ; Mask
          INC   HL
          OR    A      ; If zero, return this one
          RET   Z
          PUSH  BC
          LD    B,A
_nextlu:  CALL  SKIPEND
          DJNZ  _nextlu
          POP   BC
          RET

; ------- PRT_LU_T
;As TAB_LU but append a TAB character to the output
PRT_LU_T: CALL  TAB_LU ; Result in HL
          CALL  PRTBUF_T
          RET

;  ------------- SRCH_TAB
;  IN -  A,C:  Opcode to search for
;        HL:   Start of search table
;  OUT - A:    Zero if not found
SRCH_TAB: CP    (HL)                ; If zero then match - so return following string
          INC   HL                  ; Step on to the first character of the description
          JR    NZ,_next_s
          CALL  PRTBUF
          OR    1
          RET
_next_s:  CALL  SKIPEND             ; At end of string - HL pointing to the first character of the next value - or 0 for end table
          LD    A,(HL)
          OR    A
          LD    A,C                 ; Restore the opcode
          RET   Z                   ; End of table
          JR    SRCH_TAB

; ------ SETOFF
; Store the memory offset calculation for relative addressing
; HL: Offset to store
SETOFF:  LD (OFFSET),HL
         RET



; Random opcode instructions
OP_ADD:         DC    'ADD'
OP_ADDHL:       DB    'ADD',TAB
                DC    'HL,'
OP_DJNZ:        DC    'DJNZ'
OP_JR:          DC    'JR'
OP_JP:          DC    'JP'
OP_RET:         DC    'RET'
OP_PUSH:        DC    'PUSH'
OP_POP:         DC    'POP'
OP_CALL:        DC    'CALL'
OP_OUT_R:       DC    'OUT'
OP_IN:          DB    'IN',TAB,'A',(','+80h)
OP_IN_R:        DC    'IN'
OP_RST:         DC    'RST'
OP_SBC:         DC    'SBC'
OP_ADC:         DC    'ADC'
OP_NEG:         DC    'NEG'
OP_RETI:        DC    'RETI'
OP_RETN:        DC    'RETN'
OP_IM:          DC    'IM'
OP_JPHL:        DB    'JP',TAB,'H',('L'+80h)


OP_A:           DC    ',A'
IND_C:          DC    '(C)'
HL_1ST:         DC    'HL,'
REG_IX:         DC    'IX'
REG_IY:         DC    'IY'
; C Strings
_FAIL:          DC    "--XX--"

; 16 bit register pairs (set A including SP)
_reg16_a:       DB        3
                DC       'BC'
                DC       'DE'
                DC       'HL'
                DC       'SP'

; Alternate set including AF
_reg16_b:       DB        3
                DC       'BC'
                DC       'DE'
                DC       'HL'
                DC       'AF'

_reg8a:         DB        7
                DC       'B'
                DC       'C'
                DC       'D'
                DC       'E'
                DC       'H'
                DC       'L'
                DC       '(HL)'
                DC       'A'

_regAcc:        DB        1
                DC        'HL'
                DC        'A'

_immode:        DB        3
                DC       '0'
                DC       '0/1'
                DC       '1'
                DC       '2'

; Flag map - this is expressed as a set of high bit strings and can be searched based on the index
_mapFlag:       DB       7       ; Mask to lower 3 bits
                DC       'NZ'
                DC       'Z'
                DC       'NC'
                DC       'C'
                DC       'PO'
                DC       'PE'
                DC       'P'
                DC       'M'

_mapBits:       DB       3
                DC       'BIT'
                DC       'RES'
                DC       'SET'

; Map common arithmetic/accumulator operations
_mapArith:      DB        7      ; Mask to lower 3 bits
                DB       'ADD',TAB,'A',(','+80h)
                DB       'ADC',TAB,'A',(','+80h)
                DB       'SUB',(TAB+80h)
                DB       'SBC',TAB,'A',(','+80h)
                DB       'AND',(TAB+80h)
                DB       'XOR',(TAB+80h)
                DB       'OR',(TAB+80h)
                DB       'CP',(TAB+80h)

; Bit shifting instructions
_mapShift:      DB        7
                DC        'RLC'
                DC        'RRC'
                DC        'RL'
                DC        'RR'
                DC        'SLA'
                DC        'SRA'
                DC        'SLL'
                DC        'SRL'

; Map for 00xxx111 opcode
_map00xxx111:  DB        7
               DC        'RLCA'
               DC        'RRCA'
               DC        'RLA'
               DC        'RRA'
               DC        'DAA'
               DC        'CPL'
               DC        'SCF'
               DC        'CCF'

_map000xx010:  DB        3
               DB        'LD',TAB,'(BC),',('A'+80h)
               DB        'LD',TAB,'A,(BC',(')'+80h)
               DB        'LD',TAB,'(DE),',('A'+80h)
               DB        'LD',TAB,'A,(DE',(')'+80h)

_map_ed_z7:    DB        7
               DB        'LD',TAB,'I,',('A'+80h)
               DB        'LD',TAB,'R,',('A'+80h)
               DB        'LD',TAB,'A,',('I'+80h)
               DB        'LD',TAB,'A,',('R'+80h)
               DC        'RRD'
               DC        'RLD'
               DC        'NOP'
               DC        'NOP'

_map_blk:      DB         15
               DC         'LDI'
               DC         'CDI'
               DC         'INI'
               DC         'OUTI'
               DC         'LDD'
               DC         'CPD'
               DC         'IND'
               DC         'OUTD'
               DC         'LDIR'
               DC         'CDIR'
               DC         'INIR'
               DC         'OTIR'
               DC         'LDDR'
               DC         'CPDR'
               DC         'INDR'
               DC         'OTDR'

_idx_alu:      DB        3
OP_INC:        DC        'INC'
OP_DEC:        DC        'DEC'
OP_LD:         DC        'LD'

; Simple single byte instructions:
_desc_1:        DB        $00    ; 0 has to be the first entry - otherwise it's a terminator
OP_NOP:         DC        "NOP"
                DB        $08
                DB        "EX",TAB,"AF,AF",("'"+80h)
                DB        $76
                DC        "HALT"
                DB        $D9
                DC        "EXX"
                DB        $E3
                DB        'EX',TAB,'(SP),H',('L'+80h)
                DB        $EB
                DB        'EX',TAB,'DE,H',('L'+80h)
                DB        $F3
                DC        "DI"
                DB        $F9
                DB        'LD',TAB,'SP,H',('L'+80h)
                DB        $FB
                DC        "EI"
                DB        0

_desc_2:        DB        $E1
                DB       "POP",(TAB+80h)
                DB        $E3
                DB       "EX",TAB,"(SP)",(','+80h)
                DB        $E5
                DB       "PUSH",(TAB+80h)
                DB        $F9
                DB       "LD",TAB,"SP",(','+80h)
                DB        0


_toaddr:        DB       "    ; =>",(' '+80h)

; When an instruction is decoded we store information about how that
; instruction controls flow here. The bottom 3 bits of this value are decoded
; as follows:
CT_NONE:   .EQU    000b  ; 0 - Normal instruction, no change of control
CT_REL:    .EQU    001b  ; 1 - Single byte relative jump (eg JR NZ xx)
CT_ABS:    .EQU    010b  ; 2 - Two byte absolute jump (eg JP C xxxx)
CT_RET:    .EQU    011b  ; 3 - Return from subroutine
CT_RST:    .EQU    100b  ; 4 - A RST call
CT_IND:    .EQU    101b  ; 5 - JP (reg) - HL, IX, IY depending on prefix
CT_CALL:   .EQU    110b  ; 6 - A 'call' instruction. Absolute address but step over this is the 'next' command is used.

; Data defintions for this module
           DSEG
DISPTR:    DS      2
DISBUF:    DS     40
PREFLEN:   DS      1
EXT_REG    DS      2
CTRLFLOW:  DS      1
OFFSET     DS      2
