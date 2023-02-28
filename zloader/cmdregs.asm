; **********************************************
; Implements the following status commands:
;
;    R         ; Redraw the register display
;    R reg=val ; Set the register 'reg' to 'val
;
; 'reg' can be any 8 or 16 bit register including
; the alternate registers. Valid named:
;
; A,B,C,D,E,H,L
; A',B',C',D',E',H',L'
; BC,DE,HL
; BC',DE',HL'
; IX,IY
; PC SP
;
; Examples:
;     R PC=1234     ; Change the programme counter
;     R HL'=3456    ; Load HL' register pair
;     R A=22        ; Load A with value
; **********************************************
; Copyright Peter Wilson 2022
; https://github.com/peterw8102/Z80-Retro
; **********************************************
import defs.asm
import config.asm
import zapi.asm

import pcb_def.asm
import zlib.asm
import zios.asm
import zload.asm

  extrn  main
  extrn  E_UNKWN,E_ERROR

  public SET_RGS,DO_RGS,SHOW_RGS

RNAME  EQU   SCRATCH

; Return NC if the character is allowed in a name. All letters and apostrophe
_isnchr:  CP    27h     ; single quote (apostrophe) is OK
          RET   Z
          CP    'A'
          RET   C       ; Lower than an 'A'
          CP    'Z'+1
          CCF
          RET

; Get register name into the RNAME buffer (null perminated. Any length but
; may only contain valid characters.
_getname: PUSH  BC
          LD    C,0       ; Number of characters
          LD    HL,RNAME
_nchr:    CALL  BUFCHUP
          CALL  _isnchr
          JR    C,_badchr
          LD    (HL),A
          INC   HL
          INC   C
          JR    _nchr
_badchr:  LD    (HL),0     ; Null terminator
          LD    C,A
          OR    A          ; Return with Z set if no valid characters
          CALL  UNGET
          POP   BC
          RET


; ------------------- SET_RGS
; CMD: Set register value. R reg=val
; reg is A,B,C,D,E,H,L,BC,DE,HL,IX,IY
; val is an 8 or 16 bit hex value
SET_RGS:  CALL  _getname
          JR    Z,E_UNKWN

          ; Next character should be '='
          CALL  SKIPSPC
          CP    '='
          JR    NZ,E_ERROR

          ; Then the walue
          CALL  WASTESPC
          CALL  GET_HEX
          JR    Z,E_ERROR

          ; Got everything. Find the target.
          CALL  _reg_addr
          JR    C,E_UNKWN

          ; DE is target address, A is set/clear depending on the
          ; size of the target (8/16 bit).
          ; Write first byte of value to memory
          EX    DE,HL
          LD    (HL),E
          OR    A            ; 16 bit operation?
          JR    Z,_only8
          INC   HL
          LD    (HL),D
_only8:   JR    SHOW_RGS

; -------------- reg_addr
; INPUT:  RNAME  contains a null terminated reg name from the user
; SAVED:  HL
; OUTPUT: DE     The address of the register
;         A      0 if it's 8bit or 1 if 16 bit
;         C:     Set on unknown register name
_reg_addr:  PUSH HL
            LD   HL,R_ADDR
_nxtreg:    LD   DE,RNAME
_nxtchr:    LD   A,(DE)
            OR   A
            JR   Z,_miss
            LD   C,A      ; Char from user entered name
            LD   A,(HL)   ; From the test table
            LD   B,A
            AND  $7f      ; Ignore MSB
            CP   C
            JR   NZ,_miss
            INC  HL
            INC  DE
            RLC  B        ; Was the MSB set?
            JR   NC,_nxtchr

            ; For a complete match (DE) must now be zero
            LD   A,(DE)
            OR   A
            JR   NZ,_miss

            ; Found a match. Next byte is flags then the one after is the address
            LD   A,(HL)   ; Flag
            INC  HL
            LD   E,(HL)   ; Address
            INC  HL
            LD   D,(HL)
            ; EX   DE,HL
            OR   A         ; Clear carry flag because we matched
            POP  HL
            RET

_miss:      LD   A,$80     ; Look for character with MSB set
            AND  (HL)
            INC  HL
            JR   Z,_miss
            INC  HL        ; End of token. Step over flags and address
            INC  HL
            INC  HL
            LD   A,(HL)
            OR   A         ; If zero then end of table
            JR   NZ,_nxtreg

            ; No match. Return with carry set
            SCF
            POP  HL
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
          LD    E,L            ; HL is the application address
          LD    D,H            ; Save application address in DE
          CALL  P_MAPX         ; HL is now the physical address, DE application address

          ; Adjust BOTH application and mapped addresses
          PUSH  DE             ; Adjust mapped address
          LD    DE,15
          ADD   HL,DE
          POP   DE
          PUSH  HL             ; Adjust application space address
          LD    HL,15
          ADD   HL,DE
          LD    E,L
          LD    D,H
          POP   HL
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

REGDEF MACRO name,flg,addr
    DC     name
    DEFB   flg
    DEFW   addr
    ENDM


R_ADDR:   REGDEF "BC'",  1,   R_BC_P   ; Has to be arranged as longest name first
          REGDEF "DE'",  1,   R_DE_P
          REGDEF "HL'",  1,   R_HL_P
          REGDEF "BC",   1,   R_BC
          REGDEF "DE",   1,   R_DE
          REGDEF "HL",   1,   R_HL
          REGDEF "IX",   1,   R_IX
          REGDEF "IY",   1,   R_IY
          REGDEF "PC",   1,   R_PC
          REGDEF "SP",   1,   R_SP
          REGDEF "A'",   0,   R_AF_P+1
          REGDEF "A",    0,   R_AF+1
          REGDEF "B'",   0,   R_BC_P+1
          REGDEF "B",    0,   R_BC+1
          REGDEF "C'",   0,   R_BC_P
          REGDEF "C",    0,   R_BC
          REGDEF "D'",   0,   R_DE_P+1
          REGDEF "D",    0,   R_DE+1
          REGDEF "E'",   0,   R_DE_P
          REGDEF "E",    0,   R_DE
          REGDEF "H'",   0,   R_HL_P+1
          REGDEF "H",    0,   R_HL+1
          REGDEF "L'",   0,   R_HL_P
          REGDEF "L",    0,   R_HL
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
STK_NXT:    DEFB CR,LF,ESC,"[3C",NULL ; Down one line then to character 2


SAVE_POS:   DEFB ESC,"7",NULL
REST_POS:   DEFB ESC,"8",NULL
HOME:       DEFB ESC,"[H",NULL
