import defs.asm
import config.asm
import zapi.asm

import pcb_def.asm
import zlib.asm
import zios.asm
import zload.asm

  extrn  main
  extrn  E_UNKWN

  public SET_RGS,DO_RGS,SHOW_RGS

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
          CALL  P_MAPX         ; HL is now the physical address, DE application address
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
STK_NXT:    DEFB CR,LF,ESC,"[3C",NULL ; Down one line then to character 2


SAVE_POS:   DEFB ESC,"7",NULL
REST_POS:   DEFB ESC,"8",NULL
HOME:       DEFB ESC,"[H",NULL
