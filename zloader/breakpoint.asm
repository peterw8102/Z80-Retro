; **********************************************
; Manage breakpoints.
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


  ; Disassemmber
  extrn  DISASS, SETOFF

  ; From dump
  extrn  DECINST

  ; From regs
  extrn  DO_RGS

  ; From core
  extrn  E_BADPS,E_PRTERR
  extrn  main,MORE

  extrn  DO_GO,NEXTSTP

  public DO_BP,BRK_HDLR
  public STP_HOW,STP_CNT,SSTEP_BP,INSTBP,AUTO_STP,FINDBP,SETBP,CLRBP,BPOINTS


; Number of breakpoint locations (in code)
NUM_BK    EQU    64
BP_SIZE   EQU     4


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
          CALL  P_MAPX      ; Convert app space to physical space
          LD    D,H
          LD    E,L         ; DE now points into the first physical address
          CALL  DISASS      ; Only disassemble to get control flow information
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
          CALL  P_MAPX      ; Convert app space to physical space
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
          CALL  P_MAPX      ; Convert app space to physical space
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
; The application stack (RP_SP) in application stack will have AF at the top
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

          ; ISR was in ZIOS space so real HL and AF is on the stack. Simples.
          POP   HL
          POP   AF

          ; Next value on the stack is the PC
          EX    (SP),HL
          LD    (R_PC),HL
          EX    (SP),HL
          JR    _cnt_bp

          ; Application was running. Will have supervisor stack but data is on app stack.
_appbrk:  PUSH   DE

          LD     HL,(R_SP)      ; Application stack pointer, AF is at the top of that stack
          CALL   P_MAPX         ; Map stack pages into memory - txlated address in HL
          LD     E,(HL)
          INC    HL
          LD     D,(HL)         ; DE contains what was in AF
          INC    HL
          ; Next 2 bytes are the return address. This will get decremented in the BP handler
          ; because it thinks it's been called via a single byte RST call. To get around this
          ; increment the address on the stack.
          PUSH   DE             ; Original AF
          POP    AF             ; Get back into the AF registers

          LD     HL,(R_SP)      ; Remove the AF value just retrieved from the stack
          INC    HL
          INC    HL             ; HL now has the old value of the stack pointer
          LD     (R_SP),HL

          ; Everything now is back where we need it to be so can do standard break point processing
          POP    DE
          POP    HL

          JR     _cnt_bp

; ------------------- DO_BP
; This is the target of the BP handler. We're operating in supervisor mode. The following
; registers have been saved in shadow memory and need to be copied to our OS copy:
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
          CALL  P_MAPX         ; Map into sys space, stack memory now accessible in HL
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
          JR    NEXTSTP       ; Go again

_bfdo:    EX    DE,HL         ; Put the current address into HL so we can display it
          XOR   A             ; Use application space memory
          CALL  DECINST       ; Display the next instruction
          CALL  DO_RGS
          LD    HL,(STP_CNT)  ; Any outstanding step count?
          DEC   HL
          LD    A,L
          OR    H
          JR    Z,bpmore
          LD    (STP_CNT),HL

          ; Step again
          LD    A,(STP_IN)
          CALL  SSTEP_BP    ; Set single step BP then go
          JR    DO_GO

bpmore:   LD    HL,_MORESTP
          JR    MORE

_MORESTP  DEFW  .morest1
          DEFW  STP_HOW
          DEFW  STP_HOW

.morest1  DEFB  "% ",0
STP_HOW:  DEFB  "S",0


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
          CALL  P_MAPX          ; A: page number, HL: mapped address
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
          JR    E_PRTERR
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
          CALL  P_MAPX           ; A: page number, HL: mapped address
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
          CALL  P_MAPX                  ; ...Convert and map...
          LD    A,(HL)                  ; BP already installed?
          CP    BRK_OPCODE
          JR    Z,_iskip
          LD    (DE),A                  ; Store the byte we're replacing with the BP opcode
          LD    (HL),BRK_OPCODE         ; Installed. Don't need to BP address any longer
_iskip:   POP   HL
          RET
_insrm:   EX    DE,HL                   ; Needs to be removed so...
          CALL  P_MAPX                  ; ...Convert and map...
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
          CALL  P_MAPX      ; Convert app space to physical space
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

_BRK         DEFB "BREAK...", NULL
_FULL:       DEFB "Full",NULL

          DSEG
; Single Step Count...
STP_CNT    DEFS    2      ; Up to 64K steps!!!
STP_IN:    DEFS    1      ; Temp store for setting a BP. True means step into and CALL ops.

; If 'AUTO_RUN' is non-zero then the BP handler will automatically execute a 'GO', setting new BPs
AUTO_STP   DEFS    1
BPCT       DEFS    1      ; Value passed into INSTBP
CTRLCH:    DEFS    1

; Breakpoints. Each entry is 4 bytes:
; Type|AddrX2|Opcode
; A breakpint in placed in memory by replacing an instruction with a single byte RST 20h (or replacement) opcode
; Type: 0 - free slot - can be used to store a new breakpoint
;       1 - single shot - removed once hit
;       2 - permanent breakpoint (WIP)
BPOINTS    DEFS    NUM_BK*BP_SIZE
