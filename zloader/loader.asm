import defs.asm
import config.asm
import zapi.asm

import pcb_def.asm
import zlib.asm
import zios.asm
import zload.asm

          ; Utilities
          extrn  DISASS, SETOFF

          ; External commands for the command table.
          extrn  FILL,CLRSCR,INPUT,OUTPUT,MODIFY,SDMOD,HELP
          extrn  MORE

          extrn  DNVRAM,DECINST,DMP16,DUMPM,DUMPI,DUMP

          ; Load commands
          extrn  LDF,LDH,LDT,LOADF,BOOT,BOOTIX

          ; Date time commands
          extrn  DTIME,SHDTIME

          ; Command table
          extrn  BDG_TABLE,CMD_TABLE

          ; SDCard definitions
          extrn  BTPREP,SBCALCS
          extrn  SADDR

          ; Z80 register management
          extrn  DO_RGS,SHOW_RGS

          public main,BADPS,OPMODE

          public SDPAGE
          public PRTERR
          public END_RES

          public START
          public COLSTR
          public AUTO_RUN

          ; Error messages
          public E_NOTF,E_ERROR,E_UNKWN,E_NEMPT,E_NOSD

          ; Command handlers
          public BP,CLS,CONFIG,DECCHR,EXDBG,GO,MAPDSK,NSTEP,PAGE,RUN
          ; public SDDIR,SDLOAD,SDRUN,SDUMP
          public SHWHIST,SMAP,SSTEP,SWRITE

; OP Code to use in RST vectors
VEC_CODE  EQU   $C3

; Number of breakpoint locations (in code)
NUM_BK    EQU    64
BP_SIZE   EQU     4

          ASEG
          ORG    0
          DI
          JR     START
          JR     main       ; Warm start for the loader.

          ORG    $08
          JR     TXA

          ORG    $0b
          JR     DO_BP      ; Entry point for breakpoints

          ORG    $10
          JR     RXA

          ORG    $13
          JR     BRK_HDLR   ; Entry point for breakpoints

          ORG    $18
          JR     CKINCHAR

END_RES   EQU     $

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

           ; ------ REMOVE THIS SETUP BEFORE BURNING TO FLASH ------
if IS_DEVEL
            BANK   3,MN2_PG       ; Map second supervisor page into bank 3
            JR    RUN_CLI      ; THIS LINE SHOULD BE IN FOR DEVELOPMENT BUT OUT BEFORE PROGRAMMING TO FLASH
endif
            ; Copy the first 32KB of Flash to the first 32K of RAM.
            BANK  0,FSH_PG_0   ; Flash page 1 -> bank 1
            BANK  1,FSH_PG_0+1 ; Flash page 1 -> bank 1
            BANK  2,MN_PG      ; RAM page 0 into bank 2
            BANK  3,MN2_PG     ; RAM page 1 into bank 3
            EN_PAGE

            LD    HL,0         ; Copy Flash page 0 to RAM page 0
            LD    DE,$8000
            LD    BC,$8000
            LDIR

RUN_CLI:    ; We're running in supervisor mode
            BANK  0,MN_PG      ; RAM page 0 into bank 2
            BANK  3,MN2_PG     ; RAM page 1 into bank 3

            ; Give ourselves a stack at the end of our reserved page.
            LD    SP,SP_STK

            CALL  ZIOS_INI
            LD    (HW_SWTCH),A       ; State of the hardware config switches

            ; Set mode
            LD    A,1
            LD    (OPMODE),A      ; Command mode (don't start in debug)
            LD    HL,CMD_TABLE
            LD    (CMDTAB),HL     ; Search table for commands

            ; Set default DUMP mode to disassemble memory
            ; LD     A,'I'
            ; LD     (DUMP_MODE),A

if CSET
            ; Install character set
            CALL   INITCSET
endif

            ; Really simple CLI now. Display
NOSIO:      LD    HL, _INTRO
            CALL  PRINT_LN
            CALL  SH_HW

if !IS_DEVEL
          ; Check the state of the DIL switches and look for autoboot mode
          LD    A,(HW_SWTCH)       ; State of the hardware config switches
          AND   03h             ; Only interested in bits 0 and 1.

          DEC   A               ; If 01 then do Pi boot
          JR    Z,BOOT
          DEC   A               ; If 02 then do SDCard boot
          JR    NZ,main         ; If not 1 or 2 then just run the command line.

          ; SDCard Boot
          LD    A,1
          LD    (AUTO_RUN),A    ; Set auto-run mode
          CALL  BTPREP
          JR    Z,SDLDDEF       ; Checksum OK so go load default image.
endif
          ; In development mode don't use the DIL switches. This allows us to develop the loader more efficiently.
          JR    main

E_NOSD:   LD    HL,_NOSDADD
          JR    _prterr
E_NEMPT:  LD    HL,_NOTEMP
          JR    _prterr
E_ERROR:  LD    HL,_ERROR
          JR    _prterr
E_UNKWN:  LD    HL,_UNKWN
          JR    _prterr

E_NOTF:   ; File doesn't exist so error
          LD    HL,_NOTF
          JR    _prterr

BADPS:    LD    HL,_IOERR
PRTERR:
_prterr:  CALL  PRINT_LN

main:     LD    SP,SP_STK       ; Dump the stack and restart on *main*
          LD    HL, _PROMPT
          CALL  PRINT
          CALL  GET_LINE
          CALL  NL

          CALL  WASTESPC
          JR    Z, main

; Process an unbroken chain of characters looking for a command match
EXEC_LN::
FNDCMD:   LD    HL,(CMDTAB)
_nxtchr:  CALL  BUFCHR
          JR    Z,_miss
          CALL  TOUPPER        ; Need uppercase only
          LD    C,A            ; Save the input character
          LD    A,(HL)

          LD    B,A            ; Save the test character$
          AND   $7f            ; Ignore MSB

          CP    C              ; Compare with input character
          JR    NZ,_miss

          ; Input character matched test character. End of command if MSB of B is set.
          INC   HL
          RLC   B
          JR    NC,_nxtchr     ; Step to next command

          ; Next two bytes are the target address.
          LD    E,(HL)
          INC   HL
          LD    D,(HL)
          EX    DE,HL
          CALL  WASTESPC       ; Set buffer ready for next token
          JP    (HL)           ; Call with A being the next character

          ; Command mismatch. Step to next command, reset input buffer and try again.
_miss:    LD    A,$80
          AND   (HL)
          INC   HL
          JR    Z,_miss
          ; Step over the two address bytes
          INC   HL
          INC   HL
          CALL  RESINP
          CALL  WASTESPC
          LD    A,(HL)
          OR    A
          JR    Z,E_UNKWN
          JR    _nxtchr          ; Check against the next command...

; ----- Single Step
SSTEP:    LD    A,'S'
          LD    (STP_HOW),A
          CALL  GET_DEC        ; Get optional step count
          JR    C,_stp1
          INC   HL
_stp1:    LD    (STP_CNT),HL   ; By default step once
          LD    HL,0
          LD    E,1            ; A -> !0
          CALL  SSTEP_BP       ; Set single step BP then go
          JR    DO_GO

; ------------------- go
GO:       CALL  WASTESPC
          JR    Z,_noadd

          ; Is there an address?
          CALL  GET_HEX        ; Will be zero if there was no value specified
          JR    Z,_noadd       ; Don't upset the stored PC if nothing entered (go from PC address)

          ; Have an address - put it in the virtual PC
          LD    (R_PC),HL

_noadd:   LD    HL,1
          LD    (STP_CNT),HL ; Make sure we don't keep running when we hit a breakpoint!!
          LD    E,2
          XOR   A            ; Want breakpoints added

          ; DE contains WHAT?????
DO_GO:    PUSH  DE
          CALL  INSTBP       ; Install all permanent breakpoints
          POP   DE
          LD    A,E
          CP    2
          JR    NZ,_godo
          LD    A,(AUTO_STP)
          OR    A
          JR    NZ,SSTEP     ; Transmute into a single step

_godo:    ; Install the drivers.
          JP    PR_REST

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
          JR    GO         ; Go again

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



;
; ------------------- BP: Set breakpoint
; Get address from command line. If not specfied then BP at the current PC value
BP:       CALL  WASTESPC
          JR    Z,_LISTBP

          LD    E,1
          CP    '-'              ; Removing a breakpoint
          JR    NZ,_gadd
          CALL  BUFCHR           ; Waste the '-'
          DEC   E
_gadd:    CALL  GET_HEX          ; Address for breakpoint
          JR    Z, BADPS

          ; Have an address. Set or clear a BP
          DEC   E

          JR    Z,_doadbp

          ; Remove BP at address specified
          EX    DE,HL
          CALL  FINDBP

          JR    Z,_LISTBP
          LD    (HL),0           ; Cleared
          JR    _LISTBP

          ; Set a BP at the address in HL
_doadbp:  EX    DE,HL
          CALL  FINDBP           ; Already have a BP at this location?
          JR    NZ,_LISTBP       ; Duplicate so list (no change)
          EX    DE,HL
          LD    A,2              ; BP type
          CALL  SETBP

          ; Drop through and display set breakpoints
_LISTBP:  LD    HL,BPOINTS
          LD    B,NUM_BK
_lnxt:    PUSH  HL
          LD    A,(HL)
          OR    A
          JR    Z,_lemp
          EX    DE,HL
          DEC   A
          JR    NZ,_lprm
          LD    HL,_TMPBC
          JR    _ladd
_lprm:    LD    HL,_PRMBC
_ladd:    CALL  PRINT
          EX    DE,HL
          INC   HL
          LD    E,(HL)
          INC   HL
          LD    D,(HL)
          EX    DE,HL
          CALL  WRITE_16
          CALL  NL
_lemp:    POP   HL
          INC   HL
          INC   HL
          INC   HL
          INC   HL
          DJNZ  _lnxt
          JR    main

; ------------------- RUN
; This initial version loads the first four RAM pages into the four
; banks, reserving the last 16 bytes of page 03 to store an address
; switcher. Once switch it jumps to address 0 to run that code.
RUN:      DI
          CALL  WASTESPC
          LD    HL,0
          JR    Z,PR_RUN
          CALL  GET_HEX    ; Get optional execution address into HL
          JR    PR_RUN



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
          JR    _prterr
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

; ----- Step Over
NSTEP:    LD    A,'N'
          LD    (STP_HOW),A
          LD    HL,1
          LD    (STP_CNT),HL   ; By default step once
          LD    E,0          ; In E
          CALL  SSTEP_BP
          JR    DO_GO

; ----- SHWHIST
; Display history with index rows
SHWHIST:  LD    B,$11
_nhist:   DEC   B
          CALL  GETHIST
          JR    Z,_gnxt      ; Nothing in that slot
          ; Got a history line, pointer in HL.
          CALL  PRINT_LN
_gnxt:    LD    A,B
          OR    A
          JR    NZ,_nhist
          JR    main


; ----- CLS
; Clear screen. If in debug mode then redraw registers etc.
CLS:      LD    HL,_CLRSCR
          CALL  PRINT
          LD    A,(OPMODE)
          RRCA
          JR    C,main      ; Standard mode so nothing more to do
          JR    _fcdb

; ----- EXDBG
; Switch between command and debug modes
EXDBG:    LD    A,(OPMODE)
          LD    C,A
          RRCA
          LD    A,C
          JR    NC,_std1     ; In debug mode so can always switch to standard

          ; Check whether debugging is allowed. Requires our drivers.
if IS_DEVEL
          LD     A,1         ; For driver load for debug version
else
          LD     A,(NVRAM)
endif
          RRCA
          LD    A,C
          JR    C,_std1
          LD    HL,_NODRV
          JR    _prterr

_std1:    XOR   3
          LD    (OPMODE),A   ; Toggled mode
          LD    HL,CMD_TABLE
          LD    DE,_INTRO
          RRCA
          JR    C,_std
_fcdb:    LD    DE,_DBMD
          LD    HL,BDG_TABLE
_std:     LD    (CMDTAB),HL
          EX    DE,HL
          PUSH  AF
          CALL  PRINT_LN
          POP   AF
          JR    NC,SHOW_RGS
          JR    main




; ------------ PAGE
; Page operations. Syntax:
;   PM - Display or change page map
;   PC - Copy one pge to another
PAGE:     ; Display current application pages or change a page number
          ; Changing is in the form blknum=pagenum. Eg 3=21
          CALL  SKIPSPC
          JR    Z,_shpg
          SUB   '0'
          JR    C,_shpg
          CP    4
          JR    NC,E_ERROR
          ; Calculate PAGE_MP position
          LD    HL,PAGE_MP
          ADD   L
          LD    L,A
          CALL  SKIPSPC
          CP    '='
          JR    NZ,_shpg
          CALL  INHEX_2
          JR    C,_shpg

          LD    (HL),A
          ; Drop through and display current settings.

_shpg:    LD    HL, _APPPG
          CALL  PRINT
          LD    HL,PAGE_MP
          LD    B,4
_npd:     LD    A,(HL)
          CALL  WRITE_8
          WRITE_CHR ' '
          INC   HL
          DJNZ  _npd
          CALL  NL
          JR    main

; ------------ CONFIG
; Show or set a configuration options. All are boolean at the moment. Currently
; only using the first byte as 8 boolean flags.
; Bit 0:  Default for 'BOOT' - DON'T install page 0 drivers (image is stand alone)
CONFIG:   CALL   WASTESPC
          JR     Z,_cfshw

          ; Expect a hex(ish) number for the parameter ID
          CALL   GET_HEX
          JR     Z,BADPS
          LD     A,L
          LD     HL,CFG_TAB
          JR     C,_cfshw

          ; Find the parameter ID
          OR     A
          JR     Z,_cfset
          LD     B,A             ; B = config param ID
_cfgsn:   LD     A,(HL)          ; Bit mask
          OR     A
          JR     Z,BADPS         ; If zero then reached end of table
_cfcnxt:  INC    HL
          INC    HL
          CALL   _skpstr
          DJNZ   _cfgsn
_cfset:   LD     A,(HL)
          OR     A
          JR     Z,BADPS
          LD     C,A             ; Save mask for later

          INC    HL
          LD     B,(HL)          ; Byte offset
          INC    HL              ; Points to description/name
          EX     DE,HL           ; Save description for later in DE

          ; Calculate the address of the config byte into HL
          LD     HL,NVRAM
          LD     A,B
          CALL   ADD8T16

          LD     A,C             ; The mask

          CPL                    ; Invert
          AND    (HL)            ; Current value with flag cleared
          LD     B,A             ; Save the cleared config parameter

          ; And decide whether we need to set it.
          ; Get the new setting from the user
          CALL   SKIPSPC
          CP     '='
          JR     NZ,BADPS
          CALL   SKIPSPC

          CP     'N'
          JR     Z,_cfsv           ; Save the cleared value
          CP     '0'
          JR     Z,_cfsv           ; Save the cleared value
          ; Any other value and we have to set the flag.
          LD     A,C               ; The original mask
          OR     B                 ; Set the bit
          LD     B,A
_cfsv:    LD     A,B
          LD     (HL),B

          ; Print description
          AND    C                 ; test the flag just configured
          EX     DE,HL
          CALL   _say
          CALL   NVSAV
          JR     main

_cfshw:   LD     HL,CFG_TAB
          LD     B,0             ; Config index
_cfnc:    LD     A,(HL)
          OR     A
          JR     Z,main          ; End of table

          LD     C,A             ; Save mask for later
          ; Output the config parameter number (used for setting)
          LD     A,B
          CALL   WRITE_8
          WRITE_CHR ' '

          ; A contains the config bit mask
          INC    HL
          LD     A,(HL)          ; The byte offset
          EX     DE,HL
          LD     HL,NVRAM
          CALL   ADD8T16
          LD     A,(HL)          ; The byte containing the flag
          EX     DE,HL           ; HL back to the table
          INC    HL              ; points to the description
          AND    C               ; Mask to the relevant bit
          PUSH   HL
          CALL   _say
          POP    HL
          ; Then step HL past the description to the next config entry
          CALL   _skpstr

          ; Step one more to the start of the next config entry
          INC    B               ; Next command ID
          JR     _cfnc

; ---- _say
; Display a string in the form:     NAME = YN
; Where 'NAME' is the string pointed to by HL
;       'YN' is the 'YES' or 'NO' depending on the Z flag
; Z flag is the state of the flag. HL points to the name
;
; Usse to format the configuration settings.
_say:     PUSH  AF
          PUSH  HL
          JR    NZ,_sayyes
          LD    HL,_no
          JR    _saynow
_sayyes:  LD    HL,_yes
_saynow:  EX    (SP),HL
          CALL  PRINT
          LD    HL,COLSTR     ; Tab alignment
          CALL  PRINT
          POP   HL
          CALL  PRINT         ; The yes/no string
          CALL  NL
          POP   AF
          RET

; --------- _skpstr
; Step over a null terminated string and return HL pointing to the first byte after the null
_skpstr:  XOR    A
_cfnch:   INC    HL
          CP     (HL)
          JR     NZ,_cfnch
          INC    HL          ; Step past null
          RET

; ------------ _PGCALC
; Take a 16 bit address in application space and translate that into a block number (0-3)
; and a 16 offset. Arrange for the 16 bit offset to point into a block one address. This
; then allows us to map the application space page into block one regardless of where it
; is in the application space.
; HL - load address. Translate into an offset  and return the offset in HL
; Return:
; A  - The block number (0-3) in application space
; HL - The adjusted offset into that page, mapped as though in block 1.
; DE - WORKING SPACE, NOT SAVED!!
_PGCALC:  LD    A,H
          RLCA
          RLCA
          AND   03h        ; A now contains the block number.
          LD    D,A        ; Save to return

          ; And map the address in HL so it's in BLOCK 1.
          LD    A,3Fh
          AND   H

          ; Set bit 6 so it's in block 1
          OR    A,40h

          ; And move back to the address
          LD    H,A
          LD    A,D        ; Return the page we selected in A and D
          RET

ifdef EXCLUDE
; ------------ PGADJ
; Take a 16 bit address in application space and translate that into a page number and an offset.
; HL - load address. Translate in a page and offset then load that page into board page 1 and
;      return the offset in HL
; Return:
; A  - The actual physical page number to be mapped
; HL - The adjusted offset into that page
; DE - WORKING SPACE, NOT SAVED!!
PGADJ:    CALL  _PGCALC

          ; A is the logical block number in application space. Translate that in a RAM
          ; page number.
          LD    DE,PAGE_MP
          ADD   E
          LD    E,A      ; Monitor memory is linked from 2000h so won't cross a 256 byte boundary.
          LD    A,(DE)   ; DE is now the address of the PAGE_MP for the addressed page in application space
          RET


; ------------- PGRESTX
; Restore page 1 and 2 to the application space. Used after extended instructions that have to map
; two pages to deal with page boundarys.
PGRESTX:  BANK  2,(PAGE_MP+2)

; ------------- PGREST
; Restore page 1 to the application space.
PGREST:   BANK  1,(PAGE_MP+1)
          RET

; HL - load address. Translate in a page and offset then load that page into board page 1 and 2
;      return the offset in HL
; A returns the page mapped.
PGMAP:    PUSH  DE
          CALL  PGADJ    ; Returns the target page number in A
          BANK  1        ; which we map to bank 1
          POP   DE
          RET

; ---- PGMAPX
; Same as PGMAP but also places the *next* application space page into bank 2. Use this if
; operations could cross a 16K page boundary.
; INPUT:   HL - address in application space
; OUTPUT:  HL - mapped address to use within supervisor map
PGMAPX:   PUSH  DE
          CALL  _PGCALC         ; HL: Adjusted address, A the 16K application block number to map (0-3)
          PUSH  AF
          LD    DE,PAGE_MP
          ADD   E
          LD    E,A      ; Monitor memory is linked from 2000h so won't cross a 256 byte boundary.

          LD    A,(DE)   ; DE is now the address of the PAGE_MP for the addressed page in application space
          BANK  1

          INC   DE
          LD    A,(DE)
          BANK  2

          POP   AF        ; AF includes the logical bock number (0-3)
          POP   DE
          RET

endif



; ------- MAPDSK
; Display current mapped drives
MAPDSK:  LD    HL,SCRATCH
         LD    BC,100h | A_QDSKMP    ; B must be zero
         RST   30h            ; Get the current drive map
         LD    BC,1000h
         LD    A,'A'
         LD    HL,SCRATCH     ; Where the data went
_dmapn:  PUSH  AF
         RST   08h
         LD    A,':'
         RST   08h
         LD    A,' '
         RST   08h

         LD    A,(HL)         ; Device
         INC   HL
         PUSH  AF
         LD    E,(HL)
         INC   HL
         LD    D,(HL)
         INC   HL
         EX    DE,HL
         CALL  WRITE_D
         EX    DE,HL
         POP   AF
         CALL  _wrdev         ; Write SDCard name (from A)
         CALL  NL
         POP   AF
         INC   A

         DJNZ  _dmapn

         JR    main


_wrdev:  PUSH   HL
         PUSH   AF
         LD     A,'/'
         RST    08h
         LD     HL,_SDEV
         CALL   PRINT
         POP    AF
         ADD    A,'0'
         RST    08h
         POP    HL
         RET


; ----- SMAP
; Map a logical dic
; SM L PPPP - Map Physical drive (decimal 0-511) to logical drive letter L
SMAP:     JR    Z,MAPDSK
          CALL  BUFCHUP
          CP    'A'
          JR    C,_baddrv
          CP    'A'+16
          JR    C,_gotdrv

          ; Report bad drive letter
_baddrv:  LD    HL,_NODRIVE
          JR     _prterr

          ; Want a physical drive (0-1023 decimal)
_gotdrv:  PUSH   AF          ; Push DRIVE letter
          CALL   GET_DEC
          JR     Z,E_ERROR

          ; Number from 1-1023 (Don't allow 0 as a target)
          LD     A,$FC
          AND    H
          JR     NZ,E_NOSD
          LD     A,H
          OR     L
          JR     Z,E_NOSD

          ; Optional SDCard
          CALL   BUFCHR
          CP     ':'
          LD     A,'0'
          JR     NZ,_gotsd

          ; Must be a '0' or '1'
          CALL   BUFCHR
          JR     Z,E_ERROR
          CP     '0'
          JR     Z,_gotsd
_not1:    CP     '1'
          JR     NZ,E_ERROR


          ; A has the SDCard (ASCII) number, which needs to be in E for the API call
_gotsd:   LD     E,A        ; SDCard (into E) as a letter '0' or '1'
          POP    AF         ; Drive letter...
          PUSH   AF
          PUSH   HL         ; disk number
          PUSH   AF
          PUSH   HL         ; disk number
          LD     HL,_MAPD
          CALL   PRINT
          POP    HL
          CALL   WRITE_D
          LD     A,'/'
          RST    08h
          LD     HL,_SDEV    ; 'sd'
          CALL   PRINT
          LD     A,E         ; SDCard
          RST    08H
          LD     A,E
          SUB    '0'
          LD     E,A
          LD     HL,_MAPD_TO
          CALL   PRINT
          POP    AF         ; drive
          RST    08H
          CALL   NL
          POP    HL
          POP    AF         ; drive

          ; Tell the API the mapping we want.
          SUB    'A'
          LD     D,A             ; Drive slot
          LD     B,0             ; Standard mapping
          LD     C,A_DSKMP       ; Map drive

          ; D:  Drive slot (0-15)
          ; E:  Physical SDCard (0 or 1)
          ; HL: Virtual disk on the SDCard
          RST    30h

          JR     main

; -------- SWRITE
; Write the current SDPAGE data back to the SDCard. If no address is specified then write
; to the address prevously loaded.
SWRITE:  CALL  WASTESPC

         CALL  SADDR
         JR    C,E_NOSD

         ; Special case if this is raw write to address 0:0 on disk 0. In this
         ; case patch up the reserved page checksum.
         LD    A,C
         CP    A_DSKRW
         JR    NZ,_nocs
         XOR   A
         OR    E   ; Checksum if the raw read from address zero (start of SDCard)
         OR    D
         OR    L
         OR    H
         JR    NZ,_nocs

         PUSH  HL
         PUSH  DE
         ; Raw read so if all bytes are
         CALL  SBCALCS

         ; Write checksum into page buffer
         LD    A,$FF
         LD    (SDPAGE+480),A
         LD    (SDPAGE+481),HL
         POP   DE
         POP   HL

         ; ---------- DEBUG DUMP
         ; 'C' is the read command. Need the write command.
_nocs:   LD    A,A_DSKWR-A_DSKRD
         ADD   C
         LD    C,A
         RST   30h
         JR    main
         ; ---------- DEBUG DUMP



; ---- _disres
; Display a title string then a value string. The value string is determined by the
; contents of the Z flag. Z: 'yes', NZ: 'no'
;   HL:    Message string
;    Z:    Test value
_disres:    PUSH     AF
            LD       A,H
            OR       L
            JR       Z,_nomsg
            CALL     PRINT
_nomsg:     LD       HL,COLSTR
            CALL     PRINT
            POP      AF
            LD       HL,_yes
            JR       NZ,_isyes
            LD       HL,_no
_isyes:     JR       PRINT_LN

; ---- SH_SDC
; Display status of SDCard 1 and 2
SH_SDC:     LD       HL,_SDIS
            CALL     PRINT
            LD       A,'1'
            RST      08h
            LD       A,1
            AND      E
            LD       HL,0
            CALL     _disres
            LD       HL,_SDIS
            CALL     PRINT
            LD       A,'2'
            RST      08h
            LD       A,2
            AND      E
            JR       _disres

; ---- SH_VDU
; Show the status of the VDU card (installed or not)
SH_VDU:     LD       HL,_VDIS
            LD       A,8
            AND      E
            JR       _disres

; ---- SH_PIO
SH_PIO:     LD       HL,_PDIS
            LD       A,4
            AND      E
            JR       _disres

; ---- SH_SW
; Display the current value of the configuration DIP switch.
SH_SW:      LD       HL,_CFGSW
            CALL     PRINT
            LD       HL,COLSTR
            CALL     PRINT
            LD       A,D
            CALL     WRITE_8
            CALL     NL
            RET


SH_HW:      CALL  SHDTIME
            CALL  NL
            ; Grab the inventory
            LD    C,A_HWINV
            RST   30h

            ; Inventory in DE. Display the results
            CALL  SH_SW
            CALL  SH_SDC
            CALL  SH_VDU
            CALL  SH_PIO
            CALL  NL
            RET


; Only need this code if we're configured to load a user defined default character set to
; the graphics card. If there is no graphic card then the monitor image can be smaller.
if CSET
; ----------- INITCSET
; Initialise the VGA character set
INITCSET:   PUSH     AF
            PUSH     BC
            PUSH     DE
            PUSH     HL

            BANK     2,$FF     ; Character set in flash
            BANK     1,CSET_PG ; Where to get the character set data

            LD       BC,$1000  ; The character set is 4K
            LD       HL,$4000  ; Source address
            LD       DE,$8000  ; Desitnation address
            LDIR

            ; Set up test data in video memory
            LD       HL,$A000  ; Start of display memory
            XOR      A         ; Character to write
            LD       B,$10     ; Outer loop

_c_cs1:     LD       C,0
_c_cs2:     LD       (HL),A
            INC      A
            INC      HL
            DEC      C
            JR       NZ,_c_cs2
            DJNZ     _c_cs1

            POP      HL
            POP      DE
            POP      BC
            POP      AF
            RET
endif

DECCHR:     RST      10h
            CP       3
            JR       Z,main
            CALL     WRITE_8
            JR       DECCHR




; --------------------- STRINGS
_INTRO:   DEFB "Z80 ZIOS 1.18.10",NULL
; _INTRO:   DEFB ESC,"[2J",ESC,"[H",ESC,"[J",ESC,"[1;50rZ80 ZIOS 1.18.8",NULL
_CLRSCR:  DEFB ESC,"[2J",ESC,"[1;50r",NULL

; Set scroll area for debug
_DBMD:    DEFB ESC,"[2J",ESC,"[1m",ESC,"[1;6HSTACK",ESC,"[m",ESC,"[11;50r",ESC,"[9;1H  >",ESC,"[12,1H",NULL

_PROMPT:  DEFB "> ",0
_UNKWN:   DEFB "unknown",0
_IOERR:   DEFB CR,LF,"Param error",0

; VT100 sequences
CURS_UP:   DEFB ESC,"[A",NULL
COLSTR:    DEFB CR,ESC,'[25C',NULL

_ERROR       DEFB "Error",0
_FULL:       DEFB "Full",NULL
_APPPG:      DEFB "App pages: ", NULL
_yes:        DEFB "YES", NULL
_no:         DEFB "NO", NULL
_NODRV       DEFB "No OS", NULL
_TMPBC       DEFB "Temp: @", NULL
_PRMBC       DEFB "BP:   @", NULL
_BRK         DEFB "BREAK...", NULL
_NOSDADD     DEFB "Bad SDcard address", NULL
_NODRIVE     DEFB "Drive A-", 'A'+15, NULL
_MAPD        DEFB "Map disk: ",NULL
_MAPD_TO:    DEFB " to drive ", NULL
_VDIS:       DEFB "Video",TAB,NULL
_CFGSW:      DEFB "Cfg Sw",TAB,NULL
_SDIS:       DEFB "SDCard",TAB,NULL
_SDEV:       DEFB "sd",NULL
_PDIS:       DEFB "PIO",TAB,NULL
_EMP:        DEFB " Missing", NULL
_PRE:        DEFB " Present", NULL
_INST:       DEFB "Installed", NULL
_SFND:       DEFB "Found", NULL
_NOTF:       DEFB "Not found", NULL
_NOTEMP:     DEFB "Not empty", NULL

; ---------- CFG_TAB
; Set of boolean flags that can be configured in NVRAM. Format is:
; MASK | ByteOffset | Desc | 0
CFG_TAB:        DEFB      00000001b                ; Bit 0
                DEFB      0
                DEFB      "Install OS: ",0

                DEFB      00000010b                ; Bit 1
                DEFB      0
                DEFB      "Break handler: ",0

                DEFB      0         ; Terminator


          DSEG
; ---------------------- VARIABLES
; If you want the monitor code in ROM then add an ORG here to locate variables somewhere in RAM
INITD:     DEFS    1              ; Set to '1' once OS initialissation for an application has been done.
OPMODE:    DEFS    1              ; Operational mode. 1=normal. 2=debug
CMDTAB:    DEFS    2              ; Operational mode. 1=normal. 2=debug
LAST_CMD:  DEFS    1
CTRLCH:    DEFS    1

HW_SWTCH:  DEFS    1              ; Status of the 3 hardware config bits on boot

FIN_CODE:  DEFB    0
AUTO_RUN:  DEFB    0


SDPAGE     DEFS    512

; Breakpoints. Each entry is 4 bytes:
; Type|AddrX2|Opcode
; A breakpint in placed in memory by replacing an instruction with a single byte RST 20h (or replacement) opcode
; Type: 0 - free slot - can be used to store a new breakpoint
;       1 - single shot - removed once hit
;       2 - permanent breakpoint (WIP)
BPOINTS    DEFS    NUM_BK*BP_SIZE

; If 'AUTO_RUN' is non-zero then the BP handler will automatically execute a 'GO', setting new BPs
AUTO_STP   DEFS    1
BPCT       DEFS    1      ; Value passed into INSTBP

DUMP_CHRS  EQU   SCRATCH


; Single Step Count...
STP_CNT    DEFS    2      ; Up to 64K steps!!!
STP_IN:    DEFS    1      ; Temp store for setting a BP. True means step into and CALL ops.
.END
