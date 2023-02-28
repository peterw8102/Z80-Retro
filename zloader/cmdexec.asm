; **********************************************
; Implements: 'DI', 'DM', 'DN', 'D' commands
; Syntax:
;   S [n]          Step into
;   N [n]          Step over
;   G [addr]       Run [from address]
;   BP [-][addr]   Set or clear a breakpoint
;
; 'n' is the number of steps to take
;
; '.' for the address uses the current value
; of the application programme counter.
;
; Ommitting an address will dump the next block
; of addresses.
;
; If 'lines' is ommitted then 8 lines are
; displayed.
;
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

  ; From breakpoints
  extrn  STP_HOW,STP_CNT,SSTEP_BP,INSTBP,AUTO_STP,FINDBP,SETBP,CLRBP,BPOINTS

  ; From core
  extrn  main,MORE
  extrn  E_BADPS

  public SSTEP,NSTEP,RUN,GO,BP,DO_GO,NEXTSTP


; Number of breakpoint locations (in code)
NUM_BK    EQU    64
BP_SIZE   EQU     4


; ----- Single Step
; Optional parameter: decimal number indicating number of steps to repeat.
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

; ------ go
; Execute code. If an address is specified then the application PC is set to this
; before starting execution from that address. If there's no address then the
; application is run from whatever address is stored in the application PC (R_PC)
; register.
GO:       CALL  WASTESPC
          JR    Z,NEXTSTP

          ; Is there an address?
          CALL  GET_HEX        ; Will be zero if there was no value specified
          JR    Z,NEXTSTP       ; Don't upset the stored PC if nothing entered (go from PC address)

          ; Have an address - put it in the virtual PC
          LD    (R_PC),HL

NEXTSTP:  LD    HL,1
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


; ----- BP: Set breakpoint
; Get address from command line. If not specfied then BP at the current PC value.
; The optional parameter is either an address (hex) OR an address prefixed with
; a hyphen '-'. The hyphen means "remove any breakpoint at this address".
BP:       CALL  WASTESPC
          JR    Z,_LISTBP

          LD    E,1
          CP    '-'              ; Removing a breakpoint
          JR    NZ,_gadd
          CALL  BUFCHR           ; Waste the '-'
          DEC   E
_gadd:    CALL  GET_HEX          ; Address for breakpoint
          JR    Z, E_BADPS

          ; Have an address. Set or clear a BP
          DEC   E

          JR    Z,_doadbp

          ; Remove BP at address specified
          EX    DE,HL
          CALL  CLRBP

          ; JR    Z,_LISTBP
          ; LD    (HL),0           ; Cleared
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

; ----- RUN
; This initial version loads the first four RAM pages into the four
; banks, reserving the last 16 bytes of page 03 to store an address
; switcher. Once switch it jumps to address 0 to run that code.
RUN:      DI
          CALL  WASTESPC
          LD    HL,0
          JR    Z,PR_RUN
          CALL  GET_HEX    ; Get optional execution address into HL
          JR    PR_RUN



; ----- Step Over
NSTEP:    LD    A,'N'
          LD    (STP_HOW),A
          LD    HL,1
          LD    (STP_CNT),HL   ; By default step once
          LD    E,0          ; In E
          CALL  SSTEP_BP
          JR    DO_GO

_TMPBC       DEFB "Temp: @", NULL
_PRMBC       DEFB "BP:   @", NULL
