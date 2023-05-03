import ../zlib/defs.asm
import config.asm
import pcb_def.asm


; run.asm
; Code to configure and run an application. There are two types of application:
;   1. 'bare'      - doesn't use any ZIOS services and does it's own thing. Has
;                    complete freedom to use all the available hardware and
;                    has a full 64K memory space.
;   2. 'supported' - the application uses ZIOS for core hardware services. In
;                    this case the application is restricted from using the top
;                    512 bytes of memory and there is a RST 30h handler installed
;                    to access ZIOS primitives.
; This file prepares the run environment for both these situations.

            extrn  ISRCTXT,R_PC_S,AND_RUN,RAWGO
            extrn  BRK_HK,AP_ST,DO_BP_S,CHR_ISR,CTC_ISR
            extrn  P_MAPX,ZS_BRK
            extrn  CNTINUE
            extrn  NUL_ISR,BANKST
            extrn  ZIOS_OFF
            extrn  main

            CSEG

; ------- INSTDRV
; Check whether drivers are installed. If not check if they should be installed. Then
; intall them if reqjuired.
; INPUT: HL - execution address, which may be zero.
INSTDRV:: ; Check configuration to see whether we're meant to be installing drivers
          BANK   1,(PAGE_MP)          ; Application page 0 => bank 1
          BANK   2,(PAGE_MP+3)        ; Application page 3 => bank 2

          LD     A,(P_FLAGS)
          RRCA
          JR     C,.dodrv

          ; If the start address is NOT zero then write a jump instruction
          ; to the start of application memory.
          LD     A,H
          OR     L
          RET    Z

          LD    A,$C3          ; JP
          LD    ($4000),A
          LD    ($4001),HL     ; Start address

          ; ZIOS service not required. Disable the hardware interrupt sources we're using
          ; to avoid the running software having to programme hardware it doesn't need.
          CALL   ZIOS_OFF

          OR    A
          RET

.dodrv:   ; Copy our driver code into the end of APPLICATION memory. It's
          ; currently at the end of our second bank.
          PUSH  HL                     ; Save start address
          LD    HL,$FE00               ; Our top 512 bytes need to be copied to the application page
          LD    DE,$BE00               ; Application last page
          LD    BC,512                 ; Transfer 512 bytes
          LDIR
          POP   HL                     ; Restore start address

          ; Store HL (run address) in the app copy of our drivers
          LD     (R_PC_S - $C000 + $8000),HL
          PUSH   AF                    ; Partially rotate exec flags

          ; Patch the dispatch point for the API in page 0 (RST 30h)
          LD     A,$C3
          LD     (4030h),A             ; API entry point: RST 30h
          LD     HL,AP_ST
          LD     (4031h), HL

          ; Set all interrupt vectors to a dummy handler
          LD     DE,NUL_ISR
          LD     HL,8000h+SIO_IB
          LD     B,16
_nxtisr:  LD     (HL),E
          INC    HL
          LD     (HL),D
          INC    HL
          DJNZ   _nxtisr

          ; Patch the application SIO ISR. It's different for application
          ; space because page memory tweaks will be requied.
          LD   HL,CHR_ISR
          LD   (8000h+SIO_ARX),HL

          ; And patch the timer ISR
          LD   HL,CTC_ISR
          LD   (8000h+CTC_ICH1),HL

          POP    AF
          RRCA               ; Terminal input break handler?
          LD     HL,0
          JR     NC,_nobrk

          ; Install the break driver. Conditional on bit 1 of the flags tested above.
          LD    HL,ZS_BRK    ; Break handler stored in ZIOS memory
_nobrk:   LD    (BRK_HK),HL
          PUSH  AF           ; Still the flags rotated once more

          ; Set the ISR context and CTRLCH to zero - set as part of application ISR preamble
          ; CTRLCH will be set in the CTRL-C handler and prevents an eroneous decrement of
          ; the programme handler in DO_BP
          LD    HL,0
          LD    (ISRCTXT),HL           ; And CTRLCH

          POP  AF                      ; Get exec flags back
          RRCA                         ; BP/debugger handling required?
          JR   NC,.setres
          LD   A,$C3
          LD   (4000h+BRK_HANDLER),A   ; Breakpoint handler
          LD   HL,DO_BP_S              ; Handler in app AND ZIOS memory
          LD   (4001h+BRK_HANDLER),HL  ; Breakpoint handler target address

          ; Data in the application pages is now all set but not yet correctly
          ; mapped. Returning with application pages 0 and 3 mapped to banks
          ; 1 and 2.

          ; Set carry flag to indicate OS installed
.setres:  SCF
          RET

; ------- RUNPROC
; Run the current PCB from the address in HL.
; INPUT   HL - address from which to execute application code.
PR_RUN::  CALL   INSTDRV

          PUSH   AF            ; 'C' flag set if ZIOS API installed

          ; Map all application pages to the correct place EXCEPT this one.
          LD     HL,PAGE_MP+2
          BANK   2,(HL)
          DEC    HL
          BANK   1,(HL)
          DEC    HL
          BANK   0,(HL)

          ; How we run this process depends on whether the ZIOS services
          ; are installed. If they aren't then
          POP    AF
          JR     C,AND_RUN

          ; No drivers installed. Going to run from zero. If HL (start addr)
          ; in NOT zero then force a jump to the start of application space
          LD     A,(PAGE_MP+3)
          JR     RAWGO

; ------------------- MAPAPP
; Map application pages 0 to 2 into Z80 memory. This leaves only the
; bank 3 to be mapped back into memory space.
MAPAPP:   LD     HL,PAGE_MP+2
          BANK   2,(HL)
          DEC    HL
          BANK   1,(HL)
          DEC    HL
          BANK   0,(HL)
          RET

; ------------------ PR_REST
; Restore execution status from PCB and run. Called with:
;   BANK 0: ZIOS(0)
;   BANK 1: -unknown-
;   BANK 2: -unknown-
;   BANK 3: ZIOS(1)
PR_REST:: CALL   INSTDRV     ; This could be the very first exec of this process

          RET    NC          ; Can't do anything here if there's no ZIOS installed

          ; INSTDRV will have mapped app page 0 into bank1 amd page 3 into bank 2
          LD     HL,(R_SP)
          PUSH   HL
          CALL   P_MAPX                ; Stack memory now accessible in HL
          LD     DE,(R_AF)             ; Simulate pushing AF onto app stack
          DEC    HL
          LD     (HL),D
          DEC    HL
          LD     (HL),E
          POP    HL                    ; Get the original unmapped stack
          DEC    HL
          DEC    HL                    ; So we can later POP AF
          LD     (R_SP),HL

          BANK   2,(PAGE_MP+3)         ; Make application page 3 available

          ; Set up the jump address
          LD    HL,(R_PC)
          LD    (R_PC_S - $C000 + $8000),HL  ; Where we want to continue execution from

          CALL   MAPAPP                ; All pages EXCEPT the one we're in

          ; Restore as many regs as possible here. No longer need our stack so can
          ; discard. Only thing that can't be restored is A which is needed to reset
          ; the application space.
          LD    SP,R_AF_P
          POP   AF
          EX    AF,AF'      ; '
          POP   BC
          POP   DE
          POP   HL
          EXX                 ; Alternate register set restored
          POP   BC
          POP   DE
          POP   HL
          POP   IX
          POP   IY

          LD    SP,(R_SP)     ; and load the application SP
          LD    A,(PAGE_MP+3) ; and the page that needs to go into bank 0

          JR    CNTINUE       ; And......... GO!
