import ../zlib/defs.asm
import config.asm

; appl.asm
; This code is assembled to be located in the top 512 bytes of RAM and be
; copied there from the application address space. To do this the origin is
; set to $3E00 (where it'll reside in the hex output records) and the `phase`
;  directive tells the assember to treat the code as though it is running
; from $FE00.

            public AP_ST,END_APP,DO_BP_S,CNTINUE,AND_RUN,CHR_ISR,RAWGO
            public R_PC_S,R_AF_S
            extrn  CONTXT,PAGE_MP,AP_DISP,APP_STK,SERINT
            extrn  DO_BP,ISRCTXT

            ASEG
            ORG   $3E00
            PHASE $FE00


; Entry point for the command handler WHEN CALLED FROM APPLICATION SPACE.
AP_ST:    ; Map ZLoader into memory (Block 0). 'A' can't be used to pass parameters and
          ; doesn't need to be saved.
          LD    (R_AF_S+1),A
          DI
          LD    A,MN_PG            ; Page mask for page zero monitor
          OUT   (PG_PORT0),A

          ; Save the application stack
          LD     (APP_STK),SP      ; Save the application stack
          LD     SP,SP_STK         ; Replace with supervisor stack
          EI
          LD    A,(R_AF_S+1)
          CALL   AP_DISP

          ; At the end of a dispatch called by the application, map application memory back and repair the stack
AP_END:   LD     (R_AF_S+1),A       ; A needs to be preserved - many contain API results
          DI
          LD     SP,(APP_STK)   ; Got the appication stack back but it might not be in our address space so can't use

          ; Restore other pages that may have been modified first
          LD     A,(PAGE_MP+1)
          OUT    (PG_PORT0+1),A   ; Back into application space
          ; Put block zero back last because we're using data stored here
          LD     A,(PAGE_MP)
          OUT    (PG_PORT0),A   ; Back into application space
          EI                    ; Safe to allow interrupts again
          ; Block 2 is never modified by ZLoader and block 3 remains fixed.

          LD     A,(R_AF_S+1)   ; Restore A. The flags are unchanged
          RET                   ; And carry on

;----- DO_BP_S
; Setup for DO_BP living in the application space.
; DON'T DO ANYTHING THAT SETS FLAGS!!!
DO_BP_S:  DI                   ; have to disable interrupts while in the debugger
          LD    (R_AF_S+1),A   ; So we can page switch... But flags NOT stored so don't damage!
          LD    A,MN_PG
          OUT   (PG_PORT0),A   ; Switched out the application page. Now running from supervisor
          LD    A,(R_AF_S+1)   ; So we can page switch... But flags NOT stored so don't damage!
          EI
          JR    DO_BP          ; In supervisor mode so can do breakpoint.

; ------ CONT
; Continue execution. Map all application pages into memory (PAGE_MP), restore the
; application registers then continue from where we left off.
CNTINUE:  OUT   (PG_PORT0),A
          POP   AF

          ; Everything now into application space so can just go...
          EI
          JR     JP_RUN

;----- RUN
; Just map application page 0 into bank 0 and go to zero.
AND_RUN:  LD     A,(PAGE_MP)
          OUT    (PG_PORT0),A
          RST    0          ; And run from address zero

; Character ISR when running application code. Switch supervisor code to block zero
CHR_ISR:  PUSH   AF
          LD     A,MN_PG
          OUT    (PG_PORT0),A        ; Supervisor in CPU memory page 0
          LD     (APP_STK),SP        ; Save the application stack
          LD     SP,SP_STK           ; Get our supervisor stack
          LD     (ISRCTXT),A         ; Will be non-zero
          CALL   SERINT              ; Do the interrupt
          XOR    A
          LD     (ISRCTXT),A         ; Clear the context flag
          LD     SP,(APP_STK)        ; Restore the application stack
          LD     A,(PAGE_MP)         ; Context switch page 0
          OUT    (PG_PORT0),A        ; Back into application space
          POP    AF
          EI                         ; Back to application
          RETI


R_AF_S:   DS     2

; Debugger. Storage must be in page 0 reserved space
; JP_RUN - C3 is the JP opcode. By jumping to JP_RUN execution will
; continue from the current value of the PC. This avoids us having to
; push values onto the applications stack.
JP_RUN:    DEFB    $C3

; Storage area for working registers
R_PC_S     DEFS    2

END_APP:  EQU    $

          DEPHASE
          ORG   $3FFA
          PHASE $FFFA

; Map out last two application pages and allow the application to run from bank 0
; This code MUST be right at the end of bank 3 (last bytes) and "runs into" the
; application code at Z80 address 0.
; H: page number for app page 3
; L: page number for app page 0
RAWGO:   LD     A,L
         OUT    (PG_PORT0),A
         LD     A,H
         OUT    (PG_PORT0+3),A     ; Overwrite ourselves
