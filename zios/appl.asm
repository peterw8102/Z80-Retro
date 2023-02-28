import defs.asm
import config.asm
import zlib.asm
import pcb_def.asm

; appl.asm
; This code is assembled to be located in the top 512 bytes of RAM and be
; copied there from the ZIOS address space. To do this the origin is
; set to $7E00 (where it'll reside in the hex output records) and the `phase`
;  directive tells the assember to treat the code as though it is running
; from $FE00.
;
; Now ZIOS is taking two RAM pages both must be mapped into the Z80 address

            public AP_ST,END_APP,DO_BP_S,CNTINUE,AND_RUN,CHR_ISR,RAWGO
            public R_PC_S,JP_RUN,ZS_BRK
            extrn  AP_DISP
            extrn  ISRCTXT

            CSEG

; Entry point for the command handler WHEN CALLED FROM APPLICATION SPACE.
AP_ST:    ; Map ZLoader into memory (Block 0). 'A' can't be used to pass parameters and
          ; doesn't need to be saved.
          DI
          ; Move out pages into bank 0 and 3
          BANK   0,MN_PG
          BANK   3,MN2_PG          ; Need this one because the IV table changes

          ; Save the application stack
          LD     (R_SP),SP         ; Save the application stack
          LD     SP,SP_STK         ; Replace with supervisor stack
          EI
          CALL   AP_DISP

          ; At the end of a dispatch called by the application, map application memory back and repair the stack
AP_END:   DI
          LD     I,A            ; I is the only register we can safely use at the moment.
          LD     SP,(R_SP)      ; Got the application stack back but it might not be in our address space so can't use
          ; Restore application memory pages. Could be done faster but this only uses A
          BANK   0,(PAGE_MP)
          BANK   1,(PAGE_MP+1)  ; Back into application space
          BANK   2,(PAGE_MP+2)  ; Back into application space
          BANK   3,(PAGE_MP+3)  ; Back into application space
          LD     A,I            ; Now the memory is OK, put A somewhere we can get to it (could be stack)
          LD     (R_ACC),A   ; Definitely have the stack now so push AF where we can get it
          LD     A,VEC_BASE     ; Restore interrupt vector
          LD     I,A            ; Put I back to our vector before EI
          EI                    ; Safe to allow interrupts again
          LD     A,(R_ACC)   ; And restore A
          RET                   ; And carry on

;----- DO_BP_S
; Setup for DO_BP living in the application space.
; DON'T DO ANYTHING THAT SETS FLAGS!!!
DO_BP_S:  DI                   ; have to disable interrupts while in the debugger
          LD    I,A            ; Only register I can safely use
          BANK  0,MN_PG        ; Map in both ZIOS pages
          BANK  3,MN2_PG
          LD    A,I
          LD    (R_AF+1),A     ; So we can page switch... But flags NOT stored so don't damage!
          LD    A,VEC_BASE
          LD    I,A
          LD    A,(R_AF+1)     ; Get it back now
          EI
          JR    HNDL_BP        ; In supervisor mode so can do breakpoint.

; ------ CONT
; Continue execution. Map all application pages into memory (PAGE_MP), restore the
; application registers then continue from where we left off.
CNTINUE:  BANK  3              ; Map whatever's in A to bank 0
          POP   AF             ; App stack definitely now in memory so get AF back

          ; Everything now into application space so can just go...
          EI
          JR     JP_RUN

; Character ISR when running application code. Switch supervisor code
CHR_ISR:  DI
          PUSH   AF

          BANK   3,MN2_PG            ; ZIOS in CPU memory page 3
          LD     (R_SP),SP           ; Save the application stack into PCB
          LD     SP,SP_STK           ; Get our supervisor stack
          LD     (ISRCTXT),A         ; Will be non-zero
          CALL   SERINT              ; Do the interrupt
          XOR    A
          LD     (ISRCTXT),A         ; Clear the context flag
          LD     SP,(R_SP)           ; Restore the application stack
          BANK   3,(PAGE_MP+3)       ; Application space back into bank 0

          POP    AF
          EI                         ; Back to application
          RETI

; -------- ZIOS_BRK
; Place to jump to when a break character is trapped.
ZS_BRK::  BANK   0,MN_PG
          JR     HNDL_BRK

TMP_X:    DB     1
R_ACC:    DS     1

;----- RUN
; Just map application page 0 into bank 0 and go to zero.
AND_RUN:  BANK   3,(PAGE_MP+3)

; Debugger. Storage must be in page 0 reserved space
; JP_RUN - C3 is the JP opcode. By jumping to JP_RUN execution will
; continue from the current value of the PC. This avoids us having to
; push values onto the applications stack.
JP_RUN:   DEFB    $C3

; Storage area for working registers
R_PC_S    DEFS    2

END_APP:  EQU    $

          ASEG
          ORG   $FFFE

; Map out last two application pages and allow the application to run from bank 0
; This code MUST be right at the end of bank 3 (last bytes) and "runs into" the
; application code at Z80 address 0.
; A: page number for app page 3
; L: page number for app page 0
RAWGO:   BANK   3
