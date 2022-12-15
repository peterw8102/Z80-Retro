import ../zlib/defs.asm
import config.asm

; appl.asm
; This code is assembled to be located in the top 512 bytes of RAM and be
; copied there from the application address space. To do this the origin is
; set to $3E00 (where it'll reside in the hex output records) and the `phase`
;  directive tells the assember to treat the code as though it is running
; from $FE00.

            public AP_ST, END_APP
            extrn  CONTXT, PAGE_MP, MON_MP, AP_DISP, APP_STK

            ASEG
            ORG   $3E00
            PHASE $FE00


; Entry point for the command handler. Action depends on whether this is called
; from ZLoader or from the application. Use the CONTXT to decide.
AP_ST:    LD    A,(CONTXT)
          OR    A
          JR    Z,_issup        ; Running in ZLoader context so no changes required.

          ; Map ZLoader into memory (Block 0). 'A' can't be used to pass parameters and
          ; doesn't need to be saved.
          DI
          LD    A,(MON_MP)      ; Page mask for page zero monitor
          OUT   (PG_PORT0),A

          ; Save the application stack
          LD     (APP_STK),SP      ; Save the application stack
          LD     SP,SP_STK         ; Replace with supervisor stack
          EI

          ; Make the end handler unwind the service changes
          PUSH   HL                ; Need to preserve the content of HL
          LD     HL,AP_END         ; while pushing the restore address
          EX     (SP),HL

          ; And go the the dispatcher
          JR     AP_DISP

; Request made from within the ZLoader so no complicated memory changes. Just invoke the dispatcher.
_issup:   PUSH   HL                ; Need to preserve the content of HL
          LD     HL, Z_END         ; while pushing the restore address
          EX     (SP),HL
_dis:     JR     AP_DISP

; At the end of a dispatch called by the application, map application memory back and repair the stack
AP_END:   LD     (SAVA),A       ; A needs to be preserved - many contain API results
          DI
          LD     SP,(APP_STK)   ; Got the appication stack back but it might not be in our address space so can't use
          LD     A,(PAGE_MP)
          OUT    (PG_PORT0),A   ; Back into application space
          EI                    ; Safe to allow interrupts again
          LD     A,(SAVA)       ; Restore A. The flags are unchanged
Z_END:    RET                   ; And carry on

SAVSTK:   DS     2
SAVA:     DS     1

END_APP:  EQU    $
