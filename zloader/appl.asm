import ../zlib/defs.asm
import config.asm

; appl.asm
; This code is assembled to be located in the top 512 bytes of RAM and be
; copied to the application address space. The code is assembled to load
; from 0xFE00 (512 bytes) but set to load into address 0x3E00 (end of page zero)

            public AP_ST, END_APP
            extrn  CONTXT, APP_STK, PAGE_MP, MON_MP, AP_DISP

            ASEG
            ORG   $3E00
            PHASE $FE00

; Entry point for the command handler. Action depends on whether this is called
; from ZLoader or from the application. Use the CONTXT to decide.
AP_ST:    LD    A,(CONTXT)
          OR    A
          JR    Z,_issup       ; Running in ZLoader context so no changes required.

          ; Map ZLoader into memory (Block 0)
          DI
          LD    A,(MON_MP)     ; Page mask for page zero monitor
          OUT   (PG_PORT0),A

          ; Save the application stack
          LD     (APP_STK),SP   ; Save the application stack
          LD     SP,SP_STK      ; Replace with supervisor stack
          EI

          ; Make the end handler unwind the service changes
          LD     HL,AP_END
          PUSH   HL

          ; And go the the dispatcher
          JR     AP_DISP

; Request make from within the ZLoader so no complicated memory changes. Just invoke the dispatcher.
_issup:   LD     HL, Z_END          ; How to restore context
          PUSH   HL
          JR     AP_DISP

; At the end of a dispatch called by the application, map application memory back and repair the stack
AP_END:   DI
          LD     SP,(APP_STK)   ; Got the appication stack back but it might not be in our address space so can't use
          LD     A,(PAGE_MP)
          OUT    (PG_PORT0),A   ; Back into application space
          EI                    ; Safe to allow interrupts again
Z_END:    RET                   ; And carry on

END_APP:  EQU    $
