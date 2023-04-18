import ../zlib/defs.asm
import config.asm
import pcb_def.asm
; process.asm
; Utility routines for managing processes.

            public PR_INIT
            extrn  P_ALLOC,NVRAM

            CSEG

; ------ PR_INIT
; Initialise the PCB for a new process. This will need to do more
; but for now it initialises the active PCB space with 4 new pages.
;
; If there are no available memory pages then return with the CLEAR
; flag set.
PR_INIT:    LD    HL,PAGE_MP
            LD    B,4
_wrn:       CALL  P_ALLOC
            RET   C
            LD    (HL),A         ; Initialise the application page map
            INC   A
            INC   HL
            DJNZ  _wrn

            ; Set the default execution flags for this process to match the NVRAM exec flags
            LD    A,(NVRAM)
            AND   CFG_EXEC
            LD    (P_FLAGS),A
            RET
