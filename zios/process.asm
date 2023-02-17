import ../zlib/defs.asm
import config.asm
import pcb_def.asm
; process.asm
; Utility routines for managing processes.

            public PR_INIT
            extrn  P_ALLOC
            extrn  WRITE_8

            CSEG

; ------ PR_INIT
; Initialise the PCB for a new process. This will need to do more
; but for now it initialises the active PCB space with 4 new pages.
;
; If there are no available memory pages then return with the CLEAR
; flag set.
PR_INIT:    LD    HL,PAGE_MP
            LD    B,4
            CALL  P_ALLOC
            PUSH  AF
            CALL  WRITE_8
            LD    A,'-'
            RST   08h
            POP   AF
            RET   C
_wrn:       LD    (HL),A         ; Initialise the application page map
            INC   A
            INC   HL
            DJNZ  _wrn
            RET
