import ../zlib/defs.asm
import config.asm

  extrn   P_MIN, PR_INIT
  public  ZIOS_INI

; init.asm
; System initialisation code. ZIOS must be initialised after the loader
; has copied the code into executable RAM.

            extrn  SDPREP,RTC_INI,NVLD,INITSIO,SERINT
            ; public ZIOS_INI

            CSEG

ZIOS_INI::  ; If this is a developement build, reserve the base pages.
            LD    A,MN2_PG+1
            CALL  P_MIN
            CALL  PR_INIT

            ; Initialise drive map. Map logical drives 0-15 to physical blocks 1-16. Need to store
            ; the physical address << 6 (Upper 10 bits of the 32 bit SD card address).
            CALL   SDPREP

            ; Initialise i2c
            CALL   RTC_INI

            ; and read the NV RAM
            CALL   NVLD

            ; Initialise the interrupt vector table. Table will be right at the end of the
            ; memory map which will be in our second RAM page and will be copied for
            ; application context.
            LD     A,VEC_BASE
            LD     I,A

            ; Add entry to vector table for supervisor SIO ISR
            LD     HL,_EISR
            LD     (0xC000+SIO_ARX),HL

            ; Initialise the serial IO module
            XOR    A              ; Initialise SIO without setting up interrupt vectors.
            CALL   INITSIO
            IM     2              ; Using interrupt mode 1 AT THE MOMENT. Need to move to vectored at some point
            EI
            RET

; ---- _EISR
; Serial port rx ISR
_EISR:      CALL    SERINT
            EI
            RETI
