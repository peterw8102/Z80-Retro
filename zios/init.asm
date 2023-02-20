import defs.asm
import config.asm
import zlib.asm

  extrn   P_MIN,PR_INIT
  extrn   AP_DISP
  public  ZIOS_INI

; init.asm
; System initialisation code. ZIOS must be initialised after the loader
; has copied the code into executable RAM.

            extrn  SDPREP,RTC_INI,NVLD,INITSIO,SERINT
            ; public ZIOS_INI

            CSEG

; ---------- ZIOS_INI
; Flash has been copied to RAM and before using any ZIOS services this function
; must be called. There are no parameters however the function will return
; the status of the hardware configuration switches. The lower bits of this
; will tell the loader what automatic/boot behaviour to take.
; OUTPUT: A   - the hardware status register (bits 0-2)
ZIOS_INI::  LD    A,MN2_PG+1
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

            ; Install RST 30h for API access.
            LD     A,$C3
            LD     (30h),A
            LD     HL,AP_DISP
            LD     (31h),HL

            ; Return the status of the hardware configuration switch
            CALL   SW_CFG

            RET

; ---- _EISR
; Serial port rx ISR
_EISR:      CALL    SERINT
            EI
            RETI
