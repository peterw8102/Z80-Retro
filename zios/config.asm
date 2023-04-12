; Constants that can be used to confugure assembler time options for the
; monitor.

YES      .EQU      1
NO       .EQU      0

; ZIOS reserves the first 16 bytes of NVRAM in the RTC and are
; protected by a checksum. The first byte contains the default
; application run flags .
CF_LOADOS   EQU   00000001b
CF_BREAK    EQU   00000010b
CF_DEBUG    EQU   00000100b

; Default flag byte if NVRAM invalid
CFG_DEF     EQU   CF_DEBUG|CF_LOADOS|CF_BREAK

; Bits of the flag byte that are used for process execution.
CFG_EXEC    EQU   CF_DEBUG|CF_LOADOS|CF_BREAK

; Where to initialise the supervisors stack.
SP_STK:      .EQU     $FE00

; Breakpoints in code are handled by replacing the opcode at the break location with a RST instruction. The
; default is RST 20h. You can change this if your system is using RST 20 for something else.
BRK_OPCODE:  .EQU     0E7h     ; The instruction to use to cause a breakpoint. This must be a RST xx single byte op. Default RST 20h
BRK_HANDLER: .EQU      28h     ; The address at which to origin the break point handler Must match BRK_OPCODE

; Configuring how the monitor runs.
; IS_DEVEL should be 1h when developing new versions of the monitor (which then locates
; the monitor in PAGE 1 of available memory). Set IS_DEVEL to '0' and rebuild to create
; a version to burn to FLASH. Here we use
ifdef RELEASE
IS_DEVEL    EQU    0h          ; Make adjustments if we're testing a new loader. SET THIS
else
IS_DEVEL    EQU    1h          ; Make adjustments if we're testing a new loader. SET THIS
endif

ifdef OVERWRITE
; Overwrite flash resident ZLoader with the development version, so page numbers are different.
ZL_OFFSET   EQU    0
else
ZL_OFFSET   EQU    2
endif

; RAM_PG_0 is the first page MMU page which is RAM. Generally Flash occupies low page
; numbers while RAM resides from page 20h. This can be swapped with a jumper on the
; board, although this is more for historical reasons.
RAM_PG_0    EQU   20h
FSH_PG_0    EQU   00h

; The first MMU page into which to load code using the various load commands. Leave this
; value unchanged.
LD_PAGE     EQU   RAM_PG_0 + 2 + ZL_OFFSET

; Decide whether to install the character set (depends whether the graphic card is installed). If
; there's no graphic card then CSET can be set to 0 to disable however there are no problems leaving
; this enabled.
CSET        EQU   1
CSET_PG     EQU   2        ; Page from which to load the default character set. Always load the one from flash

; MN_PG is the memory page number from which this loader is running. NORMALLY this will be the page number
; of the first RAM page (20h if RAM is high). In debug this needs to be 1 because the real monitor loads
; into the first page.
MN_PG       EQU   RAM_PG_0+ZL_OFFSET

; The extended page for the loader is stored in the next RAM page after MN_PG.
MN2_PG      EQU   MN_PG+1

; Where to put the interrupt vector table. Place it in the top 256 bytes of RAM (ZIOS reserved area)
VEC_BASE    EQU    0xFF

ISR_BASE    EQU   0x3FE0

; Page offset to the ISR handlers
SIO_IB      EQU   0x3FE0

; Interupt vectors for each SIO identifiable condition (WR 1, Ch. B, Bit 2 set)
SIO_BTX     EQU   SIO_IB
SIO_BST     EQU   SIO_IB+2
SIO_BRX     EQU   SIO_IB+4
SIO_BSP     EQU   SIO_IB+6

SIO_ATX     EQU   SIO_IB+8
SIO_AST     EQU   SIO_IB+10
SIO_ARX     EQU   SIO_IB+12
SIO_ASP     EQU   SIO_IB+14

; Interrupt vectors for CTC
CTC_IB      EQU   0x3FF0
CTC_ICH0    EQU   CTC_IB
CTC_ICH1    EQU   CTC_IB+2
CTC_ICH2    EQU   CTC_IB+4
CTC_ICH3    EQU   CTC_IB+6

; ------------- KNOWN ADDRESSES -------------
; A loader application (LOADER) should install several entry points
; at a number of well known locations in the loader code page 0:
;    Address 04:  ZIOS will jump here to warm start the monitor CLI
;            0B:  ZIOS jumps here when a breakpoint (RST 28h) occurs
; Declare a warm start address for whatever loaded ZIOS. Jump to this to get
; back to the monitor level CLI.
WARMSTRT    EQU   04h
HNDL_BP     EQU   0bh
HNDL_BRK    EQU   13h
