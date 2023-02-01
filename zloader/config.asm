; Constants that can be used to confugure assembler time options for the
; monitor.

YES      .EQU      1
NO       .EQU      0

; Where to initialise the supervisors stack If the stack is already configured set
; this to zero. The entire monitor runs in the bottom 16K and so runs in a single
; memory page. The stack goes below the 512byte reserved area and the libsio history
; buffer.
SP_STK:      .EQU     4000h - 1024

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

; RAM_PG_0 is the first page MMU page which is RAM. Generally Flash occupies low page
; numbers while RAM resides from page 20h. This can be swapped with a jumper on the
; board, although this is more for historical reasons.
RAM_PG_0    EQU   20h
FSH_PG_0    EQU   00h

; The first MMU page into which to load code using the various load commands. Leave this
; value unchanged.
LD_PAGE     EQU   RAM_PG_0 + 1 + IS_DEVEL + IS_DEVEL

; Decide whether to install the character set (depends whether the graphic card is installed). If
; there's no graphic card then CSET can be set to 0 to disable however there are no problems leaving
; this enabled.
CSET        EQU   1
if IS_DEVEL
CSET_PG     EQU   $23
else
CSET_PG     EQU   $2
endif

; MN_PG is the memory page number from which this loader is running. NORMALLY this will be the page number
; of the first RAM page (20h if RAM is high). In debug this needs to be 1 because the real monitor loads
; into the first page.
MN_PG       EQU   RAM_PG_0 + IS_DEVEL

; The extended page for the loader is stored in the next RAM page after MN_PG.
MN2_PG      EQU   MN_PG+1
