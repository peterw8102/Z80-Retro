; Constants that can be used to confugure assembler time options for the
; monitor.

; #define WRITE_CRLF   LD A,CR \ RST 08H \ LD A,LF \ RST 08H
YES      .EQU      1
NO       .EQU      0

; If you want to run the monitor from ROM then variables need to be in RAM. Set this
; label to indicate where you want variables and change the value of FLASH_MON to YES
VARS_ADDR   .EQU    2000h
FLASH_MON   .EQU   NO

; Configurable parameters
LOAD_ADDR:   .EQU    01D0h     ; Start address for the main code

; Where to initialise the supervisors stack If the stack is already configured set
; this to zero. The entire monitor runs in the bottom 16K and so runs in a single
; memory page. The stack is set to start at the end of that page.
SP_STK:      .EQU     4000h - 200h

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

; The first MMU page into which to load code using the various load commands. Leave this
; value unchanged.
LD_PGOFF    EQU   01h + IS_DEVEL

; Decide whether to install the character set (depends whether the graphic card is installed). If
; there's no graphic card then CSET can be set to 0 to disable however there are no problems leaving
; this enabled.
CSET        EQU   1
if IS_DEVEL
CSET_PG     EQU   $23
else
CSET_PG     EQU   $2
endif
