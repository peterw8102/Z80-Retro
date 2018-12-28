; #define WRITE_CRLF   LD A,CR \ RST 08H \ LD A,LF \ RST 08H
YES:      .EQU      1
NO:       .EQU      0

; If you want to run the monitor from ROM then variables need to be in RAM. Set this
; label to indicate where you want variables and change the value of FLASH_MON to YES
VARS_ADDR:   .EQU    2000h
FLASH_MON:   .EQU   NO

; Configurable parameters
LOAD_ADDR:   .EQU    01D0h     ; Start address for the main code

; Where to initialise our stack If the stack is already configured set this to zero. If
; my system I have 32K memory pages so I run everything from the first 32 allowing me to page in
; data to the upper 32K
STACK:       .EQU     07ff0h

; My 2x32K pages are controlled by an 8bit write-only I/O port. This is the address of
; the port. The 'B' command writes a value to this port.
PAGE_REG:    .EQU     0E0h     ; I/O address for 8 bit memory page select register (B command)

; Breakpoints in code are handled by replacing the opcode at the break location with a RST instruction. The
; default is RST 20h. You can change this if your system is using RST 20 for something else.
BRK_OPCODE:  .EQU     0E7h     ; The instruction to use to cause a breakpoint. This must be a RST xx single byte op. Default RST 20h
BRK_HANDLER: .EQU      20h     ; The address at which to origin the break point handler Must match BRK_OPCODE

; The 'U' command replaces the current running monitor with a modified copy at MON_COPY. This
; is generally used for developing the monitor and doesn't have a lot of use in a running
; environment. To make the copy a small copy loop is written to unused memory at LOADER.
MON_COPY:    .EQU    2000h  ; Where the updated monitor executable has been loaded
LOADER:      .EQU    7800h  ; A safe, unused area of memory where a short copy loop can be writte.

; Include platform specific commands
PLATFORM     .EQU    0
