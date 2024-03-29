# ZLib

A set of library routines to drive the various hardware elements of the board. These are 'included' in the zloader monitor programme and can be used for other applications are required. This library also illustrates how to access the hardware.

+ **defs.asm** - common hardware definitions and some macros
+ **zlib.asm** - an include file containing `extrn` declarations for the exported ZLib interface. This can be included in assembler files that want to use ZLib APIs.
+ **libsio.asm** - A set of low level serial I/O routines for the Z80 SIO. Handles single character input/output and has an Interrupt Service Routine (ISR)that will handle input. The ISR is *not* installed - it's up to application code to install as necessary.
+ **libconin.asm** - A set of higher level console input routines that provide buffered input and also implement a simple 'history' function allowing applications to record and replay previous entered lines (see GETHIST/SETHIST). If enabled then the console code interprets VT100 up/down/left/right keys to edit the current input line.
+ **libconout.asm** - A set of higher level console output routines for printing text/numbers.
+ **libcmd.asm** I connect a Raspberry Pi to serial port B. This library implements a simple block based command protocol to talk with the Pi. At the Pi end there's a server that waits for commands from the Z80 and sends a suitable response. There's no error correction of handshaking and the two ends can sometimes get out of sync, usually fixed by resetting the Z80. The Pi end of the protocol is implemented in tools/block.js
+ **libflash.asm** - A set of low level routines that implement the Flash write algorithm. This makes writing to the onboard flash device fairly straightforward and means, for example, ZLoader can be upgraded without having to use an external flash programmer.
+ **libi2c.asm** - low level **I2C** interface library. Provides initialisation, read and write byte operations
+ **libspi.asm** - low level **SPI** interface library.
+ **librtc.asm** - uses the I2C interface to talk with the real time clock (RTC) chip. Routines allow read/write for time and to the available user non-volatile storage.
+ **libsdc.asm** - SDCard initialisation and block read/write functions. Limited right now to cards less than 2GB and only supports the first SDCard 'slot'.
+ **libmisc.asm** - Small miscellaneous hardware routines which didn't fit anywhere else. For example reading the hardware configuration switches.
+ **libutils** - Utility routines used within ZLib that aren't directly related to specific hardware. For example there's a string compare routine and binary to decimal conversion routines.
