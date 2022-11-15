# Z80-Retro
Tools and code snippets I'm playing with while building my own Z80 SBC

## zlib

A set of library routines to drive the various hardware elements of the board. These are 'included' in the zloader monitor programme.

+ **defs.asm** common hardware definitions and some macros
+ **libcmd.asm** I connect a Raspberry Pi to serial port B. This library implements a simple block based command protocol to talk with the Pi. At the Pi end there's a server that waits for commands from the Z80 and sends a suitable response. There's no error correction of handshaking and the two ends can sometimes get out of sync, usually fixed by resetting the Z80. The Pi end of the protocol is implemented in tools/block.js
+ **libsio.asm** - A set of low level serial I/O routines for the Z80 SIO. Handles single character input/output and has an Interrupt Service Routine (ISR)that will handle input. The ISR is *not* installed - it's up to application code to install as necessary.
+ **libconin.asm** - A set of higher level console input routines that provide buffered input and also implement a simple 'history' function allowing applications to record and replay previous entered lines (see GETHIST/SETHIST). If enabled then the console code interprets VT100 up/down/left/right keys to edit the current input line.
+ **libconout.asm** - A set of higher level console output routines for printing text/numbers.
+ **libi2c.asm** - low level **I2C** interface library. Provides initialisation, read and write byte operations
+ **libspi.asm** - low level **SPI** interface library.
+ **librtc.asm** - uses the I2C interface to talk with the real time clock (RTC) chip. Routines allow read/write for time and to the available user non-volatile storage.
+ **libsdc.asm** - SDCard initialisation and block read/write functions. Limited right now to cards less than 2GB and only supports the first SDCard 'slot'.

## zloader

Not a very imaginative name but grew from the simplest original code to get *something* running on the hardware and evolved from there. Structure isn't great right now - very much a work in progress.

+ **loader.asm** - main entry code and implements many of the common functions
+ **commands.asm** - Pulled some functions from loader.asm. Starting to modularise the code.
+ **disassembler.asm** - Z80 disassembler that provides meta-data used by the debuger to set break points.
+ **flash.asm** - Old code that writes data to the first 16K of flash. Used before I had a PROM programmer. Obsolete now and not used but wanted to keep the code around to remind myself how to write the the flash pages.

## tools

General place to hold tools to run in a development environment or on a Raspberry Pi.
+ **download.js** - node.js command line tool that takes an Intel Hex format file and writes the contents to the Z80 board using the memio primitives.
+ **nastohex.js** - Simple node tool that takes a Nascom format hex file and converts it to Intel Hex format so it can be downloaded using the tool above.

## lib - _now obsolete_

In the early days, before I had a PROM programmer I users the Raspberry Pi GPIO lines to connect to the Z80 bus. Using the BUSRQ/BUSACK handshake the Pi could grab hold of the bus and write data directly into the boards RAM and then release the CPU. I click of the reset button would have the Z80 executing the downloaded code. For this to work the jumper mapping the SRAM to the low order pages is necessary. I developed the first versions of the current monitor using this technique and included code in the monitor that could then copy itself Flash.

Great starter but somewhat limiting so I don't use this any longer. The code is still here though if anyone is interested.

+ **memio.js** - node.js library for applications that run on the Raspberry Pi and drives the GPIO lines in a way that's compatible with the Z80 bus. Primitives allow a Bus Request/Bus Acknowledge cycle and then a set of byte write operations. Most of the interface is exported from the library in a way that the interface can be drived from an interactive Node shell, which is particularly useful for debugging.
+ **log.js** - simple debug logger that can be turned on or off

## Development

I find my Mac a much nicer environment for development than the Raspeberry Pi. I've yet or find a code editor I like on the Pi. On my Mac I have access to Atom, GitKraken and the other tools I like. **sync.sh** compiles changed files and rsync's the contents to the Pi. I run that with nodemon and it magically compiles and copies hex files to the Pi ready to be downloaded.

For development I use the block-level protocol (see *libcmd* above). nodemon assembles software being developed and transfers to the Pi, to **BO** command on the Pi loads and runs the code over serial port B using the block protocol.

