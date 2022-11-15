# Z80-Retro
Tools and code snippets I'm playing with while building my own Z80 SBC

## zlib

A set of library routines to drive the various hardware elements of the board. These are 'included' in the zloader monitor programme. See the directory itself for details.

## zloader

Not a very imaginative name but grew from the simplest original code to get *something* running on the hardware and evolved from there. Structure isn't great right now - very much a work in progress. See the `README.md` in the zloader directory for details.

## tools

General place to hold tools to run in a development environment or on a Raspberry Pi.
+ **download.js** - node.js command line tool that takes an Intel Hex format file and writes the contents to the Z80 board using the memio primitives.
+ **nastohex.js** - Simple node tool that takes a Nascom format hex file and converts it to Intel Hex format so it can be downloaded using the tool above.

## lib - _now obsolete_

In the early days, before I had a PROM programmer I used the Raspberry Pi GPIO lines to connect to the Z80 bus. Using the BUSRQ/BUSACK handshake the Pi could grab hold of the bus and write data directly into the boards RAM and then release the CPU. A click of the reset button would have the Z80 executing the downloaded code. For this to work the jumper mapping the SRAM to the low order pages is necessary. I developed the first versions of the current monitor using this technique and included code in the monitor that could then copy itself Flash.

Great starter but somewhat limiting so I don't use this any longer. The code is still here though if anyone is interested.

+ **memio.js** - node.js library for applications that run on the Raspberry Pi and drives the GPIO lines in a way that's compatible with the Z80 bus. Primitives allow a Bus Request/Bus Acknowledge cycle and then a set of byte write operations. Most of the interface is exported from the library in a way that the interface can be driven from an interactive Node shell, which is particularly useful for debugging.
+ **log.js** - simple debug logger that can be turned on or off

## Development

I find my Mac a much nicer environment for development than the Raspeberry Pi. I've yet or find a code editor I like on the Pi. On my Mac I have access to Atom, GitKraken and the other tools I like. **sync.sh** compiles changed files and rsync's the contents to the Pi. I run that with nodemon and it magically compiles and copies hex files to the Pi ready to be downloaded.

For development I use a block-level protocol (see *libcmd* above). `nodemon` on my Mac assembles software being developed and transfers to the Pi, the zloader **BO** command on the Z80 board loads and runs the code over serial port B using the block protocol.

