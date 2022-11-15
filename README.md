# Z80-Retro
Tools and code snippets I'm playing with while building my own Z80 SBC

## zlib

A set of library routines to drive the various hardware elements of the board. These are 'included' in the zloader monitor programme. See the directory itself for details.

## zloader

Not a very imaginative name but grew from the simplest original code to get *something* running on the hardware and evolved from there. Structure isn't great right now - very much a work in progress. See the `README.md` in the zloader directory for details.

## tools

General place to hold tools to run in a development environment or on a Raspberry Pi.

## lib - _now obsolete_

In the early days, before I had a PROM programmer I used the Raspberry Pi GPIO lines to connect to the Z80 bus. Using the BUSRQ/BUSACK handshake the Pi could grab hold of the bus and write data directly into the boards RAM and then release the CPU. Obsolete now but may be of interest to others that want to bootstrap a Z80 without non-volatile programme storage. See directory for more details.

## Development

I find my Mac a much nicer environment for development than the Raspeberry Pi. I've yet or find a code editor I like on the Pi. On my Mac I have access to Atom, GitKraken and the other tools I like. **sync.sh** compiles changed files and rsync's the contents to the Pi. I run that with nodemon and it magically compiles and copies hex files to the Pi ready to be downloaded.

For development I use a block-level protocol (see *libcmd* above). `nodemon` on my Mac assembles software being developed and transfers to the Pi, the zloader **BO** command on the Z80 board loads and runs the code over serial port B using the block protocol.

