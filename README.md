# Z80-Retro
Tools and code snippets I'm playing with while building my own [Z80 single board computer (SBC)](https://oshwlab.com/peterw8102/simple-z80) and the separate [VGA text video card](https://oshwlab.com/peterw8102/8bit-video-card).

**Please refer to the [Z80-Retro Wiki](https://github.com/peterw8102/Z80-Retro/wiki) for more detail.**

## Toolchain

I evaluated a few Z80 assemblers/loaders and settled on the `zmac` [assembler](http://48k.ca/zmac.html) and `ld80` [linker](http://48k.ca/ld80.html). These aren't perfect but at much more flexible than the other available options and the author is available on Facebook and is very responsive to questions and fixes. This is a cross-assembler which I run on my Apple Mac and at the moment I'm using:

|Programme|Version|Link|
|---------|-------|----|
|zmac|18oct2022|http://48k.ca/zmac.html|
|ld80|0.7|http://48k.ca/ld80.html|

## Images

Pre-assembled images will be provided in a separate directory for those that don't want to build the code themselves.

## zlib

A set of library routines to drive the various hardware elements of the board. These are 'included' in the zloader monitor programme. See the directory itself for details.

There are a very good source of information for driving various aspects of the hardware.

## zloader

Not a very imaginative name but grew from the simplest original code to get *something* running on the hardware and evolved from there. Structure isn't great right now - very much a work in progress. See the `README.md` in the zloader directory for details.

## tools

General place to hold tools to run in a development environment or on a Raspberry Pi.

## Development

I find my Mac a much nicer environment for development than the Raspeberry Pi. I've yet or find a code editor I like on the Pi. On my Mac I have access to Atom, GitKraken and the other tools I like. **sync.sh** compiles changed files and rsync's the contents to the Pi. I run that with nodemon and it magically compiles and copies hex files to the Pi ready to be downloaded.

For development I use a block-level protocol (see *libcmd* above). `nodemon` on my Mac assembles software being developed and transfers to the Pi, the zloader **BO** command on the Z80 board loads and runs the code over serial port B using the block protocol.

