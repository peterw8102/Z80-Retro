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

## Next steps...

### Hardware

I've just designed an I/O card which is currently on it's way to me. This will provide a keyboard interface (using the Omega MSX heyboard), dual joysticks, a Z80 PIO interface and a SN76489 sound chip. The system will then comprise three cards and have all the functionality I probably need. It might be interesting to add a legacy floppy disk interface though...

### Core Operating Software

I'll be documenting intended improvements/features to the software using GitHub issues. Here's a summary of what I want to do:

+ Replace the IM0 mode interrupt model with IM2 and 'reserve' the top 512 bytes for ZLoader use.
+ Generalise the keyboard I/O so, with the Omega keyboard and video card, have the option of a completely standalone system like we had in the old days!
+ Provide 'virtual disks' that can be used by guest operating systems. The core of this is already there but it's not usable at the moment. Needs additions to the core and then utilities to make the mappings.
+ Make use of more of the hardware features

### Third party options

There are a couple of cools third party systems which would be interesting to port:

+ [Small Computer Monitor](https://smallcomputercentral.com/small-computer-monitor/) (SCM). This looks like a nice system with a reasonable hardware abstraction which should be moderately straightforward to port.
+ [RomWBW](https://github.com/wwarthen/RomWBW). A more feature rich solution that includes an HBIOS hardware abstraction. The abstraction relies on quite a lot of conditional assembly making it somewhat messy to put together and it's not going to be a quick port.
+ CP/M 3 - I've ported CP/M 2.2, version 3 doesn't really seem to offer many advantages.
+ [Fuzix](https://github.com/EtchedPixels/FUZIX) - I really like the idea of a Z80 Unix-like OS!

It appears a number of people have bought PCBs for this system from eBay (not sold by me or anyone associated with me!). I'm hoping one or more of those might port some software over. If it's you and you need help then get in touch!
