# tools

General place to hold tools to run in a development environment or on a Raspberry Pi.

+ **block.js** - Implements the Node/Raspberry Pi end of the block protocol used to download development applications to the Z80 application memory. See below for more information.
+ **bin2hex.js** - Convert a pure binary file into an Intel hex file. Takes a file nane and a start address as parameters.
+ **nastohex.js** - Simple node tool that takes a Nascom format hex file and converts it to Intel Hex format so it can be downloaded using the tool above.

## block.js

My standard way of developing code for the Z80 is to:

1 assemble on my Max (using the `zmac` assembler and `ld` linker
2 `rsync` the resulting hex file to my Raspberry Pi
3 use the `BO` command on the Z80 `zloader` to load the binary file down to the Z80 memory space and run it

By using `nodemon` to automatically run steps 1 and 2 whenever a file is saved, development becomes almost instantaneous. 

To transfer the code from the Raspberry Pi to the Z80 there's a really simple block based protocol.

The Pi end of this protocol is implemented in `block.js` in this directory. I generally have this set up to run from when the Pi boots.

The Z80 end of the protocol is implemented in `zlib/libcmd.asm` and that file should be referenced for details of the protocol.
