# Z80-Utilities
Tools and code snippets I'm playing with while building my own Z80 SBC

## lib

+ **memio.js** - node.js library for applications that run on the Raspberry Pi and drives the GPIO lines in a way that's compatible with the Z80 bus. Primitives allow a Bus Request/Bus Acknowledge cycle and then a set of byte write operations. Most of the interface is exported from the library in a way that the interface can be drived from an interactive Node shell, which is particularly useful for debugging.
+ **log.js** - simple debug logger that can be turned on or off

## tools

+ **download.js** - node.js command line tool that takes an Intel Hex format file and writes the contents to the Z80 board using the memio primitives.
+ **nastohex.js** - Simple node tool that takes a Nascom format hex file and converts it to Intel Hex format so it can be downloaded using the tool above.

## Development

I find my Mac a much nicer environment for development than the Raspeberry Pi. I've yet or find a code editor I like on the Pi. On my Mac I have access to Atom, GitKraken and the other tools I like. **sync.sh** compiles changed files and rsync's the contents to the Pi. I run that with nodemon and it magically compiles and copies hex files to the Pi ready to be downloaded.
