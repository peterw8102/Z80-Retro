#/bin/sh

# Assemble the library modules to be relocatable.
zmac --rel7 --oo obj,lst ./ccp.asm
zmac --rel7 --oo obj,lst ./bdos.asm
zmac --rel7 --oo obj,lst ./bios.asm

ld80 -o ./cpm22.hex -P DB00 -O ihex -s - -m ./zout/ccp.rel ./zout/bdos.rel ./zout/bios.rel
