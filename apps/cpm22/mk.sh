#/bin/sh

# Assemble the library modules to be relocatable.
zmac --rel7 --oo obj,lst ./cpm22.asm
zmac --rel7 --oo obj,lst ./bios.asm

ld80 -o ./bios.hex -P D900 -O ihex -s - -m ./zout/cpm22.rel ./zout/bios.rel
