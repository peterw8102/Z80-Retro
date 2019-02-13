#/bin/sh

# Assemble the library modules to be relocatable.
zmac -j -J --rel7 --oo obj,lst ./loader.asm

ld80 -o ./loader.hex -P 008D -D 1800 -O ihex -s - -m ../zlib/zout/libsio.rel ../zlib/zout/libconsole.rel ./zout/loader.rel
