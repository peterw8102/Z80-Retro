#/bin/sh

# Assemble the library modules to be relocatable.
zmac -j -J --rel7 --oo obj,lst ./libsio.asm
zmac -j -J --rel7 --oo obj,lst ./libcmd.asm
zmac -j -J --rel7 --oo obj,lst ./libconsole.asm
zmac -j -J --rel7 --oo obj,lst ./libi2c.asm
zmac -j -J --rel7 --oo obj,lst ./librtc.asm
zmac -j -J --rel7 --oo obj,lst ./libspi.asm
zmac -j -J --rel7 --oo obj,lst ./libsdc.asm
# ld80 -o ./monitor.hex -P 008D -D 1800 -O ihex -s - -m zout/libsio.rel zout/main.rel zout/disassembler.rel zout/libconsole.rel
