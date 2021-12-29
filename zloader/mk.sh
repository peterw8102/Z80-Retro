#/bin/sh
pushd ../zlib
./mk.sh
popd

# Assemble the library modules to be relocatable.
zmac -j -J --rel7 --oo obj,lst ./loader.asm
zmac -j -J --rel7 --oo obj,lst ./commands.asm
zmac -j -J --rel7 --oo obj,lst ./disassembler.asm

ld80 -o ./loader.hex -P 0100 -D 3000 -C LNBUF,3800 -O ihex -s - -m ./zout/loader.rel ./zout/commands.rel ./zout/disassembler.rel ../zlib/zout/libsio.rel ../zlib/zout/libcmd.rel ../zlib/zout/libconsole.rel ../zlib/zout/libspi.rel ../zlib/zout/libsdc.rel ../zlib/zout/libi2c.rel ../zlib/zout/librtc.rel