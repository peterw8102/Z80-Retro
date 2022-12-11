#/bin/sh
pushd ../zlib
./mk.sh
popd

# Assemble the library modules to be relocatable.
if [ -z "$RELEASE" ]; then
  echo "Building DEVELOPMENT version"
  zmac -j -J --rel7 --oo obj,lst ./services.asm
  zmac -j -J --rel7 --oo obj,lst ./loader.asm
  zmac -j -J --rel7 --oo obj,lst ./commands.asm
  zmac -j -J --rel7 --oo obj,lst ./disassembler.asm
  zmac -j -J --rel7 --oo obj,lst ./appl.asm
else
  echo "Building RELEASE version"
  zmac -DRELEASE -j -J --rel7 --oo obj,lst ./services.asm
  zmac -DRELEASE -j -J --rel7 --oo obj,lst ./loader.asm
  zmac -DRELEASE -j -J --rel7 --oo obj,lst ./commands.asm
  zmac -DRELEASE -j -J --rel7 --oo obj,lst ./disassembler.asm
  zmac -DRELEASE -j -J --rel7 --oo obj,lst ./appl.asm
fi

ld80 -o ./loader.hex -P 0100 -D 3000 -O ihex -s - -m ./zout/loader.rel ./zout/services.rel ./zout/commands.rel ./zout/disassembler.rel ./zout/appl.rel ../zlib/zout/libutils.rel ../zlib/zout/libsio.rel ../zlib/zout/libcmd.rel ../zlib/zout/libconin.rel ../zlib/zout/libconout.rel ../zlib/zout/libspi.rel ../zlib/zout/libsdc.rel ../zlib/zout/libi2c.rel ../zlib/zout/librtc.rel
