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
  zmac -j -J --rel7 --oo obj,lst ./drive.asm
  zmac -j -J --rel7 --oo obj,lst ./devmap.asm
  zmac -j -J --rel7 --oo obj,lst ./mempage.asm
  zmac -j -J --rel7 --oo obj,lst ./pcb.asm
  zmac -j -J --rel7 --oo obj,lst ./sdblk.asm
else
  echo "Building RELEASE version"
  zmac -DRELEASE -j -J --rel7 --oo obj,lst ./services.asm
  zmac -DRELEASE -j -J --rel7 --oo obj,lst ./loader.asm
  zmac -DRELEASE -j -J --rel7 --oo obj,lst ./commands.asm
  zmac -DRELEASE -j -J --rel7 --oo obj,lst ./disassembler.asm
  zmac -DRELEASE -j -J --rel7 --oo obj,lst ./appl.asm
  zmac -DRELEASE -j -J --rel7 --oo obj,lst ./drive.asm
  zmac -DRELEASE -j -J --rel7 --oo obj,lst ./devmap.asm
  zmac -DRELEASE -j -J --rel7 --oo obj,lst ./mempage.asm
  zmac -DRELEASE -j -J --rel7 --oo obj,lst ./pcb.asm
  zmac -DRELEASE -j -J --rel7 --oo obj,lst ./sdblk.asm
fi

ld80 -o ./loader.hex -P 0040 -D 3400 -O ihex -s - \
     -m ./zout/loader.rel \
        ./zout/drive.rel \
        ./zout/commands.rel \
        ./zout/appl.rel \
        ./zout/mempage.rel \
        ./zout/pcb.rel \
        ../zlib/zout/libutils.rel \
        ../zlib/zout/libsio.rel \
        ../zlib/zout/libcmd.rel \
        ../zlib/zout/libconin.rel \
        ../zlib/zout/libconout.rel \
        ../zlib/zout/libspi.rel \
        ../zlib/zout/libsdc.rel \
        ../zlib/zout/libi2c.rel \
        ../zlib/zout/librtc.rel \
        ../zlib/zout/libmisc.rel \
        ./zout/services.rel \
        ./zout/devmap.rel \
        ./zout/disassembler.rel \
        ./zout/sdblk.rel \
