#/bin/sh
pushd ../zlib
./mk.sh
popd
pushd ../zios
./mk.sh
popd

# Assemble the library modules to be relocatable.
if [ -z "$RELEASE" ]; then
  echo "Building DEVELOPMENT version"
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./loader.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./commands.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./disassembler.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./dump.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./load.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./datetime.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./cfg.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./page.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./exec.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./regs.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./sdutils.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./sdimg.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./sdboot.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./sdcmds.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./cmdtab.asm
  zmac -I ../zlib -I ../zios -j -J --rel7 --oo obj,lst ./more.asm
else
  echo "Building RELEASE version"
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./loader.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./commands.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./disassembler.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./dump.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./load.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./datetime.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./cfg.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./page.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./exec.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./regs.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./sdutils.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./sdimg.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./sdboot.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./sdcmds.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./cmdtab.asm
  zmac -I ../zlib -I ../zios -DRELEASE -j -J --rel7 --oo obj,lst ./more.asm
fi

ld80 -o ./loader.tmp -P 0040 -D 3400 -O ihex -s - -m -S 2048 \
        ./zout/loader.rel \
        ./zout/commands.rel \
        ./zout/disassembler.rel \
        ./zout/dump.rel \
        ./zout/load.rel \
        ./zout/datetime.rel \
        ./zout/cfg.rel \
        ./zout/page.rel \
        ./zout/exec.rel \
        ./zout/regs.rel \
        ./zout/sdutils.rel \
        ./zout/sdboot.rel \
        ./zout/sdcmds.rel \
        ./zout/sdimg.rel \
        ./zout/cmdtab.rel \
        ./zout/more.rel \
        -P C400 -D C000 \
        ../zlib/zout/libsio.rel \
        ../zios/zout/pcb.rel \
        ../zios/zout/nvram.rel \
        ../zios/zout/init.rel \
        ../zios/zout/mempage.rel \
        ../zios/zout/drive.rel \
        ../zlib/zout/libutils.rel \
        ../zlib/zout/libcmd.rel \
        ../zlib/zout/libconin.rel \
        ../zlib/zout/libconout.rel \
        ../zlib/zout/libspi.rel \
        ../zlib/zout/libsdc.rel \
        ../zlib/zout/libi2c.rel \
        ../zlib/zout/librtc.rel \
        ../zlib/zout/libmisc.rel \
        ../zios/zout/services.rel \
        ../zios/zout/devmap.rel \
        ../zios/zout/sdblk.rel \
        ../zios/zout/process.rel \
        ../zios/zout/run.rel \
        -P FE00 \
        ../zios/zout/appl.rel \

node ../tools/hextform --fix --move=C000,FFFF,4000 loader.tmp > loader.hex
rm ./loader.tmp
