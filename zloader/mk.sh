#/bin/sh
set -x
pushd ../zlib
./mk.sh
popd
pushd ../zios
./mk.sh
popd

rm zout/*

AFLAGS="-j -J --rel7 --oo obj,lst"
INCLUDE="-I ../zlib -I ../zios "

if [ -z "$RELEASE" ]; then
  echo "Building DEVELOPMENT version of ZLoader"

  # In development mode also allow the option to not overwrite the flashed
  # version of ZIOS/ZLoader. This is useful in the case where you're
  # working on a new version of either, as opposed to a new version
  # if a client application.
  if [ -z "$DEBUG" ]; then
    echo "Overwrite live ZLoader with development version"
    AFLAGS+=" -DOVERWRITE"
  else
    echo "Development ZLoader will reside in pages 22 and 23"
    AFLAGS+=" -DDEBUG"
  fi
else
  echo "Building RELEASE version of ZLoader"
  AFLAGS+=" -DRELEASE"
fi

# Assemble the library modules to be relocatable.
zmac $INCLUDE $AFLAGS ./loader.asm
zmac $INCLUDE $AFLAGS ./disassembler.asm
zmac $INCLUDE $AFLAGS ./charset.asm
zmac $INCLUDE $AFLAGS ./breakpoint.asm
zmac $INCLUDE $AFLAGS ./sdaddr.asm
zmac $INCLUDE $AFLAGS ./sdutils.asm
zmac $INCLUDE $AFLAGS ./cmdflsh.asm
zmac $INCLUDE $AFLAGS ./cmddump.asm
zmac $INCLUDE $AFLAGS ./cmdload.asm
zmac $INCLUDE $AFLAGS ./cmddtime.asm
zmac $INCLUDE $AFLAGS ./cmdcfg.asm
zmac $INCLUDE $AFLAGS ./cmdpage.asm
zmac $INCLUDE $AFLAGS ./cmdexec.asm
zmac $INCLUDE $AFLAGS ./cmdhist.asm
zmac $INCLUDE $AFLAGS ./cmdregs.asm
zmac $INCLUDE $AFLAGS ./cmdsdimg.asm
zmac $INCLUDE $AFLAGS ./cmdsdboot.asm
zmac $INCLUDE $AFLAGS ./cmdsd.asm
zmac $INCLUDE $AFLAGS ./cmdtab.asm
zmac $INCLUDE $AFLAGS ./cmdsdmap.asm
zmac $INCLUDE $AFLAGS ./cmdsdwrite.asm
zmac $INCLUDE $AFLAGS ./cmdhw.asm
zmac $INCLUDE $AFLAGS ./cmdfill.asm
zmac $INCLUDE $AFLAGS ./cmdio.asm
zmac $INCLUDE $AFLAGS ./cmdmodify.asm
zmac $INCLUDE $AFLAGS ./cmdhelp.asm
zmac $INCLUDE $AFLAGS ./cmdcons.asm
zmac $INCLUDE $AFLAGS ./more.asm

ld80 -o ./loader.tmp -P 0040 -D 3400 -O ihex -s - -m -S 2048 \
        ./zout/loader.rel \
        ./zout/disassembler.rel \
        ./zout/breakpoint.rel \
        ./zout/charset.rel \
        ./zout/sdaddr.rel \
        ./zout/sdutils.rel \
        ./zout/cmdflsh.rel \
        ./zout/cmddump.rel \
        ./zout/cmdload.rel \
        ./zout/cmddtime.rel \
        ./zout/cmdcfg.rel \
        ./zout/cmdpage.rel \
        ./zout/cmdexec.rel \
        ./zout/cmdhist.rel \
        ./zout/cmdregs.rel \
        ./zout/cmdsdimg.rel \
        ./zout/cmdsdboot.rel \
        ./zout/cmdsd.rel \
        ./zout/cmdtab.rel \
        ./zout/cmdsdmap.rel \
        ./zout/cmdsdwrite.rel \
        ./zout/cmdhw.rel \
        ./zout/cmdfill.rel \
        ./zout/cmdio.rel \
        ./zout/cmdmodify.rel \
        ./zout/cmdhelp.rel \
        ./zout/cmdcons.rel \
        ./zout/more.rel \
        -P C600 -D C000 \
        ../zios/zout/pcb.rel \
        ../zlib/zout/libsio.rel \
        ../zios/zout/nvram.rel \
        ../zios/zout/init.rel \
        ../zios/zout/mempage.rel \
        ../zios/zout/drive.rel \
        ../zios/zout/vdu.rel \
        ../zios/zout/console.rel \
        ../zlib/zout/libutils.rel \
        ../zlib/zout/libcmd.rel \
        ../zlib/zout/libconin.rel \
        ../zlib/zout/libconout.rel \
        ../zlib/zout/libspi.rel \
        ../zlib/zout/libsdc.rel \
        ../zlib/zout/libi2c.rel \
        ../zlib/zout/librtc.rel \
        ../zlib/zout/libmisc.rel \
        ../zlib/zout/libflash.rel \
        ../zlib/zout/libkbd.rel \
        ../zlib/zout/libvdu.rel \
        ../zios/zout/services.rel \
        ../zios/zout/devmap.rel \
        ../zios/zout/sdblk.rel \
        ../zios/zout/process.rel \
        ../zios/zout/run.rel \
        -P FE00 \
        ../zios/zout/appl.rel \

node ../tools/hextform --fix --move=C000,FFFF,4000 loader.tmp > loader.hex
rm ./loader.tmp
