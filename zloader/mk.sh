#/bin/sh

# Build script
# This script will build one of three variants of the zloader system:
#   1. RELEASE:     A version ready (and located) to be written to onboard Flash.
#                   This also creates a ROM file that can be used with the
#                   emulator.
#   2. DEBUG:       A version build to run in RAM pages 22 and 23 in theory allowing
#                it to be debugged with live version in pages 20 and 21.
#   3. DEVELOPMENT: When loaded, overwrites the running copy in pages 20 and 21.
#
# By default './mk.sh' builds the DEVELOPMENT variant.
# To build the RELEASE version, set the RELEASE environment variable:
#   RELEASE=1 ./mk.sh
#
# To build the DEBUG version, set the DEBUG environment variable:
#   DEBUG=1 ./mk.sh
#
# The script should be executed from within the 'zloader' directory. The
# final output is written to the ''../image' directory. The output filenames
# will identify the variant built and will be in the form:
#   zloader.version.variant.ext
#
# Example:
#   images/zloader.2.1.5.rel.hex
#
# The script will build the zios and zlib libraries before building the zloader
# files in this directory.

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

# This should ve kept in sync with the version number string in loader.asm
VERS="2.1.5"

BUILD="dev"

if [ -z "$RELEASE" ]; then
  echo "Building DEVELOPMENT version of ZLoader"

  # In development mode also allow the option to not overwrite the flashed
  # version of ZIOS/ZLoader. This is useful in the case where you're
  # working on a new version of either, as opposed to a new version
  # if a client application.
  if [ -z "$DEBUG" ]; then
    echo "Overwrite live ZLoader with development version"
    AFLAGS+=" -DOVERWRITE"
		BUILD="dbg"
  else
    echo "Development ZLoader will reside in pages 22 and 23"
    AFLAGS+=" -DDEBUG"
  fi
else
  echo "Building RELEASE version of ZLoader"
  AFLAGS+=" -DRELEASE -DOVERWRITE"
	BUILD="rel"
fi

TARGET=zloader.$VERS.$BUILD
echo "Target: "$TARGET

# Assemble the library modules to be relocatable.
zmac $INCLUDE $AFLAGS ./loader.asm
zmac $INCLUDE $AFLAGS ./disassembler.asm
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
zmac $INCLUDE $AFLAGS ./cmdkbd.asm
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

ld80 -o ./loader.tmp -P 0068 -D 3400 -O ihex -s - -m -S 2048 \
        ./zout/loader.rel \
        ./zout/disassembler.rel \
        ./zout/breakpoint.rel \
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
        ./zout/cmdkbd.rel \
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
        -P C500 -D C000 \
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


node ../tools/hextform --fix --report --move=C000,FFFF,4000 loader.tmp > ../images/$TARGET.hex
rm ./loader.tmp

if [ -n "$RELEASE" ]; then
  echo "Building Emulator ROM"

  # Create a ROM binary image file for the emulator.
  hex2bin -l 524288 ../images/$TARGET.hex

  # Binary image into a disk image
  dd if=../images/$TARGET.bin of=../images/$TARGET.rom bs=524288 conv=sync

  # Tidyup
  rm ../images/$TARGET.bin
fi
