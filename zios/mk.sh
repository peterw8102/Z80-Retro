#/bin/sh
set -x

rm zout/*

AFLAGS="-j -J --rel7 --oo obj,lst "

if [ -z "$RELEASE" ]; then
  echo "Building DEVELOPMENT version of ZIOS"

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
  echo "Building RELEASE version of ZIOS"
  AFLAGS+=" -DRELEASE"
fi

zmac -I ../zlib $AFLAGS ./services.asm
zmac -I ../zlib $AFLAGS ./appl.asm
zmac -I ../zlib $AFLAGS ./drive.asm
zmac -I ../zlib $AFLAGS ./devmap.asm
zmac -I ../zlib $AFLAGS ./mempage.asm
zmac -I ../zlib $AFLAGS ./pcb.asm
zmac -I ../zlib $AFLAGS ./sdblk.asm
zmac -I ../zlib $AFLAGS ./nvram.asm
zmac -I ../zlib $AFLAGS ./init.asm
zmac -I ../zlib $AFLAGS ./process.asm
zmac -I ../zlib $AFLAGS ./run.asm
zmac -I ../zlib $AFLAGS ./vdu.asm
zmac -I ../zlib $AFLAGS ./console.asm
