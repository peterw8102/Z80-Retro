#/bin/sh

# Assemble the library modules to be relocatable.
if [ -z "$RELEASE" ]; then
  echo "Building DEVELOPMENT version"
  zmac -I ../zlib -j -J --rel7 --oo obj,lst ./services.asm
  zmac -I ../zlib -j -J --rel7 --oo obj,lst ./appl.asm
  zmac -I ../zlib -j -J --rel7 --oo obj,lst ./drive.asm
  zmac -I ../zlib -j -J --rel7 --oo obj,lst ./devmap.asm
  zmac -I ../zlib -j -J --rel7 --oo obj,lst ./mempage.asm
  zmac -I ../zlib -j -J --rel7 --oo obj,lst ./pcb.asm
  zmac -I ../zlib -j -J --rel7 --oo obj,lst ./sdblk.asm
  zmac -I ../zlib -j -J --rel7 --oo obj,lst ./nvram.asm
  zmac -I ../zlib -j -J --rel7 --oo obj,lst ./init.asm
  zmac -I ../zlib -j -J --rel7 --oo obj,lst ./process.asm
  zmac -I ../zlib -j -J --rel7 --oo obj,lst ./run.asm
else
  echo "Building RELEASE version"
  zmac -I ../zlib -DRELEASE -j -J --rel7 --oo obj,lst ./services.asm
  zmac -I ../zlib -DRELEASE -j -J --rel7 --oo obj,lst ./appl.asm
  zmac -I ../zlib -DRELEASE -j -J --rel7 --oo obj,lst ./drive.asm
  zmac -I ../zlib -DRELEASE -j -J --rel7 --oo obj,lst ./devmap.asm
  zmac -I ../zlib -DRELEASE -j -J --rel7 --oo obj,lst ./mempage.asm
  zmac -I ../zlib -DRELEASE -j -J --rel7 --oo obj,lst ./pcb.asm
  zmac -I ../zlib -DRELEASE -j -J --rel7 --oo obj,lst ./sdblk.asm
  zmac -I ../zlib -DRELEASE -j -J --rel7 --oo obj,lst ./nvram.asm
  zmac -I ../zlib -DRELEASE -j -J --rel7 --oo obj,lst ./init.asm
  zmac -I ../zlib -DRELEASE -j -J --rel7 --oo obj,lst ./process.asm
  zmac -I ../zlib -DRELEASE -j -J --rel7 --oo obj,lst ./run.asm
fi
