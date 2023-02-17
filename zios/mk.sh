#/bin/sh

# Assemble the library modules to be relocatable.
if [ -z "$RELEASE" ]; then
  echo "Building DEVELOPMENT version"
  zmac -j -J --rel7 --oo obj,lst ./services.asm
  zmac -j -J --rel7 --oo obj,lst ./appl.asm
  zmac -j -J --rel7 --oo obj,lst ./drive.asm
  zmac -j -J --rel7 --oo obj,lst ./devmap.asm
  zmac -j -J --rel7 --oo obj,lst ./mempage.asm
  zmac -j -J --rel7 --oo obj,lst ./pcb.asm
  zmac -j -J --rel7 --oo obj,lst ./sdblk.asm
  zmac -j -J --rel7 --oo obj,lst ./nvram.asm
  zmac -j -J --rel7 --oo obj,lst ./init.asm
  zmac -j -J --rel7 --oo obj,lst ./process.asm
else
  echo "Building RELEASE version"
  zmac -DRELEASE -j -J --rel7 --oo obj,lst ./services.asm
  zmac -DRELEASE -j -J --rel7 --oo obj,lst ./appl.asm
  zmac -DRELEASE -j -J --rel7 --oo obj,lst ./drive.asm
  zmac -DRELEASE -j -J --rel7 --oo obj,lst ./devmap.asm
  zmac -DRELEASE -j -J --rel7 --oo obj,lst ./mempage.asm
  zmac -DRELEASE -j -J --rel7 --oo obj,lst ./pcb.asm
  zmac -DRELEASE -j -J --rel7 --oo obj,lst ./sdblk.asm
  zmac -DRELEASE -j -J --rel7 --oo obj,lst ./nvram.asm
  zmac -DRELEASE -j -J --rel7 --oo obj,lst ./init.asm
  zmac -DRELEASE -j -J --rel7 --oo obj,lst ./process.asm
fi
