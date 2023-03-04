#/bin/sh

# Compile the Z80 assembler test file
echo "Assembling..."

cd zlib
./mk.sh
cd ..

cd zloader
# RELEASE=1 ./mk.sh
./mk.sh
cd ..


# and any HEX files we have
# scp ./*.ihx  pi@pi:/home/pi/src
echo Syncronise files with Raspberry Pi
echo ----------------------------------

if [ -z "$RELEASE" ]; then
  echo "Copying DEVELOPMENT version to Raspberry Pi"
  rsync -avz zloader/loader.hex pi@pi2:/home/pi/src
else
  echo "Packaging RELEASE build into 'images/loader.hex'"
  # node ./tools/hextform --fix zloader/loader.hex > images/loader.tmp
  node ./tools/hextform --fix zloader/loader.hex images/charset.hex > images/zloader.hex
  rsync -avz zloader/loader.hex pi@pi2:/home/pi/src
fi
