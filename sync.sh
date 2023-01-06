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

rsync -avz zloader/loader.hex pi@pi2:/home/pi/src
