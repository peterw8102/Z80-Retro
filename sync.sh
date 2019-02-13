#/bin/sh
echo "Hello..."

# copy process.js and the memio.js library to pi
# scp ./process.js pi@pi:/home/pi/src
# scp ./memio.js pi@pi:/home/pi/src

# Compile the Z80 assembler test file
echo "Assember..."

cd mon2
./mk.sh
cd ..

#zmac --oo hex,lst ./main3.asm

cd samples
# z80asm -l -b test.asm
z80asm -l -b test-slow.asm
z80asm -l -b sio.asm

/Users/pete/tasm/tasm -b -g0 -a -e -f00 test.asm test.ihx
# /Users/pete/tasm/tasm -b -g0 -a -e -f00 basic.asm basic.ihx
/Users/pete/tasm/tasm -b -g0 -a -e -f00 bas56k.asm basic.ihx
# /Users/pete/tasm/tasm -g0 -e -a4 loader.asm loader.ihx
/Users/pete/tasm/tasm -b -g0 -a -e -fff siolib.asm siolib.ihx
/Users/pete/tasm/tasm -b -g0 -a -e siolib2.asm siolib2.ihx
/Users/pete/tasm/tasm -b -g0 -a -e -fff sio2.asm sio2.ihx
/Users/pete/tasm/tasm -b -g0 -a -e -fff sio3.asm sio3.ihx

echo "Generate output"
# appmake +hex -b test.bin --org=8192
appmake +hex -b test-slow.bin --org=0
appmake +hex -b sio.bin --org=0
cd ..


cd zlib
./mk.sh
cd ..

cd zloader
./mk.sh
cd ..


# and any HEX files we have
# scp ./*.ihx  pi@pi:/home/pi/src
echo Syncronise files with Raspberry Pi
echo ----------------------------------
# rsync -avz "monitor/zout/*.hex" samples lib tools pi@pi:/home/pi/src
rsync -avz lib tools zloader pi@pi2:/home/pi/src
# rsync -avz "./monitor/zout/" pi@pi:/home/pi/src
rsync -avz "./mon2/monitor.hex" pi@pi2:/home/pi/src
rsync -avz "./samples" pi@pi2:/home/pi/src
