#/bin/sh
# Assemble the library modules to be relocatable.
zmac -j -J --rel7 --oo obj,lst ./video.asm

ld80 -o ./video.hex -P 01000 -D 3000 -O ihex -s - -m ./zout/video.rel

# rsync -avz ./video.hex pi@pi2:/home/pi/src/
