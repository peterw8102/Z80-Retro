#/bin/sh
pushd ../../zlib
./mk.sh
popd

# Assemble the library modules to be relocatable.
zmac -I ../../zlib -I ../../zios -j -J --rel7 --oo obj,lst ./leds.asm

ld80 -o ./leds.hex   \
     -P 6000 -D 8000    \
     -O ihex            \
     -s -               \
     -m ./zout/leds.rel \
        ../../zlib/zout/libspi.rel \
        ../../zlib/zout/libutils.rel
