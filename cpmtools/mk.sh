#/bin/sh
pushd ../zlib
./mk.sh
popd

zmac --rel7 --oo obj,lst ./download.asm
# zmac --rel7 --oo obj,lst ./copysys.asm
zmac -I ../zios --rel7 -j --oo obj,lst ./mount.asm
zmac --zmac -I ../zios -j --rel7 --oo obj,lst ./dstats.asm

ld80 -o ./download.hex -O ihex -s - -m ./zout/download.rel
# ld80 -o ./copysys.hex -O ihex -s - -m ./zout/copysys.rel
ld80 -o ./dstats.hex -O ihex -s - -m ./zout/dstats.rel
ld80 -o ./mount.hex -O ihex -s - -m ./zout/mount.rel

# Convert mount.hex to something suitable for CP/M DOWNLOAD.COM
node ../tools/bin2cpmhex.js mount.hex > mount.cpm
