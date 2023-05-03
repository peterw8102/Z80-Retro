#/bin/sh

# Assemble the library modules to be relocatable.
zmac -j --rel7 --oo obj,lst ./ccp.asm

# Alternate command processors
# zmac -j --rel7 --oo obj,lst ./zcpr.asm
# zmac -j --rel7 --oo obj,lst ./z80ccp.asm

zmac -j --rel7 --oo obj,lst ./bdos.asm

# Alternate BDOS implementations. Haven't got ZSDOS to work so far.
# zmac -j --rel7 --oo obj,lst ./zsdos.z80
zmac -j -I ../../zios --rel7 --oo obj,lst ./bios.asm

ld80 -o ./cpm22.hex -P DD00 -O ihex -s - -m ./zout/ccp.rel ./zout/bdos.rel ./zout/bios.rel

# And placeholders for building alternative CP/M-like systems
# ld80 -o ./zcpr.hex -P DD00 -O ihex -s - -m ./zout/zcpr.rel ./zout/bdos.rel ./zout/bios.rel
# ld80 -o ./zsdos.hex -P DD00 -O ihex -s - -m ./zout/zcpr.rel ./zout/zsdos.rel ./zout/bios.rel
# ld80 -o ./z80ccp.hex -P DD00 -O ihex -s - -m ./zout/z80ccp.rel ./zout/bdos.rel ./zout/bios.rel
