#/bin/sh

# Utilities
zmac -j -J --rel7 --oo obj,lst ./libutils.asm

# SIO library
zmac -j -J --rel7 --oo obj,lst ./libsio.asm

# Command based block protocol to communicate over SIO Port B. I use this with a matching
# application running on a Raspberry Pi that accepts commands from the Z80 and repplies
# with requested data.
zmac -j -J --rel7 --oo obj,lst ./libcmd.asm

# Console input functions
zmac -j -J --rel7 --oo obj,lst ./libconin.asm

# Console input and line processing functions
zmac -j -J --rel7 --oo obj,lst ./libconout.asm

# i2c driver
zmac -j -J --rel7 --oo obj,lst ./libi2c.asm

# SPI driver
zmac -j -J --rel7 --oo obj,lst ./libspi.asm

# Real time clock driver
zmac -j -J --rel7 --oo obj,lst ./librtc.asm

# SDCard library
zmac -j -J --rel7 --oo obj,lst ./libsdc.asm

# Flash
zmac -j -J --rel7 --oo obj,lst ./libflash.asm

# Miscelaneous hardware
zmac -j -J --rel7 --oo obj,lst ./libmisc.asm
