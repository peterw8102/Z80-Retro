# CP/M 2.2

This directory contains my port of CP/M to my Retro80 computer.

+ `cpm22.asm` is the combined CCP and BDOS. Essentially unchanged from the original Digital Research version.
+ `bios.asm` is my heavily customised BIOS. Most of the actual I/O is delegated to my ZIOS core system which is accessed via RST 30h.

Low level disk management is implemented by ZIOS which supports 1023 virtual 'disks' to be hosted on a single SDCard. Each virtual disk is 4MB is size. Any of the virtual disks can be mounted on one of the four logical drives in CP/M: A-D.

A `MOUNT.COM` CP/M programme can change be used to change which disks are mounted onto which drives.
