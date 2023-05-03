This directory will include prebuilt flash and other images.

## zloader.hex

This file includes the basic ROM image. Current contents are:

|Address|Description|
|-------|-----------|
|0-3FFF | The `zloader` image |
|8000-8FFF | A default character set (8x16 pixels per character). Used if you have the video card. |

This can be flashed to the 512KB flash device on the main CPU board and will run the monitor on reset.

**Note:** zloader expects a standard board configuration (including default jumper settings). Read the [Wiki](https://github.com/peterw8102/Z80-Retro/wiki/Hardware-Jumpers) to ensure you have the board set up correctly.

## cpm22.hex

This is a standard CP/M 2.2 build using the ZLoader services BIOS linked with the original BDOS and CCP. Code is in `apps/cpm22`.

Write this to a virtual disk with the ZLoader command:

```
wi 0:A dd00 fe00
```
See [the CP/M installation instructions for details](Installing-CPM).
