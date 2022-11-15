# zloader

Not a very imaginative name but grew from the simplest original code to get *something* running on the hardware and evolved 
from there. Structure isn't great right now and I'm working on that. Most (way too much) functionality is in `loader.asm` right now.

+ **config.asm** - contains common definitions used across the loader
+ **loader.asm** - main entry code and implements many of the common functions
+ **commands.asm** - Pulled some functions from loader.asm. Starting to modularise the code
+ **disassembler.asm** - Z80 disassembler that provides meta-data used by the debuger to set break points.
+ **services.asm** - Provides an optional set of core 'services' that can be used by applications. Services are accessed via the RST 30h instruction with a command selector in the 'C' register.
+ **flash.asm** - Old code that writes data to the first 16K of flash. Used before I had a PROM programmer. Obsolete now and not used but wanted to keep the code around to remind myself how to write the the flash pages.

## Services
A few comments on the services. When code is loaded into the application space the loader can optionally install a services 'handler' accessed via the RST 30h instruction.

Services allow applications to make use of some core code without having to re-invent the wheel. This also saves space because the drivers for these services are held in the `zloader` memory page, outside of the applications 64K space.

Whether or not the service handler is installed in the application space is decided in one of two ways:

+ **Config parameter** - these are stored in the RTC NVRAM. Parameter `00` is a boolean. If true then before running application code the loader will install the service handler. The value can be changed using the `C` command on the console interface.
+ **SDCard configuration** - the loader stores metadata about applications stored on the SDCard. One of the parameters indicates whether that application requires the services to be installed.

The CP/M BIOS modifications make use of the `zloader` services to access the console and SDCards. This makes the BIOS significantly smaller than it would otherwise have been.
