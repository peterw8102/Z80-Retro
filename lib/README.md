# lib - _now obsolete_

In the early days, before I had a PROM programmer I used the Raspberry Pi GPIO lines to connect to the Z80 bus. Using the BUSRQ/BUSACK handshake the Pi could grab hold of the bus and write data directly into the boards RAM and then release the CPU. A click of the reset button would have the Z80 executing the downloaded code. For this to work the jumper mapping the SRAM to the low order pages is necessary. I developed the first versions of the current monitor using this technique and included code in the monitor that could then copy itself Flash.

Great starter but somewhat limiting so I don't use this any longer. The code is still here though if anyone is interested.

+ **memio.js** - node.js library for applications that run on the Raspberry Pi and drives the GPIO lines in a way that's compatible with the Z80 bus. Primitives allow a Bus Request/Bus Acknowledge cycle and then a set of byte write operations. Most of the interface is exported from the library in a way that the interface can be driven from an interactive Node shell, which is particularly useful for debugging.
+ **log.js** - simple debug logger that can be turned on or off

For reference the following Raspberry Pi GPIO lines were connected to the Z80 bus via a set of tr-state buffers. The interface was write only, the Pi could only write data to the Z80 memory.

|Z80 Bus|Pi Pin|Pi pin name|
|-------|------|-----------|
|  RD   |   3  |    BCM2   |
|  WR   |   5  |    BCM3   |
| BUSRQ |   8  |    BCM14  |
| BUSAK |  10  |    BCM14  |
|  D0   |  16  |    BCM23  |
|  D1   |  22  |    BCM25  |
|  D2   |  26  |    BCM7   |
|  D3   |  32  |    BCM12  |
|  D4   |  12  |    BCM18  |
|  D5   |  18  |    BCM24  |
|  D6   |  24  |    BCM8   |
|  D7   |  28  |    BCM1   |
|  A0   |  38  |    BCM20  |
|  A1   |  37  |    BCM26  |
|  A2   |  29  |    BCM5   |
|  A3   |  33  |    BCM13  |
|  A4   |  36  |    BCM16  |
|  A5   |  40  |    BCM21  |
|  A6   |  35  |    BCM35  |
|  A7   |  31  |    BCM6   |
|  A8   |  11  |    BCM17  |
|  A9   |  15  |    BCM22  |
|  A10  |  21  |    BCM9   |
|  A11  |  27  |    BCM27  |
|  A12  |   7  |    BCM4   |
|  A13  |  13  |    BCM27  |
|  A14  |  19  |    BCM10  |
|  A15  |  23  |    BCM11  |
