/** @module memio
 *  @description Memory I/O. Uses the Raspberry Pi GPIO lines to drive the Z80 bus. The
 *  library provides support for 'busreq'/'busack' handshaking with the Z*) to grab control
 *  of the bus. This uses every available GPIO line on the 40 pin connector. It uses the pigio
 *  library to drive the pins. Has to be run as root.
 *
 *  NOTE: This library only provides write functionality - it's not possible to read from
 *  memory on the Z80 SBC. Mainly because I didn't want to go through all the level shifting that
 *  would involve. I used a bank of LEDs on the breadboard to show the contents of memory.
 */
const log = require('./log')('IO');

const GpioMod   = require('pigpio');
const Gpio      = GpioMod.Gpio;
const GpioBank  = GpioMod.GpioBank;

const delay = 1000;

/* map: provides a mapping of a Z80 data or address line to a specific GPIO line. This
 * doesn't include maps for the control lines. This allows a 24 bit value (AAAADD) to
 * be translated into a 32 bit mask used for controlling the Pi lines.
 */
var map = [
  23, // D0
  25,
   7,
  12,
  18,
  24,
   8,
   1, // D7

  20, // A0
  26,
   5,
  13,
  16,
  21,
  19,
   6,  // A7

  17,  // A8
  22,
   9,
   0,
   4,
  27,
  10,
  11,  // A15
];

/** The GPIO numbers in the table above are user friendly GPIO labels. They
 *  refer to the bit number in the mask we want to create. Convert the array
 *  to one containing a 32 bit into with the appropriate bit set.
 */
map = map.map((x) => 1<<x);

/** @function mapValue
 *  @description Given a 24 bit Z80 space number convert it to a set of GPIO
 *  pins that need to be set.
 */
function mapValue(x) {
  var val = 0, mask=1;
  for (var bit=0;bit<24;bit++) {
    if (x&mask)
      val |= map[bit];
    mask <<= 1;
  }
  return val;
}
var limitMask = mapValue(0xffffff);

/** We want to talk to some pins directly so create an array of Gpio
 *  controller objects for all pins that we can pick from.
 */
var pins = new Array(32), apin;
for (var i=0;i<32;i++) {
  if (i!=15)
    apin = new Gpio(i, {mode: Gpio.OUTPUT});
  pins[i] = apin;
}
// Pin 15 is the ONLY pin we use for input - it's for the BUSACK from the Z80.
pins[15] = new Gpio(15, {mode: Gpio.INPUT});

/** Grab hold of the GPIO pins as a block so we can write to them as a parallel
 *  bus-like way.
 */
const bank = new GpioBank(GpioMod.BANK1);

// Read and Write lines
const writeBit = pins[3];
const readBit  = pins[2];

// And the busreq/ack lines for grabbing the Z80 bus
const ackBit   = pins[15];
const reqBit   = pins[14];

/** Wait for a second. This is used to wait for BUSACK if it's not immediately available. */
function wait() {
  return new Promise((resolve) => {
    setTimeout(() => resolve(), 1000);
  });
}

/** Read the BUSACK signal and return to caller */
function bus() {
  // Read GPIO 15 (bus ack)
  const val = ackBit.digitalRead();
  // console.log("ACK STATE: "+(val==0 ? 'GRANT' : 'DENIED'));
  return val;
}

/** @function writeByte
 *  @description Write a byte to the Z80. Note that this assumes we have control of the
 *  bus and so this is a low level function. The 'strobe' signal can help with debugging.
 *  By default the function sets up the GPIO to drive the address and data lines then
 *  pulses the write line low then high. When debugging a board it can be useful to manually
 *  strobe that line.
 *  @param {Number} address - 16 bit Z80 address
 *  @param {Number} data - 8 bit byte to write
 *  @param {Boolean} strobe - see above
 *  @return {Promise} that resolves on complete.
 */
function writeByte(address, data, strobe=true) {
  // Create the 24 bit write value.
  const write = (address << 8 | (data & 0xff));

  return new Promise((resolve) => {
    // Sequence is:
    //  a) Set the address and data IO lines
    //  b) Toggle write -> low
    //  c) Toggle write -> high

    // Write the data
    var set =  mapValue(write) & limitMask;
    var clr = (~set) & limitMask;

    bank.set(set);
    bank.clear(clr);

    // Toggle the write line.
    if (strobe) {
      writeBit.digitalWrite(0);
      setTimeout(() => {
        // console.log("DONE");
        writeBit.digitalWrite(1);
        resolve();
      }, 1);
    }
    else resolve();
  });
}
/** @function readByte
 *  @description While the library doesn't allow Z80 memory to be read into the Pi this
 *  function drives all the Z80 lines to do the meory read. On the SBC you can use the
 *  read pulse coupled with 'basack' to strobe data into latches and drive a set of
 *  LEDs. Very useful when debugging new hardware.
 */
function readByte(address, strobe=true) {
  // console.log("READ ADDRESS: "+address.toString(16));

  // Just need the address lines for this.
  const read = (address << 8);

  return new Promise((resolve) => {
    var set =  mapValue(read) & limitMask;
    var clr = (~set) & limitMask;

    bank.set(set);
    bank.clear(clr);
    if (strobe) {
      readBit.digitalWrite(0);
      setTimeout(() => {
        // console.log("DONE");
        readBit.digitalWrite(1);
        resolve();
      }, 1);
    }
    else resolve();
  });
}
/* Make sure the read/write control lines are inactive at start. */
writeBit.digitalWrite(1);
readBit.digitalWrite(1);

/* read(state) - sset the state of the Z80 read line */
function read(on) {
  readBit.digitalWrite(on);
}
/* write(state) - sset the state of the Z80 read line */
function write(on) {
  writeBit.digitalWrite(on);
}
/** @function requestBus
 *  @description Assert the bus request line (or de-assert)
 */
function requestBus(req) {
  reqBit.digitalWrite(req==true ? 0 : 1);
  return Promise.resolve();
}
/** @function busRequest
 *  @description Assert BUSREQ and wait for BUSACK from the Z80 to become active
 */
function busRequest(req) {
  if (req==false)
    return requestBus(false);

  // Assert the BUSREQ line
  requestBus(true);

  // if we have the bus already then return immediately
  if (bus()==0)
    return wait();

  return new Promise((resolve) => {
    const h = setInterval(() => {
      // Check the bus...
      if (bus()==0) {
        clearInterval(h);
        resolve();
      }
      else {
        // console.log("WAITING FOR BUS...");
      }
    }, 1000);
  });
}

/** EXPORT everything so those can be called from an interactive NODE shell. */
module.exports = {
  readByte, writeByte, read, write, readBit, writeBit, ackBit, reqBit, bus, pins, bank, mapValue, requestBus, busRequest
};

// and add these to the global object to make interactive easier
// Object.assign(global, module.exports);

// And some short cuts
// global.r = readByte;
// global.w = writeByte;
// global.rb = (v) => void readBit.digitalWrite(v);
// global.wb = (v) => void writeBit.digitalWrite(v);
// global.br = busRequest;
