// Convert binary file to Intel Hex format
// Two parameters: fname start_address
// If only one parameter then it's the fname and start_address defaults to 1000h
const writeout = (...args) => process.stdout.write(...args);
const enableDebug = false;
function log(...args) {
  if (enableDebug)
    console.log(...args);
}
// Intel HEX format processor. Expect a single parameter - the name of the file to load.
const args = ([]).concat(process.argv);

// Strip all parameters up until one containing our command line name
while (args.length>0 && process.mainModule.filename.indexOf(args[0])!=0)
  args.shift();

// The first thing in the array now should be US so shift that out as well.
args.shift();

log("FILE TO PROCESS: "+args[0]);
var fname;
var startAddr = 0x1000;
if (args.length>1) {
  startAddr = parseInt(args.shift(), 16);
}
fname = args.shift();
if (isNaN(startAddr)) {
  console.error("Invalid start address: ", startAddr);
  process.reallyExit();
}

// Open the file...
const fs = require('fs');

var fl = fs.readFileSync(fname);

log("FILE LENGTH: ", fl.length);

function toHex4(val) {
  return ('0000'+val.toString(16)).substr(-4);
}
function toHex2(val) {
  return ('00'+val.toString(16)).substr(-2);
}

function convertFile(file, addr) {
  var cs, byteStr;

  // Read chunks of 16 bytes.
  let offset = 0;
  while (offset<file.length) {
    const bytes = file.slice(offset, offset+16);

    // Generate a line of hex output
    const len = bytes.length;

    // Convert each byte to hex
    cs = len;

    byteStr = '';

    // cs is the row checksum
    cs = bytes.length;
    for (let i=0;i<len;i++) {
      const byte = bytes.readUInt8(i);

      if (!isNaN(byte)) {
        byteStr += toHex2(byte);
        cs += byte;
      }
    }

    // And the address bytes
    cs += (addr & 0xff);
    cs += ((addr >> 8) & 0xff);
    if (bytes.length>0) {
      // Calculate the output record.
      writeout(':'+toHex2(bytes.length)+toHex4(addr)+'00'+byteStr+toHex2((~cs) & 0xff)+'\n');
    }
    addr += 16;
    offset += 16;
  }
  writeout(':00000001FF\n');

  return "FINISHED";
}

if (fl!=null && fl.length>0) {
  convertFile(fl, startAddr);
}
