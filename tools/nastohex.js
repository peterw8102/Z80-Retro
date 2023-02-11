// Really simple utility that takes a file expected to e in Nascom's hex format
// and write out the equivalent data in Intel hex format which we can then load
// onto our board.

const writeout = (...args) => process.stdout.write(...args);

// Useful for debugging, but that's all!
function log(...args) {
  // console.log(...args);
}
/* global process, console, require */
// Intel HEX format processor. Expect a single parameter - the name of the file to load.
const args = ([]).concat(process.argv);

// Strip all parameters up until one containing our command line name
while (args.length>0 && process.mainModule.filename.indexOf(args[0])!=0)
  args.shift();

// The first thing in the array now should be US so shift that out as well.
args.shift();

log("FILE TO PROCESS: "+args[0]);

// Open the file...
const fs = require('fs');

var fl = fs.readFileSync(args[0]);

log("FILE: ", fl.toString());

var inLines = fl.toString().split(/\n/m);

function toHex4(val) {
  return ('0000'+val.toString(16)).substr(-4);
}
function toHex2(val) {
  return ('00'+val.toString(16)).substr(-2);
}

function convertFile(lines) {
  var cs, byte, bytes, byteStr;
  // Process each Nascom format line to generate an equivalent Intel format line.
  for (var l=0; l<lines.length; l++) {
    var line = lines[l];
    log("Processing line: "+line);
    if (line.charAt(0)=='.') {
      // Write the end of file record
      writeout(':00000001FF\n');
      break;
    }
    // Split out the fields.
    var [,addr, data] = ((/([0-9A-F]{4})(.*)\s*$/).exec(line)||[]);
    log("ADDR STR: ["+addr+"]");
    log("DATA STR: ["+data+"]");
    if (addr!=null && data!=null) {
      addr = parseInt(addr, 16);

      // parse the byte data to tell how many we have for the length field
      // and to calculate the Intel format checksum byte.
      bytes = [];
      byteStr = '';
      // If this is a
      cs = bytes.length;
      while (data.length>0) {
        byte = parseInt(data.substr(1,2), 16);
        data = data.substr(3);
        //log("DATA: "+data.length+" '"+data+"'");
        if (data.length>2 && !isNaN(byte)) {
          bytes.push(toHex2(byte));
          byteStr += toHex2(byte);
          cs += byte;
        }
      }

      // And the address bytes
      cs += (addr & 0xff);
      cs += ((addr >> 8) & 0xff);
      if (bytes.length>0) {
        // Calculate the output record for Intel format.
        writeout(':'+toHex2(bytes.length)+toHex4(addr)+'00'+byteStr+toHex2((-cs) & 0xff)+'\n');
      }
    }
  }

  return "FINISHED";
}

if (inLines!=null && inLines.length>0) {
  convertFile(inLines);
}
