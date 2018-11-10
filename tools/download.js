const io = require('../lib/memio.js');
const logMod = require('../lib/log');
const log = logMod('PROC');

// const io = {
//   busRequest: function(req) {
//     return new Promise((resolve) => {
//       setInterval(() => resolve(), 1000);
//     });
//   },
//   writeByte: function(addr, data) {
//     console.log("WRITE BYTE....");
//   }
// };

/* global process, console, require */
// Intel HEX format processor. Expect a single parameter - the name of the file to load.
const args = ([]).concat(process.argv);

// Debug off
logMod.logMode('none');

const writeout = (...args) => process.stdout.write(...args);
// Strip all parameters up until one containing our command line name
while (args.length>0 && process.mainModule.filename.indexOf(args[0])!=0)
  args.shift();

// The first thing in the array now should be US so shift that out as well.
args.shift();

if (args[0]=='--debug' || args[0]=='-d') {
  logMod.logMode('all');
  args.shift();
}
writeout("FILE TO PROCESS: "+args[0]+'\n');

// Open the file...
const fs = require('fs');

var fl = fs.readFileSync(args[0]);

log("FILE: ", fl.toString());

var lines = fl.toString().split(/\n/m);

function toHex4(val) {
  return ('0000'+val.toString(16)).substr(-4);
}
function toHex2(val) {
  return ('00'+val.toString(16)).substr(-2);
}

log("FILES BY LINE: ", lines);
function writeByte(addr, data) {
  log("WRITE: "+toHex4(addr)+" "+toHex2(data));
  return io.writeByte(addr, data);
}
async function downloadFile(lines) {
  // Wait until we have the bus
  await io.busRequest(true);

  // Read lines
  for (var l=0; l<lines.length; l++) {
    writeout('SENDING: '+line+'\n');
    var line = lines[l];
    // Split out the fields.
    var [,len,addr,type,data,cs] = ((/:([0-9A-F]{2})([0-9A-F]{4})([0-9A-F]{2})(.*)([0-9A-F]{2})/).exec(line)||[]);

    if (len!=null && addr!=null) {
      // If this is a
      cs = parseInt(cs, 16);
      type = parseInt(type, 16);
      if (type==1) {
        writeout("EOF");
      }
      else if (type==0) {
        // Have a data record
        addr = parseInt(addr, 16);
        len  = parseInt(len,16);

        // Format out data
        var calcCS = len + (addr&0xff) + ((addr>>8)&0xff)+type;
        while (len-->0) {
          var val = parseInt(data.substr(0,2), 16);
          calcCS += val;
          if (val!=null && !isNaN(val)) {
            // Write out byte
            await writeByte(addr++, val);
            data = data.substr(2);
          }
        }
        // log("SUM: "+(calcCS).toString(16));
        // log("SUM: "+(calcCS & 0xff).toString(16));
        // log("SUM: "+((-calcCS) & 0xff).toString(16));
        calcCS = ((-calcCS) & 0xff);
        log("CS? "+(calcCS==cs ? "OK" : "FAILED"));
        // log("CALC CS: "+calcCS.toString(16)+", EXPECTED CS: "+cs.toString(16));
      }
    }
  }

  // Release bus
  await io.busRequest(false);

  return "FINISHED";
}

if (lines!=null && lines.length>0) {
  downloadFile(lines).then((msg) => {
    log("RES: "+msg);
    process.exit();
  });
}
