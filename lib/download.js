const io = require('./memio.js');
const logMod = require('./log');
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
function toHex4(val) {
  return ('0000'+val.toString(16)).substr(-4);
}
function toHex2(val) {
  return ('00'+val.toString(16)).substr(-2);
}

function writeByte(addr, data) {
  log("WRITE: "+toHex4(addr)+" "+toHex2(data));
  return io.writeByte(addr, data);
}
async function downloadFile(lines) {
  // Read lines
  for (var l=0; l<lines.length; l++) {
    var line = lines[l];
    writeout('SENDING: '+line+'\n');
    // Split out the fields. Format is:
    //   :[LEN 1][ADDR 2][REC_TYPE 1][DATA 2]+[CHKSM 1]
    // REC_TYPE IS:
    //   00: Data record
    //   01: EOF
    // We don't process the following:
    //   02: Extended Segment Address - for 80386 addressing
    //   03: Start segment address - ditto
    //   04: Extended linear address. Address field is ignored and the record includes two data bytes that are the
    //       upper 2 bytes of a 32 bit address.
    //   05: Start linear address - 32/64 bit procesors again!
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

        // The length must be > 0
        if (len<=0){
          // Skip to next line
          continue;
        }
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

  return "FINISHED";
}
// Make a list of files...
const fileList = args;

writeout("FILES TO PROCESS: "+fileList+'\n');

(async function() {
  // Open the file...
  const fs = require('fs');

  // Concatenate all lines from all files...
  var lines = [];
  var first = true;
  for (var i=0;i<fileList.length;i++) {
    var f = fileList[i];
    var fl = fs.readFileSync(f);

    const l = fl.toString().split(/\n/m);
    if (l!=null && l.length>0) {
      if (first) {
        // Grab bus and do download
        await io.busRequest(true);
        first = false;
      }
      var res = await downloadFile(l);
      log("FILE: '"+f+"' - Result: "+res);
    }
  }
  if (!first) {
    // Release bus, which will cause a reset.
    await io.busRequest(false);
  }
})();
