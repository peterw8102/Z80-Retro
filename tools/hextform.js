// Tool to parse and transform Intel Hex format data. Note that this
// tool is intended to transform the CONTENT of the file NOT the format.
// The output will be a modified Intel Hex file to stdout that can be
// streamed to another transform.
//
// Transforms available:
//   VALIDATE:  Null operation but will report invalid checksums
//   CHECKSUM:  Validate and fix any incorrect checksums
//   RELOCATE:  Move data records for one area of memory to another
//
// The last of these is useful for the case where the target for
// the data is a PROM programmer and you want the data in a different
// location in the ROM to where it will be run by the CPU (eg if
// it's to be copied to RAM). Assemblers/linkers don't seem to do
// a great job of this!
//
var enableDebug = false;

const fs = require('fs');
const readline = require('readline');

const writeout = (...args) => process.stdout.write(...args);
const writeerr = (str, ...args) => process.stderr.write(str+'\n',...args);
function log(...args) {
  if (enableDebug)
    console.log(...args);
}

function toHex4(val) {
  return ('0000'+val.toString(16)).substr(-4);
}
function toHex2(val) {
  return ('00'+val.toString(16)).substr(-2);
}

// Intel HEX format processor. Expect a single parameter - the name of the file to load.
const args = ([]).concat(process.argv);

// Strip all parameters up until one containing our command line name
while (args.length>0 && process.mainModule.filename.indexOf(args[0])!=0)
  args.shift();

// The first thing in the array now should be US so shift that out as well.
args.shift();

// Options:
//   --debug        Output log messages
//   --validate     Validate the checksums in the file
//   --fix          Fix the checksums in the file
//   --relocate=start,end,new-start
//                  Relocate data
//
// If no operation is specified then a VALIDATE is performed.
//
// There may be an additional parameter which is a filename. If there's
// no file name then take input from stdin.
var fname = [];

var report   = false;
var fix      = false;
var move     = false;

var mvstart = 0;     // Only relocate data records >= this address
var mvend   = 0;     // And less than this address
var mvbase  = 0;     // relstart -> this address
var mvdelta = 0;     // to add to matching data record addresses

// Stats:
const stats = {
  relocated: 0,
  processed: 0,
  data:      0,
  other:     0
};

var err;
while (err==null && args.length>0) {
  const arg = args.shift();
  if (arg[0]=='-' && arg[1]=='-') {
    // it's a switch
    const [,opt,params] = (/^--(\w+)=?(.*)?$/g).exec(arg);
    log("OPT: ", opt);
    log("PRM: ", params);
    switch (opt) {
    case 'debug':
      enableDebug = true;
      break;
    case 'validate':
      report = true;
      break;
    case 'fix':
      fix = true;
      break;
    case 'move':
      // Should be three comma separated hex values
      const [,start,end,base] = (/^([0-9a-fA-F]+),([0-9a-fA-F]+),([0-9a-fA-F]+)$/).exec(params) ?? [];
      log("VALS: ", start,end,base);
      mvstart = parseInt(start,16);
      mvend   = parseInt(end,16);
      mvbase  = parseInt(base,16);
      mvdelta = (mvbase-mvstart) & 0xffff;

      log("START: ",mvstart.toString(16),", END: ",mvend.toString(16),", NEW BASE: ",mvbase.toString(16),", DELTA: ",mvdelta.toString(16));

      if (start==null) {
        err = "Invalid move parameters: --move=start,end,newstart\n   where start,end and newstart are hex addresses\n";
      }
      move = true;
      break;
    default:
      // Unknown option
      err = "Invalid parameter: "+arg+'\n';
    }
  }
  else  {
    // Can't specify more than one file name
    fname.push(arg);
  }
}

const splitLine = new RegExp(
  '^:'+
  '([a-fA-F0-9][a-fA-F0-9])'+                        // Length
  '([a-fA-F0-9][a-fA-F0-9][a-fA-F0-9][a-fA-F0-9])'+  // Address
  '([a-fA-F0-9][a-fA-F0-9])'+                        // Type
  '((?:[a-fA-F0-9][a-fA-F0-9])*)'+                   // Data
  '([a-fA-F0-9][a-fA-F0-9])$', 'i');                 // Checksum

log("REGEXP: ", splitLine);

async function processLine(line, linenum) {
  // Break the line down into it's parts
  // const parts = splitLine.exec(':0011112233333333333344');
  // line = ':04000000F3C34801FD';
  // line = ':0011112233333333333344';
  const parts = splitLine.exec(line);
  // log('LINE: "'+line+'"');
  if (parts==null) {
    log("PARTS: ", line);
    writeout(line);    // Don't understand this so transfer unchanged to the output stream
    return;
  }
  // const [,len,addr,type,data,cs] = parts;
  const len  = parseInt(parts[1],16);
  const type = parseInt(parts[3],16);
  let   cs   = parseInt(parts[5],16);
  let   addr = parseInt(parts[2],16);

  // The data element needs to be an array of byte values
  const data = [];
  let   rawdata = parts[4];
  let partialcs = len+type;     // Checksum EXCLUDING address and data
  while (rawdata.length>0) {
    const byte = parseInt(rawdata.slice(0,2),16);
    partialcs += byte;
    data.push(byte);
    rawdata = rawdata.slice(2);
  }
  let newcs = partialcs+(addr>>8)+(addr&0xff);


  newcs = ((-newcs) & 0xff);

  // Is the checksum correct?
  if (cs!=newcs && report) {
    writeerr("Line: "+linenum+". Bad checksum. Should be: "+toHex2(newcs)+" IS: "+toHex2(cs));
  }
  if (fix)
    cs = newcs;

  // If there's a
  let send = true;
  switch (type) {
  case 1:    // EOF
    // Ignore end of file records and output our own. This allows files to be joined.
    send = false;
    break;
  case 0:    // Data
    // Is there a 'MOVE' operation required?
    stats.data++;
    if (move && addr>=mvstart && addr<mvend) {
      stats.relocated++;

      const newaddr = (addr + mvdelta) & 0xffff;

      log("MOVE RECORD FROM: "+toHex4(addr)+" TO "+toHex4(newaddr)+"\n");

      // Relocoate
      addr = newaddr;
      // And recalculate the checksum
      cs = (-((partialcs+(addr>>8)+(addr&0xff))) & 0xff);
    }
    break;
  default:
    // All other records do nothing...
    stats.other++;
    break;
  }
  if (send) {
    // Write out the (potentially) corrected/modified line.
    log("USING CHECKSUM: ",cs)
    writeout((':'+toHex2(len)+toHex4(addr)+toHex2(type)+parts[4]+toHex2(cs)).toUpperCase()+'\r\n');
  }
}


async function processStream(str) {
  const rl = readline.createInterface({
    input: str,
    crlfDelay: Infinity
  });
  let lnum = 1;
  for await (const line of rl) {
    // Each line in input.txt will be successively available here as `line`.
    await processLine(line, lnum++);
  }
  stats.processed = lnum;
}
/** If `fname` is not null then open the file and return the handle. If it is]
 *  null then return stdin.
 */
function openStream(file) {
  if (file==null)
    return process.stdin;
  else {
    // Return a Promise that resolves to the opened file name.
    return fs.createReadStream(file);
  }
}

async function processFiles(files) {
  while (files.length>0) {
    fln = files.shift();
    str = openStream(fln);
    await processStream(str);
  }
  // And send a final end of file marker.
  writeout(':00000001FF\r\n');
}

if (err!=null) {
  process.stderr.write(err);
  process.reallyExit(-1);
}

processFiles(fname).then(() => {
  log("Records processed: "+stats.processed);
  log("Data records:      "+stats.data);
  log("Other records:     "+stats.other);
  log("Records relocated: "+stats.relocated);
  process.reallyExit(-1);
});
