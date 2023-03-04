const fs = require('fs');
const path = require('path');

/*****************************************************************
 * Convert one or more binary files to a stream of HEX characters
 * suitable for the CP/M `DOWNLOAD.COM` as written originally by
 * Grant Searle (http://searle.x10host.com/cpm/index.html). Grant
 * provides a Windows GUI packager, but for those of us using
 * non-windows machine that's a bit inconvenient.
 *
 * This Javascript implementation takes one or more files or
 * directory names and creates a stream of hex data and download.com
 * command lines, sent to stdout. Generally redirect this into a
 * file and you have a package that can be easily imported into
 * a CP/M computer with the download.com executable installed.
 *
 * If a file with a '.hex' extension is seen this is taken as an
 * Intel hex form of a CP/M .com file. The output is renamed
 * to the .com version and the hex data extracted and assumed to
 * to load from 100h
 *
 * Output format is:
 * -----------------------
 * A:DOWNLOAD fname
 * U0
 * :{hexdata}
 * >LLCC
 * -----------------------
 * Where:
 *  - {hexdata} is a sequence of hex character pairs (one per byte)
 *  - LL is the lower 8 bits of a count of all the bytes in the
 *       hexdata set (size of binary)
 *  - CC is the checksum, calculated by adding every byte in the
 *       hexdata set and taking the least significant 8 bits.
 */

const writeout = (...args) => process.stdout.write(...args);
const errout = (...args) => process.stderr.write(...args);
const enableDebug = false;
function log(...args) {
  if (enableDebug)
    console.log(...args);
}
const args = ([]).concat(process.argv);

// Strip all parameters up until one containing our command line name
while (args.length>0 && process.mainModule.filename.indexOf(args[0])!=0)
  args.shift();

// The first thing in the array now should be US so discard that out as well.
args.shift();

// Output two character hext value of a byte (zero padded for <0x10)
function toHex2(val) {
  return ('00'+val.toString(16)).substr(-2);
}

/** Every file in the output package is prefixed with a CP/M
 *  download command specifying the file name and then a U0
 *  prefix. The '0' specifies the CP/M user number. For this
 *  implementation the user number is always zero.
 *  @param {String} fname - the name of the file being processed
*/
function sendPrefix(fname) {
  writeout('A:DOWNLOAD '+fname.toUpperCase()+'\n');
  writeout('U0\n:');
}

// Number of bytes to output on a single line, which is followed by a line break
const sliceSize=45;

const splitLine = new RegExp(
  '^:'+
  '([a-fA-F0-9][a-fA-F0-9])'+                        // Length
  '([a-fA-F0-9][a-fA-F0-9][a-fA-F0-9][a-fA-F0-9])'+  // Address
  '([a-fA-F0-9][a-fA-F0-9])'+                        // Type
  '((?:[a-fA-F0-9][a-fA-F0-9])*)'+                   // Data
  '([a-fA-F0-9][a-fA-F0-9])$', 'i');                 // Checksum


function processLine(parts) {
}
/** Convert a single Intel hex file to CP/M hex. In this case:
 *   file: The binary contents of the file
 *   flen: The length of of the file.
 * @param {Binary} file - the binary contents of the file to process
 * @pram  {Number} flen - number of characters in the hex data (file length)
 */
function convertHexFile(file) {
  // Split the file into lines
  const lines = file.toString().split(/[\n\s]/g);
  log("LINES: ", lines);
  log("LINE[0]: ", lines[0]);

  let checksum  = 0;
  let flen      = 0;

  // Step through each line decoding the hex data.
  while (lines.length>0) {
    const line = lines.shift();
    const parts = splitLine.exec(line);
    if (parts==null)
      continue;

    // const [,len,addr,type,data,cs] = parts;
    const len  = parseInt(parts[1],16);
    const type = parseInt(parts[3],16);
    const cs   = parseInt(parts[5],16);
    const addr = parseInt(parts[2],16);
    let   data = parts[4];

    if (type!=0)
      continue;

    // Processing a data record. Need to write out the data and calculate
    // the checksum.
    writeout(data+'\n');

    while (data.length>0) {
      const byte = parseInt(data.slice(0,2),16);
      flen++;
      checksum += byte;
      data = data.slice(2);
    }
  }
  writeout('>'+(toHex2(flen & 0xff)+toHex2(checksum & 0xff)).toUpperCase()+'\n');

  // process.reallyExit();
}
/** Convert a single file to hex. In this case:
 *   file: The binary contents of the file
 *   flen: The length of of the file.
 * @param {Binary} file - the binary contents of the file to process
 * @pram  {Number} flen - number of octets in the binary data (file length)
 */
function convertFile(file, flen) {
  var byteStr;

  // Read every byte, output the hex format and then build the checksum.
  let offset    = 0;
  let checksum  = 0;
  let processed = 0;
  let oplines   = 0;

  // Output sliceSize bytes per line until the file is exhaused.
  while (offset<flen) {
    // Get the chunk to process
    const bytes = file.slice(offset, offset+sliceSize);

    // Generate a line of hex output. The last chunk in a file may be less than sliceSize.
    const len = bytes.length;

    // Shouldn't be zero but hey-ho
    if (len>0) {
      oplines++;

      byteStr = '';

      for (let i=0;i<len;i++) {
        const byte = bytes.readUInt8(i);

        processed++;

        if (!isNaN(byte)) {
          byteStr += toHex2(byte);
          checksum += byte;
        }
      }
    }
    // And the address bytes
    if (bytes.length>0) {
      // Calculate the output record and make sue everything is uppercase. Javascript likes to
      // use lower case letters in hex conversions. DOWNLOAD.COM requires uppercase.
      writeout(byteStr.toUpperCase()+'\n');
    }
    offset += sliceSize;
  }
  // All data sent, end marker ('>'), lower 8 bits of length and then lower 8 bits of the checksum
  writeout('>'+(toHex2(flen & 0xff)+toHex2(checksum & 0xff)).toUpperCase()+'\n');
  log("TOTAL BYTES PROCESSED: ", processed);
  log("TOTAL LINES OUTPUT: ", oplines);
  log("APPROX BYTES: ", oplines * sliceSize);
}


/** Given a fully qualified file path, load the contents of that
 *  file and generate the relevant HEX byte sequence.
 *  @param {String} fpath - fully qualified path to the file name
 */
function processFile(fpath) {

  // Strip the path elements to leave just the file name
  const fname   = fpath.replace(/^.*\//,'');
  const fl      = fs.readFileSync(fpath);
  let   outname = fname;

  if (fl!=null && fl.length>0) {
    const flen = fl.length;
    log("PATH TO PROCESS: "+fpath);
    log("ROOT FNAME: "+fname);
    log("FILE LENGTH: ", flen);

    // Intel hex file?
    const isHex = (/\.hex$/).test(fname);
    if (isHex) {
      log("Converting Interl HEX file to .com");
      outname = fname.replace(/\.hex$/i,'.com');
    }
    sendPrefix(outname);
    if (isHex)
      convertHexFile(fl, flen);
    else
      convertFile(fl, flen);
  }
}


const filesToProcess = [];

/** Check whether a specified argument represents a file,
 *  a directory or is invalid. If a directory then
 *  recursively parse each entry in the directory
 *  to build a linear list of all files to be processed.
 *
 *  The resulting linear list of files is built in
 *  the `filesToProcess` array.
 *
 *  @param (String} argPath - the argument to parse. This
 *      may be aither directly from the command line or
 *      is an entry from a directory.
 */
function parseParam(argPath) {
  if (!fs.existsSync(argPath)) {
    errout("Path: '"+argPath+"' doesn't exist. Argument ignored\n");
    return;
  }
  const s = fs.statSync(argPath);
  log("FSTAT["+argPath+"]: ", s);
  log("IS DIR? ", s.isDirectory());
  if (s.isDirectory()) {
    // It's a directory so recursively process contents.
    const dir = fs.readdirSync(argPath);
    log("DIRECTORY CONTENTS: ", dir);

    // Recursively parse each entry in the directory
    dir.forEach((e) => {
      parseParam(path.join(argPath,'/',e));
    });
  }
  else {
    // Standard file so this gets processed
    filesToProcess.push(argPath);
  }
}

// Do stuff...
(function() {
  // Process all command line arguments.
  var fpath;
  while (fpath = args.shift()) // eslint-disable-line
    parseParam(fpath);

  log("TOTAL FILES TO BE PROCESSED: ", filesToProcess);

  // Now generate the package for all accepted files
  while (fpath = filesToProcess.shift()) { // eslint-disable-line
    processFile(fpath);
  }
})();
