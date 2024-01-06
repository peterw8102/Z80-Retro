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
 * U{u}
 * :{hexdata}
 * >LLCC
 * -----------------------
 * Where:
 *  - {u} The CP/M user number (range 0-15)
 *  - LL is the lower 8 bits of a count of all the bytes in the
 *       hexdata set (size of binary)
 *  - CC is the checksum, calculated by adding every byte in the
 *       hexdata set and taking the least significant 8 bits.
 *
 * Command line options
 * --minsize=nnn     Exclude files smaller than nnn kilobytes
 * --maxsize=nnn     Exclude files bigger than nnn kilobytes
 * --excltype=XXX    Exclude files with the extention XXX
 * --defuser=n       Write files to this CP/M user number
 * --userdir         Use the name of a files parent directory as the CP/M
 *                   user number if it is numeric in the range 0-15
 * --patch=fname     Apply a set of patches to a single file
 */

const writeout = (...args) => process.stdout.write(...args);
const errout = (...args) => process.stderr.write(...args);
const enableDebug = false;
function log(...args) {
  if (enableDebug)
    console.error(...args);
}


function report(...args) {
  console.error(...args);
}

const args = ([]).concat(process.argv);

// Keep some basic stats
const stats = {
  files: {
    processed: 0,
    hexFiles: 0,
    excluded: 0
  },
  bytes: {
    processed: 0
  }
};


// This can get set to `true`. If true then the name of the parent directory of a file
// is checked. If the name is an integer number in the range 0-15 this is used as the
// CP/M user area number. This is sent to download.com as part of the prefix before
// the hex data.
//
// the --userdir flag sets this option to true
var userFromDir = false;

// CP/M User number. Defaults to zero but can be switched to this ID instead. Valid
// user numbers are from 0-15.
var defUser = 0;

// Min and max file sizes. Outside of these ranges files are not processed.
var minFileSize = 0;
var maxFileSize = -1; 

// Strip all parameters up until one containing our command line name
while (args.length > 0 && require.main.filename.indexOf(args[0])!=0)
  args.shift();

// The first thing in the array now should be US so discard that out as well.
args.shift();

// Output two character hext value of a byte (zero padded for <0x10)
function toHex2(val) {
  return ('00'+val.toString(16)).slice(-2);
}
// Output two character hext value of a byte (zero padded for <0x10)
function toHex4(val) {
  return ('0000'+val.toString(16)).slice(-4);
}

/** Every file in the output package is prefixed with a CP/M
 *  download command specifying the file name and then a U0
 *  prefix. The '0' specifies the CP/M user number. For this
 *  implementation the user number is always zero.
 *  @param {String} fname - the name of the file being processed
*/
function sendPrefix(fname, userID) {
  writeout('A:DOWNLOAD '+fname.toUpperCase()+'\n');
  writeout('\nU'+userID.toString(16)+'\n:');
}
function getUserID(fpath) {
  if (!userFromDir) {
    // Go to the default user
    return defUser;
  }
  // Check the path name
  log("USER DIR PATH: "+fpath);
  const tok = parseInt(((/(\d+)\/[^/]+$/).exec(fpath) ?? [])[1],10);
  log("LastPart: ", tok);
  let usr = defUser;
  if (tok!=null && !isNaN(tok) && tok>=0 && tok<=15)
    usr = tok;
  return tok;
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
  const lines = file.toString().split(/[\n\s]+/g);
  log("LINES: ", lines);

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
  stats.bytes.processed += processed;
}

/** patchFile
 *  The patch file is expected to be in an Intel hex format. If the
 *  source file has a .com extension then the contents are assumed
 *  to be located at address 100h
 *  @param {String} fpath - to the file being patched
 *  @param {Buffer} fl    - data being patched (content of fpath file)
 *  @param {Buffer} patch - Intel hex format patch file
 *  @return reference to the modified buffer
 */
function patchFile(fpath, fl, patch) {
  // Read each line in the patch file and process any data records.
  const pdata = patch.toString().split(/[\r\n]+/g);
  log("INPUT FILE LENGTH: ", fl.length);
  log("SPLIT FILE: ", pdata);
  const maxOffset = fl.length;

  while (pdata.length>0) {
    // Process a single line.
    const parts = splitLine.exec(pdata.shift()) ?? [];

    const len  = parseInt(parts[1],16);
    const type = parseInt(parts[3],16);
    let   cs   = parseInt(parts[5],16);
    let   addr = parseInt(parts[2],16);
    let   data = parts[4];

    if (type!=0)
      continue;

    log("PROCESSING: ", parts);

    // Data record. Get address and patch each byte
    addr -= 0x100;      // Execution address is 100h

    log("ADDR: ", addr.toString(16), " DATA: ", data);
    for (let i=0;i<len;i++) {
      const byte = parseInt(data.slice(i*2, i*2+2),16);
      log("BYTE: ", byte.toString(16));
      if (addr<maxOffset)
        fl.writeUInt8(byte, addr);
      else
        log("Offset out of range: ", toHex4(addr));
      addr += 1;
    }
  }
  log("MODIFIED DATA: ", fl);
  // Split into lines
  return fl;
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

    if (patchData!=null) {
      patchFile(fpath, fl, patchData);
    }

    // Intel hex file?
    const isHex = (/\.hex$/).test(fname);
    if (isHex) {
      log("Converting Intel HEX file to .com");
      stats.files.hexFiles++;
      outname = fname.replace(/\.hex$/i,'.com');
    }
    const user = getUserID(fpath);
    sendPrefix(outname, user);
    if (isHex)
      convertHexFile(fl, flen);
    else
      convertFile(fl, flen);
  }
}


const filesToProcess = [];
var   patchName, patchData;

const bannedExts = [];

/** Check whether a specific file name should be processed
 *  based on filename, file size etc.
 * @param {Object} st - output from fstat
 * @param {String} fname
 */
function acceptFile(st, fname) {
  // Compare with min/max file sizes.
  if (st.size < minFileSize) {
    log("File: "+fname+" excluded. Too small\n");
    return false;
  }
  if (maxFileSize > 0 && st.size > maxFileSize) {
    log("File: " + fname + " excluded. Too big\n");
    return false;
  }
  // Any exclude file extensions
  const fext = fname.replace(/.*\./, '').toUpperCase();
  log("FILE: "+fname+". EXCLUDE CHECK AGAINST EXT: '"+fext+"'\n");
  for (let i=0;i<bannedExts.length;i++) {
    if (bannedExts[i]==fext) {
      log("Excluding '"+fname+"' because of extension\n");
      return false;
    }
  }
  log("File included: "+fname+"\n");
  return true;
}
var minFileSize = 0;
var maxFileSize = -1; 

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
  if (argPath.indexOf('--patch=')==0) {
    // Record patch file name
    patchName = argPath.replace(/^--patch=/,'');
    log("Patch incoming file with: "+patchName);

    if (!fs.existsSync(patchName)) {
      errout("Patch file '"+patchName+"' doesn't exist\n");
      patchName = null;
    }
    else {
      const fname = patchName.replace(/^.*\//,'');
      patchData   = fs.readFileSync(patchName);

      log("PATCH DATA: ", patchData.toString());
    }
    return;
  }
  if (argPath == '--help' || argPath == '-h') {
    process.stdout.write("bin2cpmhex - packager for CP/M download files\n");
    process.stdout.write("--userdir      : set CP/M user from directory name\n");
    process.stdout.write("--defuser=n    : CP/M user number (0-15)\n");
    process.stdout.write("--excltype=XXX : Exclude files with this extention\n");
    process.stdout.write("--minsize=nnn  : Exclude files smaller than nnn KB\n");
    process.stdout.write("--maxsize=nnn  : Exclude files bigger than nnn KB\n");
    process.reallyExit();
  }
  if (argPath.indexOf('--userdir')==0) {
    userFromDir = true;
    log("Switched on directory to user mapping\n");
    return;
  }
  if (argPath.indexOf('--defuser=')==0) {
    const tmpusr = parseInt(argPath.substring(10), 10);
    if (isNaN(tmpusr) || tmpusr<0 || tmpusr>15) {
      process.stderr.write("Invalid default user number\n");
      process.reallyExit(-1);
    }
    defUser = tmpusr;
    log("Default user set to "+defUser+"\n");
    return;
  }
  if (argPath.indexOf('--minsize=')==0) {
    const tmpsz = parseInt(argPath.substring(10), 10);
    if (isNaN(tmpsz) || tmpsz < 0 || tmpsz >4096) {
      process.stderr.write("Invalid minimum file size filter (valid: 0-4096)\n");
      process.reallyExit(-1);
    }
    minFileSize = tmpsz * 1024;
    log("Ignore files smaller than "+tmpsz+"KB\n");
    return;
  }
  if (argPath.indexOf('--maxsize=')==0) {
    const tmpsz = parseInt(argPath.substring(10), 10);
    if (isNaN(tmpsz) || tmpsz < 0 || tmpsz >4096) {
      process.stderr.write("Invalid maximum file size filter (valid: 0-4096)\n");
      process.reallyExit(-1);
    }
    maxFileSize = tmpsz * 1024;
    log("Ignore files bigger than "+tmpsz+"KB\n");
    return;
  }
  if (argPath.indexOf('--excltype=') == 0) {
    const tmpext = argPath.substring(11).toUpperCase();
    bannedExts.push(tmpext);
    log("Exclude files with extention: " + tmpext + "\n");
    return;
  }
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
    // Standard file. Is it excluded?
    if (acceptFile(s, argPath)) {
      filesToProcess.push(argPath);
      stats.files.processed++;
    }
    else {
      stats.files.excluded++;
    }
  }
}

// Do stuff...
(function() {
  // Process all command line arguments.
  var fpath;
  while (fpath = args.shift()) // eslint-disable-line
    parseParam(fpath);

  log("TOTAL FILES TO BE PROCESSED: ", filesToProcess);

  if (patchData!=null && filesToProcess.length>1) {
    errout("--patch can only be used on a single file!\n")
    process.reallyExit();
  }
  // Now generate the package for all accepted files
  while (fpath = filesToProcess.shift()) { // eslint-disable-line
    processFile(fpath);
  }
  report("Files processed:     "+stats.files.processed);
  report("Hex files processed: "+stats.files.hexFiles);
  report("Files excluded:      "+stats.files.excluded);
  report("Bytes processed:     "+stats.bytes.processed);
})();
