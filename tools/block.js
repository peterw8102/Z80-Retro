/* eslint-disable no-shadow */

const fs = require('fs');
const raspi = require('raspi');
const Serial = require('raspi-serial').Serial;

const debug=false;
function log(...args) {
  if (debug)
    console.log(...args);
}
function statusLog(msg) {
  process.stdout.write(msg);
}


// Declare a
const resp = Buffer.alloc(134);
resp[0] = 0x55;
resp[1] = 0xcc;
resp[2] = 0x01;
resp[3] = 0x80;   // Status byte - default to 0
resp[4] = 0x80;   // Return 128 bytes
resp[5] = 0x00;
for (var i=6; i<135; i++)
  resp[i] = 0x55;

// 'disk' is a binary disk file representing space available to the attached CPM system. The format
// is simply a contiguos set of 128 byte sectors and the disk/track/sector values index into that
// file.
const headerLength    =   6;
const bytesPerSector  = 128;
const sectorsPerTrack = 250;
const tracksPerDisk   = 160;
const sectorsPerDisk  = (sectorsPerTrack * tracksPerDisk);
const numberOfDisks   =   4;

const totalSectors = numberOfDisks * sectorsPerTrack * tracksPerDisk;

/*** READ SECTOR BUFFER. This is used both for CP/M buffer requests and for the higher level I/O requests ***/
// Single buffer for read requests.
var readCount = 1;
const readSectBuf = Buffer.alloc(bytesPerSector+headerLength+1);
readSectBuf[0] = 0x55;
readSectBuf[1] = 0xcc;
readSectBuf[2] = 0x81;
readSectBuf[3] = 0;                      // Status - default to zero
readSectBuf[4] = bytesPerSector & 0xff;  // The length is always 128 - sector size
readSectBuf[5] = (bytesPerSector >> 8) & 0xff;

// Disk is a Promise that resolves to the diskimage requested
var diskPromise;

function createNewFile(fname) {
  return new Promise((resolve, reject) => {
    fs.open(fname, 'w+', (err, fd) => {
      // Error?
      if (err)
        reject(err);
      else {
        // Have an FD - need to force it to the required length. Create a 1K buffer (=8 sectors)
        const wbuffer = Buffer.alloc(1024);
        const sectorsPerWrite = Math.floor(1024 / bytesPerSector);

        // Fill with 0xE5
        wbuffer.fill(0xe5);

        // for (var i=0;i<1024;i++)
        //   wbuffer[i] = 0xE5;

        // Write the data to the file. Keep writing until sectors written >= totalSectors;
        var sectors = 0;
        var offset = 0;
        while (sectors<totalSectors) {
          fs.writeSync(fd, wbuffer, 0, 1024, offset);
          offset  += 1024;
          sectors += sectorsPerWrite;
        }
        resolve(fd);
      }
    });
  }).catch((err) => {
    log("ERROR creating disk image file: "+fname+", creating...", err);
    return createNewFile(fname);
  });
}
function openDiskImage(fname) {
  if (diskPromise==null) {
    // Attempt to open the file
    diskPromise = new Promise((resolve, reject) => {
      fs.open(fname, 'r+', (err, fd) => {
        // Error?
        if (err)
          reject(err);
        else
          resolve(fd);
      });
    }).catch((err) => {
      log("ERROR opening disk image file: "+fname+", creating...", err);
      return createNewFile(fname);
    });
  }
  return diskPromise;
}

var openFile, blockCount;
function closeFile() {
  return new Promise((resolve) => {
    if (openFile==null)
      resolve();
    else {
      fs.close(openFile, () => {
        openFile = null;
        resolve();
      });
    }
  });
}
function requestFile(serial, req, payload) {
  // Payload should be the fully qualified name of file to load
  statusLog("Request File: : "+payload.toString()+'\n');

  log("PAYLOAD FILE TO OPEN: ", payload.toString());
  // Close any existing file
  closeFile().then(() => {
    fs.open(payload.toString(), 'r', (err, fd) => {
      // Return either success or failure
      log("OPENED? ", err==null, err);
      blockCount = 0;
      const resBuf = Buffer.alloc(headerLength);
      resBuf[0] =  0x55;
      resBuf[1] =  0xcc;
      resBuf[2] =  0x10;        // Command code
      resBuf[3] = (err!=null);  // Only response is a status byte of 0 for success abd !0 for fail
      resBuf[4] =  0x00;
      resBuf[5] =  0x00;        // 0: OK, 1: Error
      if (err==null)
        openFile = fd;

      log("SENDING: ", resBuf);
      serial.write(resBuf, (e) => {
        log("Sent");
        if (e!=null)
          throw e;
      });
    });
  });
}
async function readFile(serial, req, payload) {
  log("PAYLOAD READ OPEN: ", payload);
  if (blockCount==0)
    statusLog('Sending');
  statusLog('.');
  // statusLog("Read file content. Block: : "+blockCount);
  // There must be an open file for this to work
  if (openFile!=null) {
    // Set the correct command response code
    readSectBuf[2] = 0x11;

    // Read 128 bytes at a time
    const bytesRead = fs.readSync(openFile, readSectBuf, headerLength, bytesPerSector);
    var eofMarker = 0;
    if (bytesRead<bytesPerSector) {
      // Reached end of file. Set the end of file flag bit (BSB of second length byte)
      eofMarker = 1;
    }
    readSectBuf[3] = eofMarker;

    // Checksum the entire 128 bytes block even of we didn't get the fill block because of EOF
    for (var i=headerLength, cs=0;i<bytesPerSector+headerLength;i++)
      cs += readSectBuf[i];

    readSectBuf[bytesPerSector+headerLength]= (cs & 0xff);

    log("SENDING BLK: "+(blockCount++));

    serial.write(readSectBuf, (err) => {
      log("Sent");
      if (err!=null)
        throw err;
      if (eofMarker!=0) {
        // Close the file
        fs.closeSync(openFile);
        statusLog('\nComplete\n\n');
        openFile = null;
      }
    });
  }
  else {
    log("FILE NOT OPEN...");
    const resBuf = Buffer.alloc(headerLength);
    resBuf[0] =  0x55;
    resBuf[1] =  0xcc;
    resBuf[2] =  0x11;        // Command code
    resBuf[3] =  0x01;        // Error status
    resBuf[4] =  0x00;
    resBuf[5] =  0x00;        // 0: OK, 1: Error

    log("SENDING (err): ", resBuf);
    serial.write(resBuf, (e) => {
      // log("Sent");
      if (e!=null)
        throw e;
    });
  }
}
// Format is DD TT TT SS (4 bytes). Take the bytes from the start of the payload.
function payloadToSector(payload) {
  // log("EXTRACT DISK ADDR FROM: ", payload);
  const disk  = payload[0];
  const track = (payload[1] | (payload[2]<<8));
  const sect  = payload[3];

  log("DISK: "+disk+", TRACK: "+track+", SECTOR: "+sect);

  return disk * sectorsPerDisk + track * sectorsPerTrack + sect;
}

async function readSector(serial, req, payload) {
  const fd = await openDiskImage('cpmdisk.img');

  const sect = payloadToSector(payload);

  // Turn this into an offset into the file and return the block.
  const offset = sect * bytesPerSector;

  // Set the correct command response code
  readSectBuf[2] = 0x81;
  readSectBuf[3] = 0x00;

  const bytesRead = fs.readSync(fd, readSectBuf, headerLength, bytesPerSector, offset);
  // log("SECTOR READ ["+(readCount++)+"]: "+sect+", READ FROM FILE: ", bytesRead, readSectBuf);
  log("SECTOR READ ["+(readCount++)+"]: "+sect+", READ FROM FILE: ", bytesRead);

  // Calculate checksum
  for (var i=headerLength, cs=0;i<bytesPerSector+headerLength;i++)
    cs += readSectBuf[i];

  readSectBuf[bytesPerSector+headerLength]= (cs & 0xff);

  serial.write(readSectBuf, (err) => {
    // log("Sent");
    if (err!=null)
      throw err;
  });
}

var writeCount = 1;
const writeSectBuf = Buffer.alloc(6);
writeSectBuf[0] = 0x55;
writeSectBuf[1] = 0xcc;
writeSectBuf[2] = 0x82;
writeSectBuf[3] = 0;
writeSectBuf[4] = 0;
writeSectBuf[5] = 0;

var nextWriteSector = -1;
async function setWriteSector(serial, req, payload) {
  nextWriteSector = payloadToSector(payload);
  log("SET WRITE ADDESS: "+nextWriteSector);
  writeSectBuf[2] = 0x82;
  serial.write(writeSectBuf, (err) => {
    // log("Sent");
    if (err!=null)
      throw err;
  });
}

async function writeSector(serial, req, payload) {
  const fd = await openDiskImage('cpmdisk.img');

  // Turn this into an offset into the file and return the block.
  const offset = nextWriteSector * bytesPerSector;

  log("WRITE ["+(writeCount++)+"] DATA TO SECT: "+nextWriteSector, payload);

  fs.writeSync(fd, payload, 0, bytesPerSector, offset);

  writeSectBuf[2] = 0x83;
  serial.write(writeSectBuf, (err) => {
    // log("Sent");
    if (err!=null)
      throw err;
  });

}

const handlers = {
  0x10: requestFile,
  0x11: readFile,
  0x81: readSector,
  0x82: setWriteSector,
  0x83: writeSector
};


/******************************************************************************
Main entry point. The BAUD rate of 460800 is for a 14.74MHz clock. If the clock
speed is 7.3528MHz then this value will need to be halved.
*******************************************************************************/
raspi.init(() => {
  log("Creating serial...");
  var serial = new Serial({
    baudRate: 460800 // 230400
  });
  log("...created");
  serial.open(() => {
    var currBuff = Buffer.alloc(1024);
    var offset = 0;
    function processBuffer() {
      // Check whether the command is complete. Don't process anything if the
      // number of characters received is less than 5.
      if (offset<5) {
        log("INPUT BUFFER LENGTH: ",offset);
        return;
      }
      // Look for start sequence *0x55 0xAA
      let cmdStart = 0;
      let found = false;

      while (!found && cmdStart<offset-1) {
        if (currBuff[cmdStart]==0x55 && currBuff[cmdStart+1]==0xAA) {
          // Found the start of command marker. Shift start of the command to the start of the buffer.
          if (cmdStart>0) {
            let from = cmdStart;
            let to   = 0;
            while (from<offset)
              currBuff[to++] = currBuff[from++];
            offset -= cmdStart;
            statusLog("Found start token 55 AA");
          }
          found = true;
        }
        cmdStart++;
      }
      if (offset<5) {
        log("INPUT BUFFER LENGTH: ",offset);
        return;
      }
      // Length meets minimum. The ONLY thing expected from the Z80 end is a
      // command. Commands are a sequence which starts with 0x55 and 0xAA.
      if (currBuff[0]!=0x55) {
        console.error("INVALID start byte: ", currBuff[0].toString(16));
        // Reset the buffer offset
        offset=0;
        return;
      }
      if (currBuff[1]!=0xAA) {
        console.error("INVALID start message type: ", currBuff[1].toString(16));
        return;
      }
      // The third character is the command byte:
      //  0x10  - request file
      //  0x11  - read file
      //  0x81  - read sector from the virtual disk
      //  0x82  - set the sector address for the next write request
      //  0x83  - write a sector to the virtual disk
      if (!handlers.hasOwnProperty(currBuff[2])) {
        console.error("Unknown CMD type: ", currBuff[2].toString(16));
        return;
      }
      // The next two bytes are a block length: the number of additional bytes to expect
      // as part of this command.
      var blkLen = currBuff[3]+(currBuff[4] << 8);
      log("PARAM LENGTH: ", blkLen);
      if (blkLen>0) {
        if (offset < blkLen+5) {
          log("Message incomplete. Actual: "+offset+", Needed: ", blkLen+5);
          return;
        }
      }
      log("CMD PAYLOAD LEN: "+blkLen+", BUFFER: ", currBuff);
      log("PARAMTER: ", (blkLen>0 ? currBuff.slice(5, 5+blkLen) : null));
      handlers[currBuff[2]](serial, currBuff, blkLen>0 ? currBuff.slice(5, 5+blkLen) : null);

      // Reset command buffer offset.
      offset = 0;
    }
    log("...Opened");
    serial.on('data', (data) => {
      // Append to current buffer
      log("RAW DATA: ",data);
      for (let i=0;i<data.length;i++)
        currBuff[offset++] = data[i];
      processBuffer();
      // process.stdout.write(data.toString());
      // send a '-' as an ack
      // if (send)
      //   serial.write('=');
    });
    log("LISTENING...");
    //serial.write('Hello from raspi-serial');
    log("...written");
    // setInterval(() => {
    //  serial.write('-');
    // }, 1000);
    //setTimeout(() => {
    //  send = true;
    //  serial.write('+');
    //}, 5000);
  });
});
