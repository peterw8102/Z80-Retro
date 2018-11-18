const io = require('../lib/memio.js');
const logMod = require('../lib/log');
const log = logMod('PROC');

(async function() {
  // Open the file...
  await io.busRequest(true);
  await io.busRequest(false);
})();
