var currentLogMode = 'all'; // all, none, select
var classesOn = {};

/** @method logger
 *  @description Return a module specific 'log' function. All log messages will
 *  have the page type and 'cls' string automatically prepended to the output.
 *  Call this function at the start of a module and pass in a module 'name' then
 *  call the function returned instead of 'console.log'.
 *  @param {String} cls - the class or module name to display at the start of
 *  of the output debug string.
 */
function logger(cls) {
  var _logger = function(...params) {
    if (currentLogMode == 'all' || (currentLogMode == 'select' && classesOn[cls] === true))
      console.log(cls + ': ', ...params);
  };
  return _logger;
}
/** @method logMode
  *  @description By default the 'log' function returned from 'logger' will display
  *  all messages logged. Use this method to limit messages that are logged. The
  *  following modes are allowed:
  *  'all':    log all messages
  *  'none':   display no messages
  *  'select': only log messages if the 'cls' string passed to 'logger' appears
  *            in the 'classesOn' hash and the stored value is 'true'. Change
  *            the allowed classes by calling switchClass
  *  @param {String} mode - 'all', 'none', 'select'
  */
logger.logMode = function(mode) {
  if (mode == 'all' || mode == 'none' || mode == 'select')
    currentLogMode = mode;
};
/** @method switchClass
  *  @description Change whether or not to output log messages to the console
  *  depending on 'cls' value registered when 'logger' was called. The current
  *  enabled output classes are only used if logMode('select') has been called.
  *  @param {String}  cls - the class/module to enable or disable.
  *  @param {Boolean} [forceOn=true] - The class is switched 'on' if this parameter
  *  is missing or 'true'
  */
logger.switchClass = function(cls, forceOn) {
  classesOn[cls] = (forceOn==null || forceOn===true);
};

module.exports = logger;
