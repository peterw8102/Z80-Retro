const store = window.localStorage;

var currentCharset;
var editorInitialised = false;
var editorCells = [];
var glyphCells = [];
var charInfo;
var currentCharFocus;
var editingCharCode;
var undoData;
var charIcons = [];

var glyphCols = 0;

function loadClipboard() {
  const data = store.getItem('clipboard');
  if (data!=null)
    return JSON.parse(data);

  return [];
}
function saveToClipboard(charData) {
  const data = loadClipboard();
  data.push(charData);
  store.setItem('clipboard', JSON.stringify(data));
}
function pullClipboard() {
  const data = loadClipboard();
  const char = data.pop();
  if (char!=null)
    store.setItem('clipboard', JSON.stringify(data));
  return char;
}
function clearClipboard() {
  store.removeItem('clipboard');
}
function saveFont(font) {
  store.setItem('font', JSON.stringify(font));
}
function loadFont() {
  const data = store.getItem('font');
  if (data!=null)
    return JSON.parse(data);
}
var metaData = {};
function saveMeta(data) {
  Object.assign(metaData, data);
  store.setItem('meta', JSON.stringify(metaData));
}
function loadMeta() {
  const data = store.getItem('meta');
  if (data!=null)
    metaData = JSON.parse(data);
  return metaData;
}
function log(...args) {
  console.log(...args);
}
function toHex4(val) {
  return ('0000'+val.toString(16)).substr(-4).toUpperCase();
}
function toHex2(val) {
  return ('00'+val.toString(16)).substr(-2).toUpperCase();
}
/** @method closestAncestorWith
 *  @description Search up through the parents of a node for one with the specified CSS class.
 *  @param {DOMElement} elem
 *  @param {String} cssClassName
 *  @return {elem | null} - The element with the class, otherwise null if not found.
 */
function closestAncestorWith(elem, cssClassName) {
  var target;
  do {
    elem = elem.parentNode;
    if (elem!=null && elem.nodeType==1 && elem.classList.contains(cssClassName))
      target = elem;
  } while (target==null && elem!=null);
  return target;
}
function bitRev(num) {
  var rev = 0;
  for (let i=0;i<8;i++) {
    rev = (rev+rev) | (num & 1);
    num = (num>>1);
  }
  // log("OLD NUM: ", ('00000000'+old.toString(2)).slice(-8));
  // log("REV NUM: ", ('00000000'+rev.toString(2)).slice(-8));
  return rev;
}
function closeExport() {
  const hex  = document.querySelector('.hex');

  hex.classList.add('removed');
}
function exportAsm(ev) {
  ev.stopPropagation();
  ev.preventDefault();
  if (currentCharset==null)
    return;

  const hex     = document.querySelector('.hex');
  const asmDesc = document.getElementById('asmFmt');

  asmDesc.classList.add('removed');
  hex.classList.remove('removed');

  const strt = parseInt(document.forms[0].strt.value, 10);
  const ln   = parseInt(document.forms[0].len.value, 10);

  const end = Math.min(256, strt+ln);

  saveMeta({start: strt, length: ln});

  log("EXPORT FROM: "+(strt)+" TO: "+end);
  const output = [];
  const font = currentCharset.font;
  const byteArr = [];
  var byte;
  for (var i=strt;i<end;i++) {
    const data = font[i]; // Array of 16 bytes
    log("BYTE: ", data);
    byteArr.length = 0;
    for (let b=0;b<data.length;b++) {
      byte = data[b];
      byteArr.push('$'+toHex2(byte));
    }
    output.push('      DEFB    '+byteArr.join(',')+'    ; Char '+toHex2(i)+'\n');
  }
  log(output);
  document.getElementById('hex').value = output.join('');
}
function exportData(ev) {
  ev.stopPropagation();
  ev.preventDefault();
  if (currentCharset==null)
    return;

  const hex  = document.querySelector('.hex');
  const asmDesc = document.getElementById('asmFmt');

  hex.classList.remove('removed');
  asmDesc.classList.add('removed');

  // Generate the output file. Base address is 8000h.
  var addr = parseInt(document.forms[0].addr.value, 16);
  log("START ADDR", addr.toString(16));
  if (isNaN(addr) || addr<0 || addr>=0x10000)
    addr = 0x8000;

  saveMeta({addr});

  // Output 16 bytes per line (one character);
  const output = [];
  const font = currentCharset.font;

  var byteStr, byte;
  for (let i=0;i<font.length;i++) {
    var cs = 16 + (addr & 0xff) + ((addr >> 8) & 0xff);
    const data = font[i]; // Array of 16 bytes

    byteStr = '';
    for (let b=0;b<data.length;b++) {
      byte = data[b];
      byteStr += toHex2(byte);
      cs += byte;
    }
    output.push(':10'+toHex4(addr)+'00'+byteStr+toHex2((~cs) & 0xff)+'\n');
    addr += 16;
  }
  output.push(':00000001FF\n');

  // Sent to the HTML wrapper
  document.getElementById('hex').value = output.join('');
  // hex.innerText = output.join('');
}
function parseData(bytes, data) {
  // Convert the set of hex character bytes into numbers.
  for (let b=0;b<(data.length/2);b++)
    bytes.push(parseInt(data.slice(b*2, b*2+2),16));
  return bytes;
}
function parseIntelHexFile(lines) {
  // Process the Intel Hex format font definition into an internal format
  const characters = [];

  const chr = [];
  for (var i=0;i<lines.length;i++) {
    const line = lines[i];
    var [,len,addr, type, data] = ((/^:([0-9A-Fa-f]{2})([0-9A-Fa-f]{4})([0-9A-Fa-f]{2})(.*)[0-9A-Fa-f]{2}/).exec(line)||[]);
    log("LINE: ",line);
    log("LEN  STR: ["+len+"]");
    log("ADDR STR: ["+addr+"]");
    log("DATA STR: ["+data+"]");
    log("TYPE STR: ["+type+"]");

    // Only process type 00 records (data records)
    if (type==0) {
      parseData(chr, data);
      while (chr.length>=16)
        characters.push(chr.splice(0,16));
    }
  }
  log("CHARACTERS: ", characters);
  return characters;
}
function parseRawHexFile(lines) {
  // Process the Intel Hex format font definition into an internal format
  const characters = [];

  const chr = [];
  for (var i=0;i<lines.length;i++) {
    const line = lines[i];
    var [,data, d1] = ((/^(([0-9A-Fa-f]{2}\s*)+)/).exec(line)||[]);
    log("D1: ", d1);
    if (data!=null) {
      // Remove spaces...
      data = data.replace(/\s+/g,'');
      log("LINE: ",line);
      log("RAW DATA STR: ["+data+"]");

      parseData(chr, data);
      while (chr.length>=16)
        characters.push(chr.splice(0,16));
    }
  }
  log("RAW CHARACTERS: ", characters);
  return characters;
}
function parseHexFile(content) {
  var lines = content.toString().split(/\n/m);
  const firstLine = lines[0];

  // Does it look like a raw hex file or an Intel hex format line?
  if (firstLine.charAt(0)==':') {
    // Assume intel format
    log("Processing Intel hex format file");
    return parseIntelHexFile(lines);
  }
  else if ((/^[\sa-zA-Z0-9]+$/).test(firstLine)) {
    log("Processing RAW hex format file");
    return parseRawHexFile(lines);
  }
}
function fmtRow(byte) {
  const str = [];
  for (let j=1;j<256;j<<=1)
    str.push((byte & j) ? '&#xe901;' : '&#xe900;');// &#x2660;
  return str.join('');
}
function displayChar(viewer, data, charID) {
  // Draw each character, 8 characters to a line.
  const wrap = document.createElement('div');

  wrap.classList.add('char');
  wrap.setAttribute('id', 'id_'+charID);
  const id  = document.createElement('div');
  const chr = document.createElement('div');
  charIcons.push(chr);
  chr.classList.add('render');
  wrap.appendChild(chr);
  wrap.appendChild(id);

  id.innerText = charID+' ('+toHex2(charID.toString(16))+')';

  // Data is 16 bytes
  for (let i=0;i<data.length;i++) {
    const byte = data[i];
    const row = document.createElement('div');
    row.innerHTML = fmtRow(byte);
    chr.appendChild(row);
  }
  viewer.appendChild(wrap);
}
function displayFont(viewer, font) {
  // Delete any existing font
  while (viewer.firstChild)
    viewer.removeChild(viewer.firstChild);
  for (let i=0;i<font.length;i++)
    displayChar(viewer, font[i], i);
}
function initEditor(editor) {
  if (!editorInitialised) {
    charInfo = editor.querySelector('.info');

    const chr = editor.querySelector('.char');

    // Create the 8x16 edit matrix
    for (var r=0;r<16;r++) {
      const rowElems = [];
      editorCells.push(rowElems);
      const row = document.createElement('div');
      chr.appendChild(row);

      for (var c=0;c<8;c++) {
        const cell = document.createElement('span');
        rowElems.push(cell);
        cell.dataset.row = r;
        cell.dataset.col = c;
        row.appendChild(cell);
      }
    }
    // Add the info area
    editorInitialised = true;
  }
  editor.classList.remove('removed');
}
function editChar(editor, charCode) {
  log("Want to edit character: ", charCode);
  // Valid character?
  const charData = currentCharset.font[charCode];

  // make a copy of the data so it can be reverted
  undoData = JSON.parse(JSON.stringify(charData));

  editingCharCode = charCode;

  log("CHAR DATA: ", charData);
  if (charData==null)
    return;

  // Clear any existing data
  const spans = Array.from(editor.querySelectorAll('span'));
  spans.forEach((c) => c.classList.remove('set'));

  // Initialise the editor by writing this character data to the edit matrix.
  for (var r=0;r<charData.length;r++) {
    const rdata = charData[r];
    const rcells = editorCells[r];
    for (let c=0,m=1;c<8;c++, m<<=1) {
      if (rdata & m)
        rcells[c].classList.add('set');
    }
  }
  charInfo.innerText = "Edit character: "+charCode+' ('+toHex2(charCode)+')';
  document.forms[0].strt.value = charCode;
}
function start() {
  const file   = document.getElementById('font');
  const fdesc  = document.querySelector('.fdesc');
  const fcont  = document.querySelector('.fcont');
  const viewer = document.querySelector('.viewer');
  const editor = document.querySelector('.editor');
  const cont   = document.querySelector('.content');

  const w       = document.getElementById('w');
  const h       = document.getElementById('h');
  const glyph   = document.getElementById('glyph');
  const draw    = document.getElementById('draw');

  const asmImportButton = document.getElementById('do_impasm');

  function importAsm(ev) {
    ev.stopPropagation();
    ev.preventDefault();
    if (currentCharset==null || editingCharCode==null)
      return;

    const hex  = document.querySelector('.hex');
    const asmDesc = document.getElementById('asmFmt');

    asmDesc.classList.remove('removed');
    hex.classList.remove('removed');
    document.getElementById('do_impasm').classList.remove('removed');
  }
  function doImportAsm(ev) {
    ev.stopPropagation();
    ev.preventDefault();

    if (currentCharset==null || editingCharCode==null) {
      hex.classList.add('removed');
      asmBut.classList.add('removed');
      return;
    }
    // Grab the text area content and parse each line. Every full
    // 16 bytes is the next character.
    const hex    = document.querySelector('.hex');
    const asm    = document.getElementById('hex').value;
    const asmBut = document.getElementById('do_impasm');
    hex.classList.add('removed');
    asmBut.classList.add('removed');

    // Split input into lines
    const lines = asm.split(/[\n\r]+/g);
    log("VALUE: ", lines);
    const lineTest = /^.*DEFB\s+/;
    const lineFix = /^.*DEFB\s+|\s+;.*$/g;
    const bytes = [];
    while (lines.length>0) {
      let line = lines.shift();
      if (lineTest.test(line)) {
        line = line.replace(lineFix, '');
        const toks = line.split(/\s*,\s*/);
        log("ACCEPTED: ", toks);

        // Each token *should* be a byte. Only accept $ hex format.
        toks.forEach((t) => {
          if (t.charAt(0)=='$')
            bytes.push(parseInt(t.substr(1), 16));
        });
      }
    }
    log("NUMBER OF BYTES: ", bytes.length);
    log("BYTES: ", bytes);
    let charCode = editingCharCode;
    while (bytes.length>=16) {
      // Same as pasting a single character except we want to increment the current edit character after each one.
      log("UPDATE CHAR: ", charCode);
      setCharacter(charCode, bytes.splice(0, 16));
      charCode = (charCode + 1) & 0xFF;
    }
    // Edit the first character
    editChar(editor, editingCharCode);
  }
  function setCharacter(charCode, data) {
    const wrap = document.getElementById('id_'+charCode);
    if (wrap==null)
      return;

    currentCharset.font[charCode] = data;
    const charRender = wrap.querySelector('.render');

    // redraw this character
    for (let r=0;r<data.length;r++)
      charRender.childNodes[r].innerHTML = fmtRow(data[r]);
  }
  function reverse() {
    if (currentCharset==null)
      return;

    const font = currentCharset.font;

    for (let i=0;i<font.length;i++) {
      font[i] = font[i].map(bitRev);
    }
    saveFont(currentCharset);

    // Redraw modified font
    displayFont(viewer, font);

    if (editingCharCode!=null) {
      // There's a current character being edited then that needs resetting.
      editChar(editor, editingCharCode);
    }
  }
  function revertChanges() {
    if (currentCharset==null || editingCharCode==null || undoData==null)
      return;

    currentCharset.font[editingCharCode] = JSON.parse(JSON.stringify(undoData));

    // Redraw each row.
    for (let r=0;r<undoData.length;r++)
      currentCharFocus.childNodes[r].innerHTML = fmtRow(undoData[r]);

    // And re-edit
    editChar(editor, editingCharCode);
  }
  function saveChar() {
    if (currentCharset==null || editingCharCode==null || undoData==null)
      return;
    saveToClipboard(currentCharset.font[editingCharCode]);
  }
  function pasteClipboard() {
    if (currentCharset==null || editingCharCode==null || undoData==null)
      return;

    const data = pullClipboard();
    if (data!=null) {
      currentCharset.font[editingCharCode] = data;

      // Redraw each row.
      for (let r=0;r<data.length;r++)
        currentCharFocus.childNodes[r].innerHTML = fmtRow(data[r]);

      // And re-edit
      editChar(editor, editingCharCode);
    }
  }
  function editCharID(id) {
    if (isNaN(id) || id<0 || id>255)
      return;

    log("EDIT INITIAL CHARACTER", id);
    const wrap = document.getElementById('id_'+id);
    if (wrap!=null) {
      if (currentCharFocus!=null) {
        // Losing focus so save font data
        saveFont(currentCharset);
        currentCharFocus.classList.remove('current');
      }
      currentCharFocus = wrap.querySelector('.render');
      currentCharFocus.classList.add('current');

      saveMeta({editChar: id});
      editChar(editor, id);
      enableGlyph(true);
    }
  }
  function startEditChar(ev) {
    ev.stopPropagation();
    ev.preventDefault();

    const wrap = closestAncestorWith(ev.target, 'char');
    if (wrap==null)
      return;
    log("WRAP: ", wrap);

    if (currentCharFocus!=null) {
      // Losing focus so save font data
      saveFont(currentCharset);
      currentCharFocus.classList.remove('current');
    }
    currentCharFocus = wrap.querySelector('.render');
    currentCharFocus.classList.add('current');

    // ID gives us the character code to edit.
    const id = parseInt(wrap.getAttribute('id').slice(3), 10);
    log("CHAR ID: ", id);

    if (!isNaN(id)) {
      saveMeta({editChar: id});
      editChar(editor, id);
    }
    enableGlyph(true);
  }
  function updateChar(charNum, row, col, set) {
    // Update the data
    const charData = currentCharset.font[charNum];
    if (set)
      charData[row] = (charData[row] | (1<<col));
    else
      charData[row] = (charData[row] & (~(1<<col)));

    // Update the icon row.
    const rowData = charIcons[charNum].childNodes[row];
    rowData.innerHTML = fmtRow(charData[row]);

  }
  function changeChar(ev) {
    log("EDIT BIT: ", ev.target);
    if (ev.target==null || ev.target.tagName!='SPAN')
      return;
    const row = ev.target.dataset.row;
    const col = ev.target.dataset.col;

    // Toggle the bit.
    ev.target.classList.toggle('set');

    // Update the stored character and the icon representation.
    updateChar(editingCharCode, row, col, ev.target.classList.contains('set'));
  }
  function changeGlyphChar(ev) {
    log("EDIT BIT: ", ev.target);
    if (ev.target==null || ev.target.tagName!='SPAN')
      return;

    const row = ev.target.dataset.row;
    const col = ev.target.dataset.col;

    // Character row and column
    const rowchr = row>>4;
    const colchr = col>>3;

    // Toggle the bit.
    ev.target.classList.toggle('set');

    // Update the stored character and the icon representation.
    const thisChr = (editingCharCode + glyphCols*rowchr + colchr);
    log("Modify char: ", thisChr);
    updateChar(thisChr, row&15, col&7, ev.target.classList.contains('set'));
  }
  function loadFile(ev) {
    if (ev.target.files[0]!=null) {
      const fl = ev.target.files[0];
      fcont.classList.remove('hidden');
      fdesc.innerText = fl.name;
      fdesc.classList.remove('Error');

      // Process Text...
      fl.text().then((t) => {
        const font = parseHexFile(t);
        if (font==null) {
          fdesc.innerText = 'invalid file - plain text only';
          fdesc.classList.add('Error');
          return Promise.reject("Invalid Font");
        }
        currentCharset = {name: fl.name, font};
        saveFont(currentCharset);
        displayFont(viewer, font);
        cont.classList.remove('removed');
      });
    }
    else {
      fdesc.innerText = 'none selected';
    }
    return true;
  }
  function enableGlyph(en) {
    glyph.disabled = !en;
    w.disabled = !en;
    h.disabled = !en;
  }
  function startGlyph() {
    var width  = glyphCols = parseInt(document.forms[0].width.value);
    var height = parseInt(document.forms[0].height.value);
    log("WIDTH: ", width);
    log("HEIGHT: ", height);

    saveMeta({g_width: width, g_height: height});

    const area = draw.querySelector('.pixels');

    // Clear any existing glyph
    while (area.firstChild)
      area.removeChild(area.firstChild);

    glyphCells.length = 0;

    const xpix = width << 3;   // Turn into pixels
    const ypix = height <<4;   // Turn into pixels
    for (let r=0;r<ypix;r++) {
      const rowElems = [];
      glyphCells.push(rowElems);
      const row = document.createElement('div');
      area.appendChild(row);

      for (let c=0;c<xpix;c++) {
        const cell = document.createElement('span');
        rowElems.push(cell);
        cell.dataset.row = r;
        cell.dataset.col = c;
        row.appendChild(cell);
      }
    }
    log("GLYPH CELLS: ", glyphCells);
    // Initialise each character from the font.
    let startChar = editingCharCode;
    log("START CHAR: ",startChar);
    for (let r=0;r<height;r++) {
      for (let c=0;c<width;c++) {
        const chr = currentCharset.font[startChar++];
        renderGlyphChar(chr, r, c);
      }
    }
    draw.classList.remove('removed');
  }
  function renderGlyphChar(charData, row, col) {
    const pixrow = row<<4;
    const pixcol = col<<3;
    for (let r=0;r<charData.length;r++) {
      const rdata = charData[r];
      const rcells = glyphCells[r+pixrow];
      for (let c=0,m=1;c<8;c++, m<<=1) {
        if (rdata & m)
          rcells[c+pixcol].classList.add('set');
      }
    }
  }
  function saveGlyph() {
    // Data is already in the font data - just need to save it to non-volatile and close the editor pane.
    saveFont(currentCharset);

    draw.classList.add('removed');
  }
  function setInitialValues() {
    const meta = loadMeta();

    if (meta.addr!=null)
      document.forms[0].addr.value = meta.addr.toString(16);
    if (meta.start!=null)
      document.forms[0].strt.value = meta.start;
    if (meta.length!=null)
      document.forms[0].len.value = meta.length;
    if (meta.g_width!=null)
      document.forms[0].width.value = meta.g_width;
    if (meta.g_height!=null)
      document.forms[0].height.value = meta.g_height;
    if (meta.editChar!=null)
      editCharID(meta.editChar);
  }

  file.addEventListener('change', loadFile);
  viewer.addEventListener('click', startEditChar);

  editor.querySelector('.char').addEventListener('click', changeChar);

  draw.querySelector('.pixels').addEventListener('click', changeGlyphChar);
  document.getElementById('saveg').addEventListener('click', saveGlyph);

  // Existing font?
  const existing = loadFont();
  if (existing!=null) {
    log("EXISTING: ", existing);
    displayFont(viewer, existing.font);
    currentCharset = existing;
    cont.classList.remove('removed');
  }
  document.getElementById('revert').addEventListener('click', revertChanges);
  document.getElementById('save').addEventListener('click', () => saveFont(currentCharset));
  document.getElementById('copy').addEventListener('click', saveChar);
  document.getElementById('clear').addEventListener('click', clearClipboard);
  document.getElementById('paste').addEventListener('click', pasteClipboard);
  document.getElementById('export').addEventListener('click', exportData);
  document.getElementById('expasm').addEventListener('click', exportAsm);
  document.getElementById('impasm').addEventListener('click', importAsm);
  document.getElementById('reverse').addEventListener('click', reverse);
  document.getElementById('closehex').addEventListener('click', closeExport);

  asmImportButton.addEventListener('click', doImportAsm);
  glyph.addEventListener('click', startGlyph);


  saveFont(currentCharset);
  initEditor(editor);
  setInitialValues();
}
if (document.readyState=='loading')
  document.addEventListener('DOMContentLoaded', start);
else {
  // Already loaded
  start();
}
