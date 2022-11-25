## Font Editor

This font editor can load/edit/save bitmap fonts for use with the [video card](https://oshwlab.com/peterw8102/8bit-video-card).

To use the editor just open `font-editor.html` in the Google Chrome browser. It'll probably work in other browsers but I've not tested those.

Features:

+ Load font files in Intel (1) or Raw (2) hex format
+ Save fonts in Intel hex format
+ Export characters in Z80 assembler format to include as data in a programme
+ Edit individual characters
+ Edit groups of characters as a 'sprite', eg a 4x8 spaceship (!)
+ Persists character set in Chrome persistent store.

## Notes

1. The address values in the Intel hex file are ignored and the files is treated as a byte string and the bytes define the characters starting with the character code 0.
2. Raw hex format files comprise a set of hex character pairs, each defining a byte value. Each pair can optionally be separated by spaced.
