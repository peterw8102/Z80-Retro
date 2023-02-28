; **********************************************
; Implements: 'DI', 'DM', 'DN', 'D' commands
; Syntax:
;   DN                    Dump NVRAM
;   DM [addr|.][,lines]   Dump memory
;   DI [addr|.][,lines]   Disassemble memory
;
; '.' for the address uses the current value
; of the application programme counter.
;
; Ommitting an address will dump the next block
; of addresses.
;
; If 'lines' is ommitted then 8 lines are
; displayed.
;
; **********************************************
; Copyright Peter Wilson 2022
; https://github.com/peterw8102/Z80-Retro
; **********************************************
import defs.asm
import config.asm
import zapi.asm

import pcb_def.asm
import zlib.asm
import zios.asm
import zload.asm

  public DNVRAM,DECINST,DMP16,DUMPM,DUMPI,DUMP

  extrn  main,MORE
  extrn  DISASS,SETOFF,S_COLSTR

DUMP_CHRS  EQU   SCRATCH

; ------------------- DNVRAM
; Dump the 56 bytes of NVRAM in the RTC chip
DNVRAM:   LD     HL,0
          CALL   NVRD
          LD     C,7         ; 7 rows
_nrnv:    LD     B,8         ; 8 bytes per row
_ncnv:    LD     A,(HL)
          CALL   WRITE_8
          INC    HL
          WRITE_CHR SPC
          DJNZ   _ncnv
          CALL   NL
          DEC    C
          JR     NZ,_nrnv
          JR     main

; ----- DUMPM - Block dump memory
DUMPM:    LD     A,'M'
          JR     _dodmp

; ----- DUMPI - Disassembler
DUMPI:    LD     A,'I'
          JR     _dodmp

; ----- _GETDPAR
; Get the DI/DM parameters in the form:
;   [addr|.][,lines]
; Default for number of lines is 8.
;
; OUTPUTS: C  - number of output lines to generate
;          HL - address from which to start dumping memory
_GETDPAR: LD     C,8              ; Default count
          CALL   WASTESPC
          JR     Z,.noaddr        ; No address specified so use the end of the last dump
          CP     '.'
          JR     NZ,.gethex

          CALL   BUFCHR
          LD     HL,(R_PC)        ; Use the current PC address
          JR     .gotval

.gethex:  CALL   GET_HEX          ; May or may not be a number. If not use last address
          JR     NZ,.gotval

.noaddr:  LD     HL,(DUMP_ADDR)   ; Default value to use

          ; Using default
.gotval   CALL   SKIPSPC
          CP     ','
          RET    NZ               ; Line count is optional and will default to 8
          PUSH   HL
          CALL   GET_DEC
          LD     A,L
          OR     A
          JR     Z,.udef
          LD     C,L
.udef:    POP    HL
          RET

; ------------------- DUMP
DUMP:     LD    A,(DUMP_MODE)     ; Use previous/default dump mode
_dodmp:   LD    B,A               ; Store mode in 'B'
          LD    (DUMP_MODE),A     ; Store mode

          ; Format of parameters is [addr|.][,count]
          CALL  _GETDPAR           ; HL: address, A: count

          LD    (DUMP_ADDR),HL
          LD    A,B               ; Get the mode character back
          CP    'M'
          JR    NZ,decode         ; Mode is not 'M' so it's an instruction level disassemble

          LD    A,C

dloop2:   CALL  WRITE_16          ; 4 hex digits from HL (the address)
          CALL  P_MAPX            ; Translate into an offset and a page number and map application pages into memory
          CALL  DMP16             ; Dump block od 16 bytes

          LD    HL,(DUMP_ADDR)    ; Get the original (unmodified) dump address and...
          LD    A,16
          CALL  ADD8T16           ; ...add 16 to get to the next display line address
          LD    (DUMP_ADDR), HL   ; It's stores so if this is the last line the address is rememebered
          DEC   C
          JR    NZ,dloop2
          LD    HL,_MOREBLK
          JR    MORE

; -------------------- DMP16 --------------------
; Dump 16 bytes from content of HL to stdout.
; INPUT:  HL - Address of first byte to display
; All registered EXCEPT 'AF' preserved.
DMP16:    PUSH  BC
          PUSH  DE
          PUSH  HL

          ; Prepare dump area
          LD     HL,DUMP_CHRS
          LD    (DUMP_CHRS),HL
          LD     B,20h
          LD     A,20h
_clr:     LD     (HL),A
          INC    HL
          DJNZ   _clr;
          XOR    A
          LD     (HL),A
          POP   HL
          WRITE_CHR SPC
          LD    DE,DUMP_CHRS+2
          LD    B,16             ; Number of bytes to display
dloop:    LD    A,(HL)
          CP    20h
          JR    C, outdot
          CP    7fh
          JR    NC, outdot
          LD    (DE),A
          JR    writeout
outdot:   LD    A,'.'
          LD    (DE), A
          LD    A,(HL)
writeout: INC   DE
          INC   HL
          CALL  WRITE_8
          WRITE_CHR SPC
          DJNZ  dloop
          PUSH  HL
          LD    HL,DUMP_CHRS
          CALL  PRINT_LN
          POP   HL
          POP   DE
          POP   BC
          RET

; ---- decode
; DUMP decode (instruction decode and display)
; INPUT: C  - Number of lines to disassemble
decode:   LD    B,C        ; Number of instructions
_nexti:   CALL  DECINST    ; Disassemble a single (multi-byte) instruction
          DJNZ  _nexti
          LD    (DUMP_ADDR), HL
          LD    HL,_MOREBLK
          JR    MORE

; ----- DECINST
; Decode and display single instruction. HL points to the sart of the instruction. Displays
; the HEX bytes for this instruction followed by a newline.
; INPUT:  HL - the application space address of the instruction
;          A - Address mode
; OUTPUT: HL - First byte of _next_ instruction
; Registers not saved: A
DECINST:  PUSH  BC         ; DISASS returns instruction information we don't need in BC
          PUSH  DE
          PUSH  HL         ; Application space address we want to display
_appdec2: CALL  WRITE_16   ; Write out the application space address
          CALL  P_MAPX     ; Translate into an offset and a page number and map application pages into memory

          ; A contains the block number of the address, 0-3:
          RRCA
          RRCA             ; Now in top 2 bits
          SUB   40h        ; Gives the top byte of the offset
          LD    D,A
          LD    E,0
          EX    DE,HL
          CALL  SETOFF
          EX    DE,HL

          WRITE_CHR SPC
          PUSH  HL
          CALL  DISASS     ; HL now points at the description
          LD    C,A        ; The length of the instruction - saved for later
          LD    D,H        ; Save pointer to disassembled string
          LD    E,L
          POP   HL         ; Back to pointing to the start of the instruction
          LD    B,C        ; Number of bytes to display
_nextb:   LD    A,(HL)     ; Write out 'B' HEX bytes for this instruction
          CALL  WRITE_8
          WRITE_CHR SPC
          INC   HL
          DJNZ  _nextb

          ; Output disassembler description
          LD    HL,S_COLSTR
          CALL  PRINT
          LD    H,D
          LD    L,E
          CALL  PRINT_LN
          POP   HL          ; Restore the adddress we were given originally. C contains the length of the instruction
          LD    A,C
          CALL  ADD8T16

          ; Row complete
          POP   DE
          POP   BC
          RET


_MOREBLK  DEFW  0
          DEFW  .more2
          DEFW  .more3

.more2    DEFB  "D",0
.more3    DEFB  "D ,1",0

          DSEG

DUMP_ADDR: DEFS    2
DUMP_MODE: DEFS    1
