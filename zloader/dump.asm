import defs.asm
import config.asm
import zapi.asm

import pcb_def.asm
import zlib.asm
import zios.asm
import zload.asm

  public DNVRAM,DECINST,DMP16,DUMPM,DUMPI,DUMP

  extrn  main,MORE
  extrn  DISASS,SETOFF,COLSTR

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

; ------------------- DUMPM - Block dump memory
DUMPM:    LD     A,'M'
          JR     _dodmp

; ------------------- DUMPI - Disassembler
DUMPI:    LD     A,'I'
          JR     _dodmp

GETDPAR:  LD     C,8              ; Default count
          CALL   WASTESPC
          JR     Z,.noaddr
          CP     '.'              ; Use the current PC address
          JR     NZ,.gethex

          CALL   BUFCHR
          LD     HL,(R_PC)
          JR     .gotval

.gethex:  CALL   GET_HEX          ; May or may not be a number. If not use last address
          JR     NZ,.gotval

.noaddr:  LD     HL,(DUMP_ADDR)   ; Default value to use

          ; Using default
.gotval   CALL   SKIPSPC
          CP     ','
          RET    NZ
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
_dodmp:   LD    B,A               ; Accepted mode in 'B'
          LD    (DUMP_MODE),A     ; Store mode

          ; Format of parameters is [addr][,count]
          CALL  GETDPAR           ; HL: address, A: count

          LD    (DUMP_ADDR),HL
          LD    A,B               ; Get the mode character back
          CP    'M'
          JR    NZ,decode         ; Mode is not 'M' so it's an instruction level dump

          LD    A,C

dloop2:   CALL  WRITE_16          ; 4 hex digits from HL
          CALL  P_MAPX            ; Translate into an offset and a page number and map application pages into memory

          ; Dump address (start of line)
          CALL  DMP16

          LD    HL,(DUMP_ADDR)
          LD    A,16
          CALL  ADD8T16
          LD    (DUMP_ADDR), HL
          DEC   C
          JR    NZ,dloop2
          CALL  P_RESTX            ; Reset application space registers
          LD    HL,_MOREBLK
          JR    MORE

; -------------------- DMP16 --------------------
; Dump 16 bytes from content of HL to stdout.
; Input  HL: Address of first byte to display
; Output HL: Points to first byte AFTER the 16 byte display
DMP16:    PUSH  BC
          PUSH  DE
          PUSH  HL
          ; Prepare dump area
          LD     HL,SCRATCH
          LD    (SCRATCH),HL
          LD     B,20h
          LD     A,20h
_clr:     LD     (HL),A
          INC    HL
          DJNZ   _clr;
          XOR    A
          LD     (HL),A
          POP   HL
          WRITE_CHR SPC
          LD    DE,SCRATCH+2
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
          LD    HL,SCRATCH
          CALL  PRINT_LN
          POP   HL
          POP   DE
          POP   BC
          RET

; ---- decode
; DUMP decode (instruction decode and display)
decode:   LD    B,C        ; Number of instructions
_nexti:   CALL  DECINST
          DJNZ  _nexti
          LD    (DUMP_ADDR), HL
          CALL  P_RESTX    ; Undo any application space page damage
          LD    HL,_MOREBLK
          JR    MORE

; -- DECINST
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
          LD    HL,COLSTR
          CALL  PRINT
          LD    H,D
          LD    L,E
          CALL  PRINT_LN
          POP   HL          ; Restore the adddress we were given originally. BC contains the length of the instruction
          LD    A,L
          ADD   C
          LD    L,A
          LD    A,B         ; Which will be zero because we finised a DJNZ loop
          ADC   H
          LD    H,A         ; Adjusted
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
