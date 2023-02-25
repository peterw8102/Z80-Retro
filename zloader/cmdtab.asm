import zlib.asm


   extrn BP,NSTEP,SET_RGS,SSTEP,GO,SDDIR,SDLOAD,SDRUN,BOOTIX,BOOT
   extrn CONFIG,DHW,DUMPM,DUMPI,DNVRAM,DTIME,DUMP,FILL,CLS,INPUT,LDF,LDH,LDT
   extrn SDMOD,MODIFY,NSTEP,OUTPUT,PAGE,DECCHR,SDUMP,SMAP,SWRITE
   extrn MAPDSK,EXDBG,SHWHIST,RUN,WBOOT,IMG,HELP

   public BDG_TABLE,CMD_TABLE,FNDCMD,SETMODE


   CSEG




; ----- SETMODE
; Set operational mode for the UI. Currently there are two modes:
; MONITOR: THe standard boot commands are available
; DEBUG:   The additional debug commands replace some standard commands
; Set the mode by calling this function with the required mode in A:
; INPUT: A = 0    - Return current mode
;        A = 1    - Set standard mode
;        A = 2    - Set debug mode
;        A = ff   - Toggle between modes (if we evey habe more than 2
;                   modes then this will cycle through modes)
SETMODE:  PUSH   BC
          LD     B,A
          LD     A,(OPMODE)
          XOR    B
          AND    3           ; Only two lowest bits used
          LD     (OPMODE),A

          ; Set the correct command table for this mode
          BIT    0,A
          JR     Z,_dbg
          LD     HL,CMD_TABLE
          JR     _fin

_dbg:     LD     HL,BDG_TABLE
_fin:     LD     (CMDTAB),HL
          POP    BC
          RET



; ----- FNDCMD
; Search the current command table looking for the command currently
; in the input buffer.
; INPUT:   Uses the standard input line buffer for the input string
; OUTPUT:  HL  - Address of implementation routing (if found)
;          C   - Carry SET if there's a match to the command tabel
FNDCMD:   LD    HL,(CMDTAB)
_nxtchr:  CALL  BUFCHUP
          JR    Z,_miss
          LD    C,A            ; Save the input character
          LD    A,(HL)

          LD    B,A            ; Save the test character$
          AND   $7f            ; Ignore MSB

          CP    C              ; Compare with input character
          JR    NZ,_miss

          ; Input character matched test character. End of command if MSB of B is set.
          INC   HL
          RLC   B
          JR    NC,_nxtchr     ; Step to next command

          ; Next two bytes are the target address.
          LD    E,(HL)
          INC   HL
          LD    D,(HL)
          EX    DE,HL

          CALL  WASTESPC       ; Set buffer ready for next token
          SCF
          RET

          ; Command mismatch. Step to next command, reset input buffer and try again.
_miss:    LD    A,$80
          AND   (HL)
          INC   HL
          JR    Z,_miss
          ; Step over the two address bytes
          INC   HL
          INC   HL
          CALL  RESINP
          CALL  WASTESPC
          LD    A,(HL)
          OR    A
          JR    NZ,_nxtchr          ; Check against the next command...
          ; No more entries. Return with carry clear.
          OR    A
          RET





CMDDEF MACRO name,addr
    DC     name
    DEFW   addr
    ENDM


; Alternate command table format: LETTER:ADDRESS
BDG_TABLE:   CMDDEF 'B',   BP
             CMDDEF 'N',   NSTEP
             CMDDEF 'R',   SET_RGS
             CMDDEF 'S',   SSTEP
             CMDDEF 'G',   GO

CMD_TABLE:   CMDDEF 'BOS?',SDDIR
             CMDDEF 'BOS-',SDLOAD
             CMDDEF 'BOS', SDRUN
             CMDDEF 'BO-', BOOTIX
             CMDDEF 'BO',  BOOT
             CMDDEF 'C',   CONFIG
             CMDDEF 'DM',  DUMPM
             CMDDEF 'DI',  DUMPI
             CMDDEF 'DN',  DNVRAM
             CMDDEF 'DH',  DHW
             CMDDEF 'DT',  DTIME
             CMDDEF 'D',   DUMP
             CMDDEF 'F',   FILL
             CMDDEF 'H',   CLS
             CMDDEF 'I',   INPUT
             CMDDEF 'LF',  LDF
             CMDDEF 'LH',  LDH
             CMDDEF 'L',   LDT
             CMDDEF 'MS',  SDMOD
             CMDDEF 'M',   MODIFY
             CMDDEF 'N',   NSTEP
             CMDDEF 'O',   OUTPUT
             CMDDEF 'P',   PAGE              ; Display/change application page assignment
             CMDDEF 'Q',   DECCHR
             CMDDEF 'SM',  SMAP              ; Map a logical to physical SD card.
             CMDDEF 'SD',  SDUMP             ; Display SDCard sector contents
             CMDDEF 'SW',  SWRITE            ; Write to an SDCard sesctor
             CMDDEF 'S',   MAPDSK            ; Map a logical to physical SD card.
             CMDDEF 'T',   EXDBG
             CMDDEF '.',   SHWHIST
             CMDDEF 'G',   RUN
             CMDDEF 'WB',  WBOOT
             CMDDEF 'WI',  IMG
             CMDDEF '?',   HELP
             DB      0


             DSEG
OPMODE       DB      1
CMDTAB       DW      CMD_TABLE
