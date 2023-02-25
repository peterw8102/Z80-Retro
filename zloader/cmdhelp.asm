; **********************************************
; Implements: '?' (Show help text)
; **********************************************
; Copyright Peter Wilson 2022
; https://github.com/peterw8102/Z80-Retro
; **********************************************
import defs.asm
import config.asm
import zios.asm
import zlib.asm
import zload.asm


          ; Imports from 'zloader'
          extrn  main,E_BADPS,SDPAGE,COLSTR
          extrn  SETMODE

          ; Exports
          public HELP

; ----- HELP
; Display mode specific help text. The first byte of each entry is a bitmap
; of the modes in which this command entry is recognised. The bits in this
; byte are compared against the current OPMODE for a match and skipped if
; not applicable. The flag is followed by two strings, each terminated by
; a character with the MSB set. The first is the command syntax, the
; second is a summary description.
HELP:     LD    HL,_ref
          CALL  PRINT_LN
          LD    HL,_HTEXT
          XOR   A
          CALL  SETMODE
          LD    B,A
_nline:   LD    A,(HL)
          OR    A
          JR    Z,main
          AND   B
          JR    Z,_skp2
          CALL  PRINT_80         ; Returns HL after end of first string
          PUSH  HL
          EX    DE,HL
          LD    HL,COLSTR
          CALL  PRINT
          EX    DE,HL
          CALL  PRINT_80
          CALL  NL
          JR    _nline


          ; Skip the next two 80h terminated strings.
_skp2:    INC   HL
          LD    A,(HL)
          RLA
          JR    NC,_skp2

          ; And the second one
_skp3:    INC   HL
          LD    A,(HL)
          RLA
          JR    NC,_skp3
          INC   HL
          JR    _nline


HELPT MACRO flg,cmd,txt
    DEFB   flg
    DC     cmd
    DC     txt
    ENDM

_HTEXT:         HELPT 2,"B XXXX",             "set BP"
                HELPT 1,"BO-",                "load block protocol file (no exec)"
                HELPT 1,"BO",                 "as BO- but run the loaded file"
                HELPT 1,"BOS [n]",            "SDCard boot"
                HELPT 1,"BOS- [n]",           "SDCard load image"
                HELPT 1,"BOS?",               "List boot images"
                HELPT 3,"C [id=val]",         "Config parameters"
                HELPT 3,"DH",                 "Display hardware configuration"
                HELPT 3,"DI [addr]",          "Disassemble"
                HELPT 3,"DM [addr]",          "Dump mem"
                HELPT 3,"DN",                 "Dump NVRAM (56 bytes)"
                HELPT 3,"DT [dtime]",         "Display or set date/time"
                HELPT 3,"D",                  "Dump more"
                HELPT 3,"F ADD LN VV",        "Fill LN bytes from ADD with VV"
                HELPT 2,"G [addr]",           "Run (from address) until breakpoint"
                HELPT 1,"G [addr]",           "Run (from address)"
                HELPT 3,"H",                  "Clear screen"
                HELPT 3,"I pp",               "Read input port pp"
                HELPT 1,"L",                  "Load Intel hex from Stdin"
                HELPT 1,"LF",                 "Load binary file via the block protocol"
                HELPT 1,"LH",                 "Load hex file via the block protocol"
                HELPT 3,"M [addr]",           "Modify memory at addr"
                HELPT 2,"N [n]",              "Step over [n] times"
                HELPT 3,"O XX YY",            "Output YY to port XX"
                HELPT 3,"P bb=pp",            "Map page pp into bank bb"
                HELPT 2,"R RR=VV[VV]",        "Set register (8 or 16) to VV[VV]",0
                HELPT 2,"S [n]",              "Step into next instruction 'n' times",0
                HELPT 1,"SD [sect]",          "Display SDCard sector",0
                HELPT 1,"SB [params]",        "Save Bootable Image to SDCard",0
                HELPT 1,"SM",                 "Display SDCard drive map",0
                HELPT 1,"SM [drv=dsk]",       "Map disk 'dsk' to logical drive 'drv'",0
                HELPT 1,"SW",                 "Write SDCard sector",0
                HELPT 3,"WB [params]",        "Add bootable image",0
                HELPT 3,"WI [params]",        "Write image to SD",0
                DEFB  0
_ref:           DEFB  "See Wiki for details",10,13,"https://github.com/peterw8102/Z80-Retro/wiki",10,13,0
