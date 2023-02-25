; **********************************************
; Implements: 'C' command
; Syntax: C [id=value]
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


  extrn  COLSTR
  extrn  S_YES,S_NO
  ;
  ; ; From core
  extrn  E_BADPS
  extrn  main

  public CONFIG,CFG_TAB


; ------------ CONFIG
; Show or set a configuration options. All are boolean at the moment. Currently
; only using the first byte as 8 boolean flags.
; Bit 0:  Default for 'BOOT' - DON'T install page 0 drivers (image is stand alone)
CONFIG:   CALL   WASTESPC
          JR     Z,_cfshw

          ; Expect a hex(ish) number for the parameter ID
          CALL   GET_HEX
          JR     Z,E_BADPS
          LD     A,L
          LD     HL,CFG_TAB
          JR     C,_cfshw

          ; Find the parameter ID
          OR     A
          JR     Z,_cfset
          LD     B,A             ; B = config param ID
_cfgsn:   LD     A,(HL)          ; Bit mask
          OR     A
          JR     Z,E_BADPS         ; If zero then reached end of table
_cfcnxt:  INC    HL
          INC    HL
          CALL   _skpstr
          DJNZ   _cfgsn
_cfset:   LD     A,(HL)
          OR     A
          JR     Z,E_BADPS
          LD     C,A             ; Save mask for later

          INC    HL
          LD     B,(HL)          ; Byte offset
          INC    HL              ; Points to description/name
          EX     DE,HL           ; Save description for later in DE

          ; Calculate the address of the config byte into HL
          LD     HL,NVRAM
          LD     A,B
          CALL   ADD8T16

          LD     A,C             ; The mask

          CPL                    ; Invert
          AND    (HL)            ; Current value with flag cleared
          LD     B,A             ; Save the cleared config parameter

          ; And decide whether we need to set it.
          ; Get the new setting from the user
          CALL   SKIPSPC
          CP     '='
          JR     NZ,E_BADPS
          CALL   SKIPSPC

          CP     'N'
          JR     Z,_cfsv           ; Save the cleared value
          CP     '0'
          JR     Z,_cfsv           ; Save the cleared value
          ; Any other value and we have to set the flag.
          LD     A,C               ; The original mask
          OR     B                 ; Set the bit
          LD     B,A
_cfsv:    LD     A,B
          LD     (HL),B

          ; Print description
          AND    C                 ; test the flag just configured
          EX     DE,HL
          CALL   _say
          CALL   NVSAV
          JR     main

_cfshw:   LD     HL,CFG_TAB
          LD     B,0             ; Config index
_cfnc:    LD     A,(HL)
          OR     A
          JR     Z,main          ; End of table

          LD     C,A             ; Save mask for later
          ; Output the config parameter number (used for setting)
          LD     A,B
          CALL   WRITE_8
          WRITE_CHR ' '

          ; A contains the config bit mask
          INC    HL
          LD     A,(HL)          ; The byte offset
          EX     DE,HL
          LD     HL,NVRAM
          CALL   ADD8T16
          LD     A,(HL)          ; The byte containing the flag
          EX     DE,HL           ; HL back to the table
          INC    HL              ; points to the description
          AND    C               ; Mask to the relevant bit
          PUSH   HL
          CALL   _say
          POP    HL
          ; Then step HL past the description to the next config entry
          CALL   _skpstr

          ; Step one more to the start of the next config entry
          INC    B               ; Next command ID
          JR     _cfnc

; ---- _say
; Display a string in the form:     NAME = YN
; Where 'NAME' is the string pointed to by HL
;       'YN' is the 'YES' or 'NO' depending on the Z flag
; Z flag is the state of the flag. HL points to the name
;
; Usse to format the configuration settings.
_say:     PUSH  AF
          PUSH  HL
          JR    NZ,_sayyes
          LD    HL,S_NO
          JR    _saynow
_sayyes:  LD    HL,S_YES
_saynow:  EX    (SP),HL
          CALL  PRINT
          LD    HL,COLSTR     ; Tab alignment
          CALL  PRINT
          POP   HL
          CALL  PRINT         ; The yes/no string
          CALL  NL
          POP   AF
          RET

; --------- _skpstr
; Step over a null terminated string and return HL pointing to the first byte after the null
_skpstr:  XOR    A
_cfnch:   INC    HL
          CP     (HL)
          JR     NZ,_cfnch
          INC    HL          ; Step past null
          RET


; ---------- CFG_TAB
; Set of boolean flags that can be configured in NVRAM. Format is:
; MASK | ByteOffset | Desc | 0
CFG_TAB:        DEFB      00000001b                ; Bit 0
                DEFB      0
                DEFB      "Install OS: ",0

                DEFB      00000010b                ; Bit 1
                DEFB      0
                DEFB      "Break handler: ",0

                DEFB      0         ; Terminator
