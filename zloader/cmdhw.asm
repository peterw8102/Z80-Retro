; **********************************************
; Implements the following status commands:
;
; Syntax:
;    DH           ; Display hardware status
;
; Also exports SH_HW which is used to display
; the time and hardware configuration on boot.
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

  extrn  S_COLSTR
  extrn  SHDTIME
  extrn  S_YES,S_NO
  extrn  main

  public SH_HW, DHW

; ---- DHW
; Dump hardware configuration. No parameters.
DHW:        CALL    SH_HW2
            JR      main


SH_HW:      CALL  SHDTIME
            CALL  NL

            ; Grab the inventory
SH_HW2:     LD    C,A_HWINV
            RST   30h

            ; Inventory in DE. Display the results
            CALL  SH_SW
            CALL  SH_SDC
            CALL  SH_VDU
            CALL  SH_PIO
            CALL  NL
            RET

; ---- SH_VDU
; Show the status of the VDU card (installed or not)
SH_VDU:     LD       HL,_VDIS
            LD       A,8
            AND      E
            JR       _disres

; ---- SH_PIO
SH_PIO:     LD       HL,_PDIS
            LD       A,4
            AND      E
            JR       _disres

; ---- SH_SW
; Display the current value of the configuration DIP switch.
SH_SW:      LD       HL,_CFGSW
            CALL     PRINT
            LD       HL,S_COLSTR
            CALL     PRINT
            LD       A,D
            CALL     WRITE_8
            CALL     NL
            RET

; ---- SH_SDC
; Display status of SDCard 1 and 2
SH_SDC:     LD       HL,_SDIS
            CALL     PRINT
            LD       A,'1'
            RST      08h
            LD       A,1
            AND      E
            LD       HL,0
            CALL     _disres
            LD       HL,_SDIS
            CALL     PRINT
            LD       A,'2'
            RST      08h
            LD       A,2
            AND      E
            JR       _disres


; ---- _disres
; Display a title string then a value string. The value string is determined by the
; contents of the Z flag. Z: 'yes', NZ: 'no'
;   HL:    Message string
;    Z:    Test value
_disres:    PUSH     AF
            LD       A,H
            OR       L
            JR       Z,_nomsg
            CALL     PRINT
_nomsg:     LD       HL,S_COLSTR
            CALL     PRINT
            POP      AF
            LD       HL,S_YES
            JR       NZ,_isyes
            LD       HL,S_NO
_isyes:     JR       PRINT_LN



_VDIS:       DEFB "Video",TAB,NULL
_CFGSW:      DEFB "Cfg Sw",TAB,NULL
_SDIS:       DEFB "SDCard",TAB,NULL
_PDIS:       DEFB "PIO",TAB,NULL
_EMP:        DEFB " Missing", NULL
_PRE:        DEFB " Present", NULL
_INST:       DEFB "Installed", NULL
_SFND:       DEFB "Found", NULL
