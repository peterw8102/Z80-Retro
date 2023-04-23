; **********************************************
; Implements: '.' (Show command history)
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


  extrn  main,E_PRTERR

  public FLSH_ID,FLSH_PRG,FLSH_CHR

TPG_0    EQU   00h
TPG_1    EQU   01h
TPG_2    EQU   02h       ; Page that contains the character set
; ------------------- FLSH_ID - Read Flash Info
; No parameters. Displays the flash ID. Good test to see whether the flash
; device is accessible and reporting sensible data (basically that THIS
; implementation is working correctly!)
FLSH_ID:  LD    HL,_flsh_id
          CALL  PRINT
          CALL  FL_ID
          LD    A,H
          CALL  WRITE_8
          WRITE_CHR '-'
          LD    A,L
          CALL  WRITE_8
          CALL  NL
          JR    main

; ------------------- FLSH_PRG - Write the monitor to the
; The monitor must first be loaded into application RAM but not run. The load
; will leave application pages 0 and 1 containing the new version of the monitor.
; The monitor will be written to flash. DO NOT SWITCH OFF DEVICE WHILE
; BEING PROGRAMMED!!!
;
; A sanity check is made on the RAM contents which must include "ZIOS" at
; locations 003Ch.
FLSH_PRG: BANK   1,(PAGE_MP)
          LD     A,(403Ch)
          CP     'Z'
          JR     NZ,_isnok
          LD     A,(403Dh)
          CP     'I'
          JR     NZ,_isnok
          LD     A,(403Eh)
          CP     'O'
          JR     NZ,_isnok
          LD     A,(403Fh)
          CP     'S'
          JR     NZ,_isnok

          ; Looks like it's probably valid. Confirm with user.
          LD     HL,_inst
          CALL   PRINT_LN
          LD     HL,_conf
          CALL   PRINT
          RST    10h
          CP     'Y'
          JR     Z,_do

          LD     HL,_cancel
          JR     E_PRTERR

_do:      LD     HL,_gd_img
          CALL   PRINT_LN

          ; And do it!!!!
          ; 1. Clear both existing pages
          LD     HL,_clpage
          CALL   PRINT
          LD     A,TPG_0
          CALL   WRITE_8
          CALL   NL

          LD     A,TPG_0
          CALL   FL_CPAGE

          LD     HL,_clpage
          CALL   PRINT
          LD     A,TPG_1
          CALL   WRITE_8
          CALL   NL

          LD     A,TPG_1
          CALL   FL_CPAGE

          ; 2. Write data to the flash device
          LD     HL,_wrpage
          CALL   PRINT
          LD     A,(PAGE_MP)
          CALL   WRITE_8
          CALL   NL

          LD     A,(PAGE_MP)
          LD     B,A
          LD     C,TPG_0
          CALL   FL_WPAGE

          LD     HL,_wrpage
          CALL   PRINT
          LD     A,(PAGE_MP+1)
          CALL   WRITE_8
          CALL   NL

          LD     A,(PAGE_MP+1)
          LD     B,A
          LD     C,TPG_1
          CALL   FL_WPAGE
          LD     HL,_done
          JR     E_PRTERR


_isnok:   LD     HL,_bad_img
          JR     E_PRTERR

; ------ FLSH_CHR
; Write a character set to a flash page. A character set defines 256 characters as a set of
; 16 bytes per character - so the whole character set is 4KB in size. The character
; set MUST have been preloaded into application address range 8000h-8fffh. No
; checls are made on the content.
FLSH_CHR: LD     HL,_chset
          CALL   PRINT_LN
          LD     HL,_conf
          CALL   PRINT
          RST    10h
          CP     'Y'
          JR     Z,_docset

          LD     HL,_cancel
          JR     E_PRTERR

_docset:  LD     HL,_gd_cset
          CALL   PRINT_LN

          ; Clear the 4K flash page holding the character set
          LD     HL,_clpage
          CALL   PRINT
          LD     A,TPG_2
          CALL   WRITE_8
          CALL   NL

          LD     A,TPG_2
          LD     HL,0
          CALL   FL_CSECT

          ; Write the first 4K of application page 2 into the
          ; first 4K of the flash page TPG_2 (writes one
          ; flash sector)
          LD     A,(PAGE_MP+2)
          LD     B,A
          LD     C,TPG_2
          LD     HL,0
          LD     DE,0
          CALL   FL_WSECT
          LD     HL,_done
          JR     E_PRTERR


_flsh_id  DEFB 'Flash ID: ',0
_bad_img  DEFB "Ram doesn't contain a valid ZLoader image",0
_gd_img   DEFB "Writing ZLoader to Flash...",0
_gd_cset  DEFB "Writing new character to Flash...",0
_done     DEFB "complete",0
_clpage   DEFB "Clearing page: ",0
_wrpage   DEFB "Writing page: ",0
_cancel   DEFB "Cancelled",0
_chset    DEFB "Overwrite character set stored in flash",0
_inst     DEFB "This command will upgrade ZLoader in flash",0
_conf     DEFB "Please confirm (Y/n) ",0
