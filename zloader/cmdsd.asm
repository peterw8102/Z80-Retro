; **********************************************
; Implements the following status commands:
;    SD addr          ; Dump data from SDCard
;
; Where 'addr' can be one of the standard forms:
;   NNNNNNNN - 32 bit sector address into the whole SDCard
;   NNNNNNN: - 32 bit sector offset into the currently selected default drive (last mapped or listed)
;   NNNNNN:L - 32 bit offset into the specified logical drive (L becomes the default)
;
; Examples:
;     SD 100     ; Dump the 100h sector. Each sector is 512 bytes. Absolute sector address
;     SD 100:b   ; Dump the content of the 100th sector in logical drive B
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

  public SDUMP

  extrn  main,MORE
  extrn  SADDR,SDMP_MD,SDMP_L
  extrn  SETDMA
  extrn  DMP16
  extrn  SDPAGE
  extrn  E_NOSD

; -------- SDUMP
; Dump the contents of a 512 byte SD card sector. By default this is an absolute sector
; number on the SDCard. Alternatively you can specify an offset into a logical drive letter.
; Formats for the display are:
;   NNNNNNNN - 32 bit sector address into the whole SDCard
;   NNNNNNN: - 32 bit sector offset into the currently selected default drive (last mapped or listed)
;   NNNNNN:L - 32 bit offset into the specified logical drive (L becomes the default)
SDUMP:   CALL  SETDMA
         CALL  WASTESPC
         JR    NZ,_getsd      ; By default load the next available sector

         ; Load the stored values and add one to the sector number
         LD    HL,(SDMP_L)
         INC   HL
         LD    A,L
         OR    H
         JR    Z,main  ; zero sector - uninitialised
         LD    (SDMP_L),HL
         EX    DE,HL
         LD    HL,(SDMP_L+2)

         LD    A,(SDMP_MD)
         LD    C,A
         JR    _ldsd

_getsd:  CALL  SADDR
         JR    C,E_NOSD

_ldsd:   PUSH  HL
         PUSH  DE
         CALL  WRITE_16
         EX    DE,HL
         LD    A,':'
         RST   08h
         CALL  WRITE_16
         POP   DE
         POP   HL

         ; Read the data into SDPAGE
         RST   30h

_do_dmp: CALL  NL
         ; 512 bytes to dump in blocks of 32 bytes
         LD    DE,0       ; The block offset to display
         LD    HL,SDPAGE  ; Source for data
         LD    B,32       ; Row count
_sdnxt:  ; Display a row
         EX    DE,HL
         CALL  WRITE_16   ; The offset
         EX    DE,HL

         ; Display 16 bytes. At the end of this call expect HL to point... (first byte past end?)
         CALL  DMP16

         LD    A,16
         ADD   E
         LD    E,A
         JR    NC,_sdnx3
         INC   D
_sdnx3:  DJNZ  _sdnxt

         LD    HL,_MORESD
         JR    MORE

_MORESD   DEFW  0
          DEFW  .sdmore2
          DEFW  .sdmore2

.sdmore2    DEFB  "SD",0
