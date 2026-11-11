; **********************************************
; Implements the following status commands:
;    SWD addr          ; Dump data from SDCard
;
; Where 'addr' can be one of the standard forms:
;   NNNNNNNN - 32 bit sector address into the whole SDCard
;   NNNNNNN: - 32 bit sector offset into the currently selected default drive (last mapped or listed)
;   NNNNNN:L - 32 bit offset into the specified logical drive (L becomes the default)
;
; If no address is specified then data is written to the sector last
; read. To modify a sector first dump it 'SD addr', modify the content
; using 'SM' and then write back to the SDCard with 'SW' (this command)
;
; Data is written from the ZLoader internal buffer
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

  public SWRITE

  extrn  main
  extrn  E_NOSD,E_SDERR

; Position of the boot table end marker and checksum (same as
; ENDMOFF/CSUMOFF in cmdsdboot.asm: 16 entries of 23 bytes).
BT_ENDM    EQU   16*23
  extrn  SADDR,SDPAGE,SBCALCS
  extrn  SETDMA



; -------- SWRITE
; Write the current SDPAGE data back to the SDCard. If no address is specified then write
; to the address prevously loaded.
SWRITE:  CALL  SETDMA
         CALL  WASTESPC
         CALL  SADDR
         JR    C,E_NOSD

         ; Special case if this is raw write to address 0:0 on disk 0. In this
         ; case patch up the reserved page checksum.
         LD    A,C
         CP    A_DSKRW
         JR    NZ,_nocs
         XOR   A
         OR    E   ; Checksum if the raw read from address zero (start of SDCard)
         OR    D
         OR    L
         OR    H
         JR    NZ,_nocs

         PUSH  HL
         PUSH  DE

         CALL  SBCALCS

         ; Write checksum into page buffer
         ; This used to write SDPAGE+480/481: the table kept its old checksum
         ; and partition entry 3 of the MBR was overwritten.
         LD    A,$FF
         LD    (SDPAGE+BT_ENDM),A
         LD    (SDPAGE+BT_ENDM+1),HL
         POP   DE
         POP   HL

         ; ---------- DEBUG DUMP
         ; 'C' is the read command. Need the write command.
_nocs:   LD    A,A_DSKWR-A_DSKRD
         ADD   C
         LD    C,A
         LD    B,0            ; SDCard 0 for absolute addresses
         RST   30h
         JR    NZ,E_SDERR     ; The write error used to be lost
         JR    main
         ; ---------- DEBUG DUMP
