; **********************************************
; SDCard address parsing code shared by
; a number of SDCard management functions.
;
; SADDR parses input text to determine
; a valid SDCard address. See function header
; for acceptable formats.
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

  public SETDMA,SDPAGE,SDBREAD


; ------- SETDMA
; Tell ZIOS to DMA SDCard data into our own memory (SDPAGE buffer).
SETDMA:   PUSH  HL
          PUSH  BC
          LD    HL,SDPAGE     ; The address (offset into page)
          LD    B,MN_PG       ; The page
          LD    C,S_DSKDM
          RST   30h
          POP   BC
          POP   HL
          RET

; ------- SDBREAD
; Load data from SDCard into memory. Handles both absolute and drive relative addresses.
; INPUTS   (SDDRV):   Drive (0 or 1 at the moment)
;          (SDADDR):  Start sector on the SDCard
;          A:         Drive access mode. This is the API call ID (zapi.asm)
;                     and will be either: A_DSKRD or A_DSKRW
;          HL:        Target address in application space RAM
;                     into which to the loaded store data
;          DE:        Number of bytes to load
SDBREAD:  LD    (DDMP_MDE),A    ; Save drive access mode (raw or translated)
          LD    A,D
          OR    A
          RRA
          LD    B,A    ; b = number of full blocks to load

          ; Need a partial block at the end?
          LD     A,D
          AND    1
          LD     D,A
          LD     (LASTB),DE

          ; Start loading blocks.
_bxtbl:   PUSH   BC
          PUSH   HL

          ; Tell API where to put data (HL)
          LD     C,A_DSKDM
          RST    30h

          ; Load the SDCard sector address
          LD     DE,(SDADDR)
          LD     HL,(SDADDR+2)
          LD     A,(SDDRV)
          LD     B,A

          LD     A,(DDMP_MDE)
          LD     C,A
          RST    30h                    ; Load the next page in to RAM

          ; Increase SD address and memory address by 512
          POP    HL
          INC    H                      ; Add 512
          INC    H
          PUSH   HL

          ; Move to next sector
          LD     HL,(SDADDR)
          INC    HL
          LD     (SDADDR),HL
          LD     A,L
          OR     H
          JR     NZ,_nxt1

          ; Overflow on the low 16 bit address. Inc high 16 bits.
          LD     HL,(SDADDR+2)
          INC    HL
          LD     (SDADDR+2),HL

          ; Any more full blocks to load?
_nxt1:    POP    HL
          POP    BC
          DJNZ   _bxtbl

          ; Full blocks loaded and addresses set up for any partial last block
          LD     DE,(LASTB)
          LD     A,E
          OR     D
          RET    Z

          ; DE contains number of bytes from last partial block. HL
          ; is the target address. Standard buffer and use from there.
          PUSH   HL
          CALL   SETDMA
          LD     DE,(SDADDR)
          LD     HL,(SDADDR+2)
          LD     A,(SDDRV)
          LD     B,A
          LD     A,(DDMP_MDE)
          LD     C,A
          RST    30h                  ; Load the next page in to RAM

          ; Copy to destination
          POP    HL                   ; Current load address. Need to know translate into a valid page address
          CALL   P_MAP                ; HL contains the target address

          LD     DE,SDPAGE            ; The partial block
          LD     BC,(LASTB)           ; Number of bytes
          EX     DE,HL
          LDIR                        ; Copy
          RET






          DSEG

; Buffer space for SDCard operations
SDPAGE     DEFS    512
