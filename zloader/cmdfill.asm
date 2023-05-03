; **********************************************
; Implements: 'F' (fill memory)
; Syntax:
;    F start len val;
;
; 'val' is currently a single byte value to be
; repeated through memory.
;
; Examples:
;    F 100 200 55   ; Fill 200h bytes from addr 100h
;                     with value 55h
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

  extrn  SETDMA
  extrn  E_BADPS
  extrn  main

  public FILL


PATTERN     EQU SCRATCH

; ------------------- _bfill
; INPUTS: HL   - Start address
;         BC   - Number of bytes
;          A   - The byte to write
_bfill:   PUSH  DE
          PUSH  AF
          CALL  P_MAPX
          LD    D,H             ; DE is the address in application memory
          LD    E,L
          INC   DE
          POP   AF
          LD    (HL),A
          LDIR
          POP   DE
          RET

; ------------------- fill
FILL:     CALL  GET_HEX          ; Address
          JR    Z, E_BADPS
          LD    D,H
          LD    E,L              ; DE: Address
          CALL  WASTESPC
          JR    Z,E_BADPS
          CALL  GET_HEX          ; Length
          JR    Z,E_BADPS
          LD    B,H              ; BC: Count
          LD    C,L
          CALL  WASTESPC
          JR    Z,E_BADPS
          CALL  INHEX_2
          JR    C,E_BADPS
          PUSH  AF
          LD    HL,_fill_msg
          CALL  PRINT
          LD    H,D
          LD    L,E
          CALL  WRITE_16
          LD    HL,_fill_sz
          CALL  PRINT
          LD    H,B
          LD    L,C
          CALL  WRITE_16
          LD    HL,_fill_wt
          CALL  PRINT
          POP   AF
          PUSH  AF
          CALL  WRITE_8
          CALL  NL

          ; Save values
          POP   AF
          LD    (PATTERN),A

          ; Fill memory...
          ; DE: address
          ; BC: count - Limit up to 64K - any size
          ; A:  fill value
          DEC   BC         ; To get a sensible range

_nfblk:   LD    A,B
          OR    C
          JR    Z,main      ; No bytes left to fill so exit
          PUSH  BC          ; Save total count
          LD    A,B
          AND   $C0
          JR    Z,_fsmblk   ; Block size is < 16K

          ; Dealing with a block > 16K so limit to 16K
          LD    BC,$4000

_fsmblk:  ; Decrease the total count by the number of bytes in this block.
          POP   HL          ; Get the original total remaining count
          LD    A,L         ; Will always be 0 after
          SUB   C
          LD    L,A
          LD    A,H
          SBC   B
          LD    H,A
          PUSH  HL          ; Save adjusted count on stack

          ; Add the block start address ready for the next block (if any)
          LD    H,D
          LD    L,E         ; Fill address -> HL
          LD    A,L
          ADD   C
          LD    L,A
          LD    A,H
          ADC   B
          LD    H,A
          PUSH  HL          ; Save the start of the next block. Stack: Ptr next block, remianing fill size

          ; Fill this block
          LD    A,(PATTERN) ; Fill value
          EX    DE,HL       ; Target address into HL
          CALL _bfill

          ; Get ready for the next block
          POP   DE
          POP   BC

_fone:    JR    _nfblk;


_fill_msg:   DEFB "Fill: ADDR: ",NULL
_fill_sz:    DEFB " LEN:",NULL
_fill_wt:    DEFB " WITH:",NULL
