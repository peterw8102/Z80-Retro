; Test assembler file...
ORG 0

.start

  LD A, 0x01
  LD C, 0xff
.loop
  OUT (C),A
  RRA
  JP loop
  NOP



; .start
;   LD HL, data
;   LD C, 0x15
;   LD B, 0x4
; .loop
;   LD A, (HL)
;   OUT (C),A
;   INC HL
;   DJNZ loop
;   JP 0
;   NOP
; .data
;   DEFB 0xc3, 0x3C, 0xAA, 0x55
;   DEFB 0
