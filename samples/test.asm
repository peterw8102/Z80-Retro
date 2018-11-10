; Test assembler file...
ORG 0

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
.start
  LD C,0xe0

  LD A,0x3e
.loop
  OUT (C), A
  RLA
  LD H,0xff
.loop2
  LD L,0xff
.loop3

  DEC L
  JR NZ,loop3
  DEC H
  JR NZ,loop2
  JP loop
  NOP
.data
  DEFB 0xc3, 0x3C, 0xAA, 0x55
  DEFB 0
