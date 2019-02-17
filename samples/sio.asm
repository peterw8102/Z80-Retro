  ; Test assembler file...
ORG 0

; DEFC SIO_A_D=$0081
; DEFC SIO_A_C=$0080
; DEFC SIO_B_D=$0083
; DEFC SIO_B_C=$0082

DEFC SIO_A_D=$0081
DEFC SIO_A_C=$0083
DEFC SIO_B_D=$0080
DEFC SIO_B_C=$0082

DEFC LED=$00A0

.start
  LD    A,1
  OUT   (LED),A

; Initialise SIO channel 1
  LD  HL,$2000
  LD  SP,HL

; WR0 - Error reset
  LD A, 00110000b
  OUT (SIO_A_C),A

  ; Channel reset
  LD A, 018h
  OUT (SIO_A_C),A

  ; Set up mode in WR1
  LD A, 01h
  OUT (SIO_A_C),A
  LD A, 00000000b
  OUT (SIO_A_C),A

  ; Set up mode in WR3
  LD A, 03h
  OUT (SIO_A_C),A
  LD A, 11000001b
  OUT (SIO_A_C),A

  ; Set up mode in WR4
  LD A, 004h
  OUT (SIO_A_C),A
  LD A, 11000100b
  OUT (SIO_A_C),A

  ; WR5
  LD A, 005h
  OUT (SIO_A_C),A
  LD A, 01101000b ; 0D8h
  OUT (SIO_A_C),A

  LD A, 01h
  OUT (SIO_B_C),A
  LD A,00000100b
  OUT (SIO_B_C),A

  LD A, 02h
  OUT (SIO_B_C),A
  LD A, 0h
  OUT (SIO_B_C),A

  LD   A,2
  OUT (LED),A

; Poll for inbound characters

; Start writing data OUT
.sloop_1
  LD A,21h
  LD B,1fh

.sloop_2
  OUT (LED),A
  OUT (SIO_A_D), A
  INC A
  CALL wait
  DJNZ sloop_2
  LD A,0Ah
  OUT (SIO_A_D), A
  CALL wait
  LD A,0Dh
  OUT (SIO_A_D), A
  CALL wait

  JP sloop_1
  NOP

.wait
  PUSH HL
  LD H,0xff
.loop2
  LD L,0xff
.loop3
  DEC L
  JR NZ,loop3
  DEC H
  JR NZ,loop2
  POP HL
  RET




.data
  DEFB 0xc3, 0x3C, 0xAA, 0x55
  DEFB 0
