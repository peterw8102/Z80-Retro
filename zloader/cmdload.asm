; **********************************************
; Implements the following status commands:
;
;    L          ; Parse Intel hex from terminal
;    LH         ; Load intel hex file from RaspPi
;    LF         ; Load binary file from RaspPi
;    BO         ; Boot default image from RaspPi
;    BO-        ; Load default image from RaspPi
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

  extrn main
  extrn E_PRTERR
  extrn AUTO_RUN

  extrn E_BADPS,E_NOTF,E_ERROR

  public LDF,LDH,LDT,LOADF,BOOT,BOOTIX


; Aliases for the scratch area
FILE_BUF   EQU   SCRATCH



; _recerr: Return with A set to 0xfe and the Z flag clear (NZ)
_recerr:  XOR   A
          DEC   A
          DEC   A
          RET
; _impeof - End of file record. Return NZ to stop processing and 0xFF in A to indicate no error
_impeof:  XOR   A
          DEC   A
          RET

SHOWBCNT:  PUSH  HL
           PUSH  DE
           PUSH  BC
           PUSH  AF
           LD    HL,(REPTYPE)
           CALL  PRINT
           LD    HL,(LOAD_CNT)
           INC   HL
           LD    (LOAD_CNT),HL
           CALL  WRITE_D
           POP   AF
           POP   BC
           POP   DE
           POP   HL
           RET


; Function to process a single line of text from the in-buffer. Pulled this into a subroutine
; so it can be called from both a file load and a paste to terminal. Must only be called
; once the INBUF has been primed with text.
; INPUT:  INBUF contains a line of text ready to be processed.
; OUTPUT: Z  - return with with Z flag set if no error, otherwise Z clear for error, code in A (?)
_procln:  CALL  BUFCHR
          CP    ':'
          RET    NZ                ; Return on invalid start of line
          ; Accept this line. Format: [LEN 1][ADDR 2][REC_TYPE 1][DATA 2]+[CHKSM 1]
          CALL  INHEX_2            ; Length -> A

          JR    C, _recerr
          LD    B,A                ; Length into B
          CALL  INHEX_4            ; Address - HL
          JR    C, _recerr
          CALL  INHEX_2            ; Command - which should be 00. If not then EOF so end.
          JR    C, _recerr
          OR    A
          JR    NZ, _impeof
          LD    A,B                ; Check for zero length
          OR    A
          JR    Z,_recerr

          ; The upper two bits of the address identifies the page block - set
          ; up the page register so it's in the range 4000h-7fffh.
          CALL  P_MAP

          ; And write these bytes into that page
next_b:   CALL  INHEX_2            ; Get a hex byte
          JR    C, _recerr
          LD   (HL), A
          INC   HL
          DJNZ  next_b
          ; END of that record (ignoring checksum)
          ; Return with Z flag set (A=0)
          XOR   A
          RET

; When loading a binary file, the user can optionally enter a hex
; load address. The default address is 100h
cmd_bin:  LD    A,1
          LD    (LOAD_MODE),A     ; Binary mode
          CALL  WASTESPC
          JR    Z,cmd_load
          CP    '@'
          JR    NZ,cmd_load
          CALL  SKIPSPC
          CALL  INHEX_4
          JR    C,E_BADPS
          LD    (PROGADD),HL
          CALL  SKIPSPC
          JR    Z,E_BADPS
          CP    ','
          JR    NZ,E_BADPS
          JR    cmd_load


PREPLD:   LD    HL,100h           ; Default load address for binary data
          LD    (PROGADD),HL
          XOR   A
          LD    (AUTO_RUN),A      ; Cancel auto-run mode
          LD    (LOAD_MODE),A     ; Default to HEX mode
          RET

LDF:      CALL  PREPLD
          LD    HL,_nxtblk$
          LD    (REPTYPE),HL
          JR    cmd_bin

LDH:      CALL  PREPLD
          LD    HL,_record$
          LD    (REPTYPE),HL
          JR    cmd_load

; ---------------- LOAD
; Process all lines starting with a ':'. Hex load is always over the serial console
LDT:      XOR   A
          CALL  CNS_SET           ; Set serial console
          CALL  PREPLD
          XOR   A
          CALL  SETHIST
          LD    HL, _WAITING      ; Command line HEX input
          CALL  PRINT_LN
nextline: CALL  GET_LINE
          JR    Z, nextline       ; Ignore blank lines

          CALL  _procln           ; If Z flag is set then the line has been processed property
          JR    Z, nextline
          ; If A contains 0xFF then end of file (?)
          CALL  SETHIST           ; Know A is not zero at this point
          INC   A
          JR    Z,M_COMP
          JR    E_REC             ; Otherwise an error


; Use the CMD_B loader to read blocks of data. Rest of line is the name of the file
cmd_load: CALL  WASTESPC
          LD    HL,(INPTR)
          LD    BC,0
          ; Count number of characters until end of line OR space
_cnxt:    LD    A,(HL)
          OR    A
          JR    Z,_eofn
          CP    ' '
          JR    Z,_eofn
          INC   C
          INC   HL
          JR    _cnxt
_eofn:    LD    HL,(INPTR)

          ; This is also an entry point. Call this with:
          ;   HL: Pointer to file name
          ;   BC: Number of characters in the file name
LOADF:    LD    A,$10         ; Open file command
          LD    DE,FILE_BUF
          CALL  CMD_B         ; Open the file on the remote system

          ; A contains the status. Zero means the file exists, !0 it doesn't
          JR    NZ,E_NOTF

_blkdn:   ; File is OPEN. LOAD_MODE tells us whether to process input blocks as
          ; ASCII hex or not.
          LD    A,(LOAD_MODE)
          OR    A
          JR    Z,_hexld

          ; ---- BINARY LOAD
          ; loaded from the address stored in PROGADD. We're going to load into
          ; user pages though which are to be located in bank 1.
          LD    HL,0
          LD    (LOAD_CNT),HL  ; Clear block count
          LD    DE,(PROGADD)   ; Where to put the file we're loading (binary load)

          EX    DE,HL
          CALL  P_MAP          ; Map programme space address into user space.
          LD    (PROGPG),A     ; Save the selected page
          EX    DE,HL

_nxtblk:  LD    A,$11      ; Read block command
          LD    BC,0       ; No payload
          CALL  CMD_B
          CALL  SHOWBCNT   ; Display block count and increment
          OR    A
          JR    NZ,_fin
          ; DE now points to the first byte AFTER the downloaded block. If the upper 2 bits is
          ; no longer '01' then we've overflowed the logical page boundary so adjust.
          LD    A,D
          RLCA
          RLCA
          AND   03h        ; If it's still 01 then can just contine
          DEC   A
          JR    Z,_nxtblk
          LD    A,D        ; Map back to start of block 1
          AND   3fh
          OR    40h
          LD    D,A

          ; *************** The following looks WRONG. Should be using the page map *************
          PUSH  HL
          PUSH  AF
          LD    HL,(PROGADD)
          ; Move to the next 16K address
          LD    A,40h
          ADD   H
          LD    H,A
          ; Remap this address to get the page (don't care about the address)
          CALL  P_MAP
          LD    (PROGPG),A
          BANK  1
          POP   AF
          POP   HL
          ; *************** ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ************************
          JR    _nxtblk

_fin:     INC   A
          JR    M_COMP
          ; JR    main

; ------------------- _hex_load
; File is open. Read blocks into our own space and use to fill the input line buffer
; and call the HEX load for each line.
_hexld:   LD    DE,INBUF
          LD    (INPTR),DE
          XOR   A
          LD    (FIN_CODE),A
_hexldn:  LD    A,(FIN_CODE)
          OR    A
          JR    NZ,E_REC
          LD    A,$11        ; Read block command
          LD    BC,0         ; No payload
          LD    DE,FILE_BUF  ; Into our 128 byte buffer
          CALL  CMD_B
          LD    (FIN_CODE),A ; Status for THIS block - but doesn't mean this block doesn't include data.
          LD    B,80h        ; Size of block (bytes)
          LD    HL,FILE_BUF
          LD    DE,(INPTR)
_nexchr:  LD    A,(HL)       ; Copy to INPTR (INBUF)
          LD    (DE),A
          INC   HL
          CP    0Dh
          JR    Z,_goon
          INC   DE
          CP    0Ah          ; Newline?
          JR    NZ,_goon
          ; Have a full line in the buffer so process now
          PUSH  HL
          PUSH  BC
          CALL  SHOWBCNT
          XOR   A
          DEC   DE
          LD    (DE),A

          LD    HL,INBUF
          LD    (INPTR),HL
          CALL  _procln
          POP   BC
          POP   HL
          JR    NZ, _eofhxok   ; Saw end of hex data record
          LD    DE,INBUF

_goon:    DJNZ  _nexchr
          ; End of block - should be a new block!
          LD    (INPTR),DE
          JR    _hexldn

_eofhxok: LD    A,(AUTO_RUN)
          OR    A
          LD    HL,0        ; Run from address 0000h
          JR    NZ,PR_RUN

M_COMP:   LD    HL,_COMPLETE
          JR    E_PRTERR

; ----- BOOT
; Load default image from the Raspberry Pi (Hex format) but
; DO NOT RUN THE IMAGE
BOOT:     LD    A,1
          LD    (AUTO_RUN),A       ; Set auto-run mode

; ----- BOOTIX
; Load default image from Raspberry Pi and if AUTO_RUN is sset
; then execute that image.
BOOTIX:   LD    HL,_record$
          LD    (REPTYPE),HL
          XOR   A
          LD    (LOAD_MODE),A      ; Force Intel Hex format load
          LD    HL,_BOOTHEX        ; From the default file
          LD    BC,8
          JR    LOADF



E_REC:    LD    HL,_REC_ERR
          JR    E_PRTERR




_WAITING     DEFB CR,LF,"Waiting...",NULL
_FNAME       DEFB "File? ",0
_nxtblk$     DEFB CR,"Block: ",0
_record$     DEFB CR,"Record: ",0
_REC_ERR     DEFB CR,LF,"Bad rec",NULL
_COMPLETE    DEFB CR,LF,"Complete",0
_BOOTHEX     DEFB "boot.ihx",0



          DSEG

LOAD_MODE: DEFB    0
LOAD_CNT   DEFW    0

PROGADD    DEFS    2
PROGPG     DEFS    1

FIN_CODE:  DEFB    0
REPTYPE:   DEFW    0
