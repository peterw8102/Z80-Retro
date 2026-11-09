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
  public LD_ST


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
           LD    A,ESC
           RST   08h
           LD    A,'8'
           RST   08h
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
          RET   Z                  ; Empty line: nothing to do (Z = ok)
          CP    ':'
          RET    NZ                ; Return on invalid start of line
          ; Accept this line. Format: [LEN 1][ADDR 2][REC_TYPE 1][DATA 2]+[CHKSM 1]
          ; The checksum is now checked (it was ignored: a character corrupted in
          ; transit ended up in memory, and with FP in the flash).
          CALL  INHEX_2            ; Length -> A

          JR    C, _recerr
          LD    B,A                ; Length into B
          LD    E,A                ; E = sum of all the record's bytes
          CALL  INHEX_4            ; Address - HL
          JR    C, _recerr
          LD    A,H
          ADD   A,E
          ADD   A,L
          LD    E,A
          CALL  INHEX_2            ; Record type
          JR    C, _recerr
          LD    D,A                ; D = type
          ADD   A,E
          LD    E,A
          LD    A,D
          OR    A
          JR    NZ, _rectyp        ; Any type other than 00 used to mean end of file
          LD    A,B                ; Check for zero length
          OR    A
          JR    Z,_recerr

          ; The upper two bits of the address identifies the page block - set
          ; up the page register so it's in the range 4000h-7fffh.
          ; P_MAPX (two pages): a record crossing a 16K boundary, or wrapping from
          ; FFFF to 0000, was written to whatever page was in bank 2 (ZLoader at boot).
          CALL  P_MAPX

          ; And write these bytes into that page
next_b:   CALL  INHEX_2            ; Get a hex byte
          JR    C, _recerr
          LD   (HL), A
          INC   HL
          ADD   A,E
          LD    E,A
          DJNZ  next_b
_rcks:    CALL  INHEX_2            ; Record checksum
          JR    C, _recerr
          ADD   A,E
          JR    NZ, _recerr        ; All bytes must sum to 0
          LD    A,D                ; Record type
          OR    A
          RET   Z                  ; 00 data: ok (Z, A=0)
          DEC   A
          JR    Z, _impeof         ; 01 end of file
          DEC   A                  ; 02 extended segment
          JR    Z, _rext
          DEC   A                  ; 03 start address: ignored
          JR    Z, _rok
          DEC   A                  ; 04 extended linear address
          JR    Z, _rext
          DEC   A                  ; 05 start linear address: ignored
          JR    Z, _rok
          JR    _recerr            ; Unknown type
_rext:    LD    A,C                ; Only base 0 accepted (64K space)
          OR    A
          JR    NZ, _recerr
_rok:     XOR   A
          RET

; Record with no data for memory: read the data only for the checksum.
; C = OR of the data (to insist on base 0 for types 02/04).
_rectyp:  LD    C,0
          LD    A,B
          OR    A
          JR    Z, _rcks
_rskip:   CALL  INHEX_2
          JR    C, _recerr
          LD    H,A
          OR    C
          LD    C,A
          LD    A,H
          ADD   A,E
          LD    E,A
          DJNZ  _rskip
          JR    _rcks

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
          LD    A,$FF             ; Load in progress = failed until it completes
          LD    (LD_ST),A
          XOR   A
          LD    (AUTO_RUN),A      ; Cancel auto-run mode
          LD    (LOAD_MODE),A     ; Default to HEX mode
          RET

LDF:      CALL  PREPLD
          LD    HL,_nxtblk$
          CALL  PRINT
          JR    cmd_bin

LDH:      CALL  PREPLD
          LD    HL,_record$
          CALL  PRINT
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
          CALL  P_MAPX         ; Two pages: a block may cross a 16K boundary
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

          PUSH  HL
          PUSH  AF
          LD    HL,(PROGADD)
          ; Move to the next 16K address
          LD    A,40h
          ADD   H
          LD    H,A
          ; Save the progress: PROGADD never changed, so from the third 16K
          ; block on the same page was written again.
          LD    (PROGADD),HL
          ; Remap this address to get the page (don't care about the address)
          CALL  P_MAPX
          LD    (PROGPG),A
          POP   AF
          POP   HL
          JR    _nxtblk

_fin:     INC   A
          JR    Z,E_REC        ; FF: block with a bad checksum (used to say "Complete")
          DEC   A
          DEC   A              ; 01: normal end of file
          JR    NZ,E_REC       ; Any other status is an error
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
          INC   A            ; FF: bad block checksum, do not use its data
          JR    Z,E_REC
          LD    B,80h        ; Size of block (bytes)
          LD    HL,FILE_BUF
          LD    DE,(INPTR)
_nexchr:  LD    A,(HL)       ; Copy to INPTR (INBUF)
          ; Line limit: a long line (or a file without LF) kept copying past
          ; INBUF and overwrote ZLoader's data.
          PUSH  HL
          LD    HL,INBUF+LINELEN-1
          OR    A
          SBC   HL,DE
          POP   HL
          JR    C,E_REC      ; DE > INBUF+LINELEN-1
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
          JR    Z, _hxok
          INC   A              ; Only A=FF is end of file; any error or odd line
          JR    Z, _eofhxok    ; used to be taken as the end (and BO ran it)
          JR    E_REC
_hxok:
          LD    DE,INBUF

_goon:    DJNZ  _nexchr
          ; End of block - should be a new block!
          LD    (INPTR),DE
          JR    _hexldn

_eofhxok: LD    A,1
          LD    (LD_ST),A      ; Load complete and correct
          LD    A,(AUTO_RUN)
          OR    A
          LD    HL,0        ; Run from address 0000h
          JR    NZ,PR_RUN

M_COMP:   LD    A,1
          LD    (LD_ST),A      ; Load complete and correct
          LD    HL,_COMPLETE
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
          CALL  PRINT
          LD    A,$FF
          LD    (LD_ST),A          ; Load in progress
          XOR   A
          LD    (LOAD_MODE),A      ; Force Intel Hex format load
          LD    HL,_BOOTHEX        ; From the default file
          LD    BC,8
          JR    LOADF

E_REC:    LD    HL,_REC_ERR
          JR    E_PRTERR


_WAITING     DEFB CR,LF,"Waiting...",NULL
_FNAME       DEFB "File? ",0
_nxtblk$     DEFB CR,"Block: ",ESC,'7',0
_record$     DEFB CR,"Record: ",ESC,'7',0
_REC_ERR     DEFB CR,LF,"Bad rec",NULL
_COMPLETE    DEFB CR,LF,"Complete",0
_BOOTHEX     DEFB "boot.ihx",0



          DSEG

LOAD_MODE: DEFB    0
LOAD_CNT   DEFW    0

PROGADD    DEFS    2
PROGPG     DEFS    1

FIN_CODE:  DEFB    0

; Status of the last L/LH/LF/BO load: 0 none, 1 good, FF failed or
; interrupted. FP refuses to program the flash when it is FF.
LD_ST:     DEFB    0
