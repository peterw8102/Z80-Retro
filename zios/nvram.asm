import ../zlib/defs.asm
import config.asm

; nvram.asm
; Provide abstracted access to the 56 bytes of user NVRAM provided
; by the RTC chip.


          public NVRD,NVLD,NVCHK,NVSAV,NVRAM
          extrn  RTC_MRD,RTC_MWR

          CSEG

; ------ NVLD ------
; Read the full 56 bytes of data from the RTC RAM into
; memory pointed to by HL (which must be mapped into
; Z80 memory!). If HL is 0 (null) then copy into our own
; buffer space and return the pointer to the called.
; INPUT:  HL  - target location for data, or zero to
;               use system buffer.
; OUTPUT: HL  - pointer to loaded data (same as input
;               if buffer provided by caller).
; AF, HL not preserved.
NVRD:     LD     A,H
          OR     L
          JR     NZ,.usebuf
          LD     HL,NVRAM
.usebuf:  PUSH   HL          ; Save the output buffer address to return.
          PUSH   BC
          LD     BC,3800h    ; Read 56 bytes from offset 0
          CALL   RTC_MRD
          POP    BC
          POP    HL
          RET


; ------ NVLD ------
; Load and verify the first 16 reserved bytes of NVRAM. This should be called as part
; of ZIOS cold start initialisation and after each reset.
; OUTPUT:  HL  - Return a pointer to the NVRAM structure
;
NVLD:     LD     HL,NVRAM
          LD     BC,1000h    ; Read 16 bytes from offset 0
          CALL   RTC_MRD

          ; Check chksum
          CALL   NVCHK
          CALL   NZ,NVINI
          LD     HL,NVRAM
          RET

; ------- NVINI
; Initialise the NVRAM structure
NVINI:   LD      HL,NVRAM
         LD      (HL),CFG_DEF
         INC     HL
         LD      B,14
         XOR     A
_nvcl:   LD      (HL),A
         INC     Hl
         DJNZ    _nvcl
         ; DROP THROUGH TO SAVE THE INITIALISED NVRAM

; ------- NVSAV
; Save the content of the NVRAM area to the RTC
NVSAV:   CALL   NVCALC
         LD     HL,NVRAM
         LD     BC,1000h    ; WRITE 16 bytes from offset 0
         CALL   RTC_MWR
         RET

; ------- _NVC
; Calculate the NVRAM checksum. 1s complement of the sum of the first 15 bytes
; Return: HL - Points to the first byte after the data
;         A  - Calculate checksum
_NVC:     PUSH  BC
          LD    HL,NVRAM
          LD    B,15
          LD    C,0
_nvnx:    LD    A,(HL)
          INC   HL
          CPL
          ADD   C
          LD    C,A
          DJNZ  _nvnx
          POP   BC
          RET

; ------- NVCHK
; Checksum the first 15 bytes of the NV memory and compare with the check sum in the last byte. Calculated on
; the 1s complement of each data byte.
; Return: A Calculate checksum
;         Z is set on exit of the checksum matches the stored value.
NVCHK:   CALL   _NVC
         CP     (HL)
         RET

; ------- NVCALC
; Calculate and store the checksum for the current NVRAM data
NVCALC:  CALL   _NVC
         LD     (HL),A
         RET


          DSEG

; 56 bytes of NV RAM. First 16 bytes used by the loader, others available by the application.
NVRAM      DEFS    56
