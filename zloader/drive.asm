; Set of utilities used to manage virtual SDCard drive mappings.

    ; Utilities
    extrn  ADD8T16, WRITE_8,WRITE_16

    ; Function exports
    public SDTXLT,SDTXLTD,SDMPADD,SDMPDSK,SDPREP,SDMPRAW ;,SDDRV
    public DRVMAP

CSEG


; --------- SDMPADD
; Application is addressing a logical sector address, where a sector is 512 bytes. The
; address comprises a 13 bit sector offset and an 4 bit virtual disk number which provides
; an additional 10 bits to generate a 23 bit full sector address. 23 bits addressing
; 512 byte sectors gives total addressible space of 2^9 x 2^23 = 2^32 = 4GB. Input format
; for these commands:
;
;  +------ H -------+------ L -------+------ D -------+------ E -------+
;  |    0  | vdisk  |   0000 0000    |  000s  ssss    |  ssss  ssss    |
;  +----------------+----------------+----------------+----------------+
;
; vdisk gets expanded to 9 bits resulting in a sector address:
;
;  +------ H -------+------ L -------+------ D -------+------ E -------+
;  |    0  |   0    |   0vvv vvvv    |  vvvs  ssss    |  ssss  ssss    |
;  +----------------+----------------+----------------+----------------+
;
; 22 bit sector address which is then multiplied by 512 by the SDCard
; driver to give a byte address range of 4GB.
;
; DRVMAP contains the values to replace 'v' in the resultant address.
;
SDMPADD:  LD     A,H        ; The upper 8 bits contain logical drive, needs to be mapped
                            ; to the fully qualified upper 10 bits of the sector number.
          CALL   SDTXLT     ; Upper 16 bit address of SDCard now in HL

          ; Mask most sig 10 bits into the lower sector offset
          LD     A,$1F
          AND    D          ; Bottom 5 bits of sector number
          OR     L          ; Merge in the drive offset
          LD     D,A        ; Put back into D
          LD     L,H        ; And HL comes straight from the offset table
          RET

; --------- SDTXLT
; Given a logical drive letter (0-15) return the current assigned drive offset (16 bits, least sig 5 bits zero)
; This basically returns the upper 16 bits of the base sector address for the mapped drive.
; INPUTS:   A - logical drive number, 0-15 (4 bits)
; OUTPUTS: HL - The upper 16 bit of the current drive mapping.
;
; Other registers save.
SDTXLT:   CALL   _toslot
          PUSH   DE         ; Stack <= LSWord
          LD     E,(HL)
          INC    HL
          LD     D,(HL)     ; DE is the content of the drive table: replacement upper 10 bits of sector address
          EX     DE,HL
          POP    DE
          RET

; --------- SDTXLTD
; Given a logical drive letter (0-15) return the current assigned disk number (0-1024).
; INPUTS:   A - logical drive number, 0-15 (4 bits)
; OUTPUTS: HL - The mapped disk number, 0-1024
;
; Other registers save.
SDTXLTD:  CALL   SDTXLT
          JR     _div32

; --------- SDMPDSK
; Change drive mappings. Set logical drive slot (0-15) to point to one of
; 1024 virtual 4MB drives.
; INPUTS:   A - the drive slot (letter) to be mapped
;          HL - the virtual disk to map to this drive
SDMPDSK:  PUSH   DE
          EX     DE,HL         ; Save HL into DE (disk to be mapped to drive)
          CALL   _toslot       ; HL points to the slot to contain the mapping

          ; Need to multiply DE (virtual disk) by 32 (<<5)
          LD     A,D
          SLA    E             ; *2
          RLA
          SLA    E             ; *4
          RLA
          SLA    E             ; *8
          RLA
          SLA    E             ; *16
          RLA
          SLA    E             ; *32
          RLA
          LD     (HL),E        ; Store in the address map
          INC    HL
          LD     (HL),A
          POP    DE
          RET


; --------- SDMPRAW
; Change drive mappings. Set logical drive slot (0-15) to point to one of
; 1024 virtual 4MB drives. In this case the drive offset is specified as
; an absolute offset to store in the DRVMAP table
; INPUTS:   A - the drive slot (letter) to be mapped
;          HL - the virtual disk to map to this drive
SDMPRAW:  PUSH   DE
          EX     DE,HL         ; Save HL into DE (disk to be mapped to drive)
          CALL   _toslot       ; HL points to the slot to contain the mapping

          ; Make sure low 5 bits are zeros
          LD     A,$F8
          AND    E
          LD     E,A

          LD     (HL),E        ; Store in the address map
          INC    HL
          LD     (HL),D
          POP    DE
          RET



; ------- SDPREP
; Initialise the drive map. Each virtual drive is mapped to the one of the first
; 16 vitual disks.
SDPREP:     LD     HL,DRVMAP
            LD     B,16
            LD     DE,0020h        ; 0000 0000 0010 0000
_nxtdrv:    LD     (HL),E
            INC    HL
            LD     (HL),D
            INC    HL
            EX     DE,HL
            LD     A,20h           ; Add 20h to get to the next logical drive.
            CALL   ADD8T16
            EX     DE,HL
            DJNZ   _nxtdrv
            RET

; ------- _toslot
; Given a logical drive letter in A, return a pointer to the correct slot
; in the logical drive map in HL.
; INPUTS:   A - logical drive number 0-15
; OUTPUTS: HL - pointer to correct word in the DRVMAP
_toslot:  AND    $0F        ; Ignore out of range drive letters
          ADD    A          ; x2 to give offset into DRVMAP

          ; A is now the offset into DRVMAP, add to base
          LD     HL,DRVMAP
          JP     ADD8T16    ; No need to return, no tidying.

; ----- Divide HL by 32 (5 bits)
_div32:   LD    A,L
          SRL   H
          RRA
          SRL   H
          RRA
          SRL   H
          RRA
          SRL   H
          RRA
          SRL   H
          RRA
          LD    L,A
          RET

DSEG

; SDCard disk emulation. Treat the SD card as an array of 4M chunks (physical disks). The mapped
; space is 10bit (1024 physical spaces) each 4MB is size allowing a theoretical maximum of 4GB
; although at the moment we're limiting the code to the original 32bit address space mode of
; smaller cards. Generally 2GB cards are the target. Each physical space can be mapped into
; 16 logical drives. This allows an application to 'mount' up to 16x4MB partitions. By default
; the 16 available logical drives map to physical drives 1-16. Drive 0 is used by the loaded
; and so is not normally used by guest operating systems.
;
; Each entry in this table is the upper 10 bits of the 32 bit SD card address.
;
;THIS SPACE NEEDS TO BE INITIALISED ON RESTART.
DRVMAP     DEFS    16*2
