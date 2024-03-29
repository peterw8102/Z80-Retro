; Set of utilities used to manage virtual SDCard drive mappings.
import zlib.asm
import pcb_def.asm

    ; Function exports
    public SDTXLT,SDTXLTD,SDMPADD,SDMPDSK,SDPREP,SDMPRAW

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
; Relative addresses also contain a device number (0 or 1) for the
; the SDCards (needs to be more flexible than this to allow future
; floppy/hard disks).
;
; DRVMAP contains the values to replace 'v' in the resultant address.
;
; INPUTS:   HLDE   - Logical SDCard address
; OUTPUTS   A      - Physical SDCard (0 or 1)
;           HLDE   - Physical address in the specified SDCard
;
SDMPADD:  LD     A,H        ; The upper 8 bits contain logical drive, needs to be mapped
                            ; to the fully qualified upper 10 bits of the sector number.
          CALL   SDTXLT     ; Upper 16 bit address of SDCard now in HL

          PUSH   AF         ; Save SDCard number

          ; Mask most sig 10 bits into the lower sector offset
          LD     A,$1F
          AND    D          ; Bottom 5 bits of sector number
          OR     L          ; Merge in the drive offset
          LD     D,A        ; Put back into D
          LD     L,H        ; And HL comes straight from the offset table
          POP    AF         ; Restore SDCard number
          RET

; --------- SDTXLT
; Given a logical drive letter (0-15) return the current assigned drive offset (16 bits, least sig 5 bits zero)
; This basically returns the upper 16 bits of the base sector address for the mapped drive.
; INPUTS:   A - logical drive number, 0-15 (4 bits)
; OUTPUTS: HL - The upper 16 bit of the current drive mapping.
;           A - Physical device (SDCard 0 or 1)
;
; Other registers saved.
SDTXLT:   CALL   _toslot
          PUSH   DE         ; Stack <= LSWord
          LD     A,(HL)     ; Return the physical SDCard number
          INC    HL         ; Step past the SDCard number, not relevant here
          LD     E,(HL)     ; And get the sector address
          INC    HL
          LD     D,(HL)     ; DE is the content of the drive table: replacement upper 10 bits of sector address
          EX     DE,HL
          POP    DE
          RET

; --------- SDTXLTD
; Given a logical drive letter (0-15) return the current assigned disk number (0-1024).
; INPUTS:   A - logical drive number, 0-15 (4 bits)
; OUTPUTS: HL - The mapped disk number, 0-1024
;           A - Physical device (SDCard 0 or 1)
;
; Other registers save.
SDTXLTD:  CALL   SDTXLT
          JR     _div32

; --------- SDMPDSK
; Change drive mappings. Set logical drive slot (0-15) to point to one of
; 1024 virtual 4MB drives.
; INPUTS:   A - the drive slot (0-15) to be mapped
;           D - the physical SDCard (0 or 1)
;          HL - the virtual disk to map to this drive (1-1023)
SDMPDSK:  PUSH   DE
          PUSH   BC
          LD     B,D           ; Save the SDCard number
          EX     DE,HL         ; Save HL into DE (disk to be mapped to drive)
          CALL   _toslot       ; HL points to the slot to contain the mapping

          ; Store drive number (make sure it's 0 or 1)
          LD     A,$FE
          AND    B
          JR     NZ,_invsd

          LD     (HL),B        ; SDCard number
          INC    HL

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
_invsd:   POP    BC
          POP    DE
          RET


; --------- SDMPRAW
; Change drive mappings. Set logical drive slot (0-15) to point to one of
; 1024 virtual 4MB drives. In this case the drive offset is specified as
; an absolute offset to store in the DRVMAP table
; INPUTS:   A - the drive slot (letter) to be mapped
;           D - the physical DEVICE (0 or 1) - which SDCard
;          HL - the virtual disk to map to this drive
SDMPRAW:  PUSH   DE
          PUSH   BC
          LD     B,D           ; Save the SDCard number
          EX     DE,HL         ; Save HL into DE (disk to be mapped to drive)
          CALL   _toslot       ; HL points to the slot to contain the mapping

          ; Store drive number (make sure it's 0 or 1)
          LD     A,$FE
          AND    B
          JR     NZ,_invsd2

          LD     (HL),B
          INC    HL

          ; Make sure low 5 bits are zeros
          LD     A,$F8
          AND    E

          LD     (HL),A        ; Store in the address map
          INC    HL
          LD     (HL),D

_invsd2:  POP    BC
          POP    DE
          RET



; ------- SDPREP
; Initialise the drive map. Each virtual drive is mapped to the one of the first
; 16 vitual disks on the first SD card.
SDPREP:     PUSH   HL
            PUSH   BC
            LD     HL,DRVMAP
            LD     BC,1000h
            LD     DE,0020h        ; 0000 0000 0010 0000
_nxtdrv:    LD     (HL),C          ; SDCard 0
            INC    HL
            LD     (HL),E
            INC    HL
            LD     (HL),D
            INC    HL
            EX     DE,HL
            LD     A,20h           ; Add 20h to get to the next logical drive.
            CALL   ADD8T16
            EX     DE,HL
            DJNZ   _nxtdrv
            POP    BC
            POP    HL
            RET

; ------- _toslot
; Given a logical drive letter in A, return a pointer to the correct slot
; in the logical drive map in HL.
; INPUTS:   A - logical drive number 0-15
; OUTPUTS: HL - pointer to correct entry in the DRVMAP
_toslot:  PUSH   BC
          AND    $0F        ; Ignore out of range drive letters
          LD     B,A
          ADD    A          ; x2 to give offset into DRVMAP
          ADD    B          ; x3
          POP    BC

          ; A is now the offset into DRVMAP, add to base
          LD     HL,DRVMAP
          JP     ADD8T16    ; No need to return, no tidying.

; ----- Divide HL by 32 (5 bits)
_div32:   PUSH  AF
          LD    A,L
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
          POP   AF
          RET
