          public PROC
          public PAGE_MP

          ; Register stored
          public R_PC,R_SP,R_AF,R_AF_P
          public R_BC_P,R_DE_P,R_HL_P
          public R_BC,R_DE,R_HL,R_IX,R_IY

          ; DRVMAP (Drive Map)
          ; Each entry in this table is three bytes:
          ;   byte 0:   The physical SDCard (0 or 1)
          ;   byte 1,2  The upper 10 bits of the 32 bit SD card address.
          ;
          ;THIS SPACE NEEDS TO BE INITIALISED ON RESTART.
          public DRVMAP
          public ENDPCB

          public DMA_PAGE    ; Page the application wants us to write SDcard data to
          public DMA_ADDR    ; Offset into the page of the DMA buyffer


          public SZ_PCB


          DSEG

; ------------ PROCESS CONTROL DATA -------------
; Process specific storage. Currently there's a single instance of this data.
; To "context switch" copy this data to a process backup and re-initialise
; for a separate process. In theory any number of processes can be supported
; however there needs to be a way to switch focus between processes.
PROC:      EQU     $
PAGE_MP:   DEFS    4              ; Used to restore/track active pages

; ---------- DMA_??? ----------
; Each process needs its own DMA target address for SDCard data. Make this part
; of the PCB data.
DMA_PAGE   DEFS    1              ; Page the application wants us to write SDcard data to
DMA_ADDR   DEFS    2              ; Offset into the page of the DMA buyffer


; Suspenended execution registers. NOTE - application page 0
; has the applications AF and PC.
R_PC       DEFS    2
R_SP       DEFS    2 ; Initial application stack is NOT the same as ours
R_AF       DEFS    2
R_AF_P     DEFS    2
R_BC_P     DEFS    2
R_DE_P     DEFS    2
R_HL_P     DEFS    2
R_BC       DEFS    2
R_DE       DEFS    2
R_HL       DEFS    2
R_IX       DEFS    2
R_IY       DEFS    2

; SDCard disk emulation. Treat the SD card as an array of 4M chunks (physical disks). The mapped
; space is 10bit (1024 physical spaces) each 4MB is size allowing a theoretical maximum of 4GB
; although at the moment we're limiting the code to the original 32bit address space mode of
; smaller cards. Generally 2GB cards are the target. Each physical space can be mapped into
; 16 logical drives. This allows an application to 'mount' up to 16x4MB partitions. By default
; the 16 available logical drives map to physical drives 1-16. Drive 0 is used by the loaded
; and so is not normally used by guest operating systems.
;
; Each entry in this table is three bytes:
;   byte 0:   The physical SDCard (0 or 1)
;   byte 1:   Flags (bitmapped)
;      bit 0: Read-only
;   byte 1,2  The upper 10 bits of the 32 bit SD card address.
;
;THIS SPACE NEEDS TO BE INITIALISED ON RESTART.
DRVMAP     DEFS    16*4

ENDPCB     EQU     $
SZ_PCB     EQU   ENDPCB-PROC
