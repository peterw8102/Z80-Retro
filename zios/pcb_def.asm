; Imports that define the process control block layout.
;
          extrn PROC
          extrn PAGE_MP
          extrn P_FLAGS

          ; Register stored
          extrn R_PC
          extrn R_SP
          extrn R_AF
          extrn R_AF_P
          extrn R_BC_P
          extrn R_DE_P
          extrn R_HL_P
          extrn R_BC
          extrn R_DE
          extrn R_HL
          extrn R_IX
          extrn R_IY

; DRVMAP (Drive Map)
; Each entry in this table is three bytes:
;   byte 0:   The physical SDCard (0 or 1)
;   byte 1,2  The upper 10 bits of the 32 bit SD card address.
          extrn DRVMAP
          extrn ENDPCB

          extrn SZ_PCB

; SDCard DMA addresses
          extrn DMA_PAGE    ; Page the application wants us to write SDcard data to
          extrn DMA_ADDR    ; Offset into the page of the DMA buyffer
