import ../zlib/defs.asm
import config.asm

          extrn  SD_INIT,SD_RBLK,SD_WBLK,SD_PRES
          extrn  PAGE_MP
          extrn  PGADJ
          extrn  PRTERR
          extrn  SDMPADD,SDTXLTD

          extrn  SDPAGE

          public  AP_DISP

CSEG

; IO system dispatcher. Registers:
; A: Not used but NOT saved. Can be used for outputs
; C: Command
;  1: Select RAM page
;  2: TX serial port A
;  3: RX serial port A (wait for char)
;  4: Check for character on RXA
;  5: Map logical disk address (10 bits)
;  6: Set disk DMA address
;  7: SD Card Read
;  8: SD Card Read Raw
;  9: SD Card Write
; 10: SD Card Write Raw
; 11: SD Card Status
; 12: Video Card Status
; 13: PIO Card Status
;
; All other registers are command specific.
AP_DISP:  LD     A,C
          OR     A
          JR     Z,LD_MON       ; CMD 0  - Jump back to ZLoader
          DEC    C
          JR     Z,LD_PGSEL     ; CMD 1  - RAM Page Select (map)
          DEC    C
          JR     Z,TX_CHR       ; CMD 2  - Send character to serial port A
          DEC    C
          JR     Z,RX_CHR       ; CMD 3  - Rx character from serial port A, wait if none available
          DEC    C
          JR     Z,CHK_CHR      ; CMD 4  - Check for a character, return if available
          DEC    C
          JR     Z,LD_STDSK     ; CMD 5  - Map logical disk number
          DEC    C
          JR     Z,LD_STDMA     ; CMD 6  - Set memory address to receive SDCard data
          DEC    C
          JR     Z,LD_SDRD      ; CMD 7  - SDCard Read
          DEC    C
          JR     Z,LD_RWRD      ; CMD 8  - SDCard Raw read - absolute sector (512 byte) address
          DEC    C
          JR     Z,LD_SDWR      ; CMD 9  - SDCard Write
          DEC    C
          JR     Z,LD_RWWR      ; CMD 10 - SDCard Write to raw sector (unmapped)
          DEC    C
          JR     Z,SD_PRES      ; CMD 11 - SDCard card status
          DEC    C
          JR     Z,LD_VDU       ; CMD 12 - Video card status
          DEC    C
          JR     Z,LD_PIO       ; CMD 13 - PIO card status

          ; System requests (internal calls from ZLoader context)
          AND    7Fh
          CP     6
          JR     Z,LD_EDMA

          ; Unknown function
          RET

; Jump to monitor. Basically an abort from the application. The monitor page is
; already mapped into CPU space so just restart the monitor
LD_MON:   LD     HL,_M_MONS
          LD     SP,SP_STK
          EI
          JR     PRTERR

; Transmit the character in E
TX_CHR:   LD     A,E
          RST    08h
          RET

; Return next character in A
RX_CHR:   RST    10h
          LD     C,A
          RET

; If there are any characters in the buffer then return the first one in A
CHK_CHR:  RST    18h
          RET

; ---------- LD_PGSEL (CMD 1)
; Map memory page
; D:  Physical memory page to map into the page (0-255)
; E:  The block number into which to map the physical memory (0-3).
; NOTE: Mapping page 0 and 3 are likely to be a problem!
LD_PGSEL: LD     A,E
          CP     3
          RET    NC             ; There are 4 pages but the last one can't be changed by the application
          PUSH   HL
          PUSH   BC
          LD     HL,PAGE_MP
          ADD    L
          LD     L,A            ; PAGE_MP will always be low in data store and not on a 256 byte boundary
          LD     A,D
          LD     (HL),A         ; Save the page we're about to map
          LD     A,PG_PORT0     ; Calculate the right port number for the requested block
          ADD    E
          LD     C,A            ; C has the port number
          LD     A,D            ; The page to select
          OUT    (C),A          ; Make the change
          POP    BC
          POP    HL
          RET

; --------- LD_STDSK (CMD 5)
; Map an application disk number (0-15) to a physical address (1024). By default each
; logical disk maps to a physical disk in the same location. This allows the map to change.
; This in turn allows an application to 'mount' logical drives
; HL - the target slot on the SD card to be mapped (0-1023)
; D  - the logical disk to map to this physical block. 0-15
; Translate into a page and offset and store in local memory
LD_STDSK: LD     A,D           ; Check drive number in range
          CP     16
          RET    NC
          JP     SDTXLTD

; --------- LD_STDMA (CMD 6)
; Record the address (in application space) of the SD Card DMA buffer
; HL - Address into application pages.
; Translate into a page and offset and store in local memory
LD_STDMA: CALL   PGADJ
          LD     (DMA_PAGE),A
          LD     (DMA_ADDR),HL
          CALL   SD_INIT
          RET

; --------- LD_STDMA (CMD 86)
; Set the DMA address to the SDPage address. Fixed buffer so no
; required parameters.
LD_EDMA:  LD     A,MN_PG
          LD     (DMA_PAGE),A
          LD     HL,SDPAGE+0x4000
          LD     (DMA_ADDR),HL
          CALL   SD_INIT
          RET

; --------- LD_SDRD (CMD 8)
; SDCard Read Block.
;   HLDE is the logical address (H: virtual disk 0-15, DE sector offset into drive)
; The result will be written to the currently defined DMA address
LD_SDRD:  CALL   SDMPADD              ; HLDE is now the physical offset into the SDCard
          ; Drop through to a raw read

; --------- LD_SDRD (CMD 9)
; SDCard Read Block.
;   HLDE is the raw **SECTOR ADDRESS** (32 bits)
; The result will be written to the currently defined DMA address
LD_RWRD:  PUSH   BC
          LD     BC,(DMA_ADDR)
          LD     A,(DMA_PAGE)         ; Get the buffer page into block 1
          OUT    (PG_PORT0+1),A
          CALL   SD_RBLK             ; Get the SDCard data
          POP    BC
          RET

; --------- LD_SDWR (CMD 10)
; SDCard Write Block.
;   HLDE is the **SECTOR ADDRESS** (32 bits)
; The result will be written to the currently defined DMA address
LD_SDWR:  CALL   SDMPADD
          ; Drop through to a raw write

; --------- LD_RWWR (CMD 11)
; SDCard Write Block.
;   HLDE is the **SECTOR ADDRESS** (32 bits)
; The result will be written to the currently defined DMA address
LD_RWWR:  PUSH   BC
          LD     BC,(DMA_ADDR)
          LD     A,(DMA_PAGE)         ; Get the buffer address into page 1
          OUT    (PG_PORT0+1),A
          CALL   SD_WBLK
          POP    BC
          RET


; --------- LD_SDSTT (CMD 12)
; SDCard presense
; Returns the presense or absense of the two SDCards slots. NOTE:
; The hardware can't distinguish between card present and adaptor
; missing due to the way the adaptor hardware works. As such it's
; recommended that slot 0 at least have an SDCard adaptor even if
; not used.
;
; The API takes no parameters and returns a two bit result in the
; accumulator:
;  Bit 0: Card 0 presense
;  Bit 1: Card 1 presense
; The value of each bit will be either:
;     0: There is an adaptor fitted but there's no card inserted
;     1: Either there's no adaptor OR there is an adaptor and there
;        is a card inserted into the adaptor.
; NOTE This function is implemented directly by SD_PRES in the
; SDCard library so currently no implementation here!


; --------- LD_VDU (CMD 13)
; VDU/Graphic Card Presense
; Returns the presense or absense of the video card with the
; `Z` flag. `Z` is true if there's a video card installed.
LD_VDU:     LD       A,$FF
            OUT      (PG_PORT0+1), A  ; Map VDU memory into CPU space

            ; Try to write to memory
            LD       HL,$55AA
            LD       ($7FFE),HL
            LD       DE,($7FFE)
            LD       A,E
            CP       $AA
            RET      NZ
            LD       A,D
            CP       $55
            RET


; --------- LD_PIO (CMD 14)
; PIO Card Presense
; Returns the presense or absense of the PIO Card. Haven't worked out to do that. Currently
; this is a NOP until the PIO has been tested.
; `Z` flag. `Z` is true if there's a video card installed.
LD_PIO:     XOR      A
            INC      A
            RET



_M_MONS:  DEFB   10,13,"Monitor...", 0

          DSEG

DMA_PAGE   DEFS    1              ; Page the application wants us to write SDcard data to
DMA_ADDR   DEFS    2              ; Offset into the page of the DMA buyffer

.END
