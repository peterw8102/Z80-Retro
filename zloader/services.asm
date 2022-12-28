import ../zlib/defs.asm
import config.asm

          extrn  SD_INIT, SD_RBLK, SD_WBLK
          extrn  APP_STK, PAGE_MP
          extrn  PGADJ
          extrn  PRTERR

          extrn  DRVMAP
          extrn  PRINT_LN
          extrn  _IOERR
          extrn  MON_MP, SDPAGE
          extrn  WRITE_16

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
;  8: SD Card Write
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

          ; System requests (internal calls from ZLoader context)
          AND    7Fh
          CP     6
          JR     Z,LD_EDMA

          ; Unknown function
          RET

; Jump to monitor. Basically an abort from the application. The monitor page is
; already mapped into CPU space so just restart the monitor
LD_MON:   LD     HL, _M_MONS
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
LD_STDSK: LD     A,D
          CP     16
          RET    NC

          EX     DE,HL         ; DE <= physical block number
          LD     HL,DRVMAP     ; Mapping table base
          ADD    A,A           ; *2
          ADD    L             ; Find the correct offset
          LD     L,A
          LD     A,0
          ADC    H
          LD     H,A           ; HL now points to where to store the new physical block map for this logical drive

          ; Need to multiply DE by 32 (<<5)
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
          RET

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
LD_EDMA:  LD     A,(MON_MP)
          LD     (DMA_PAGE),A
          LD     HL,SDPAGE+0x4000
          LD     (DMA_ADDR),HL
          CALL   SD_INIT
          RET

; --------- LD_SDRD (CMD 7)
; SDCard Read Block.
;   HLDE is the logical address (H: virtual disk 0-15, DE sector offset into drive)
; The result will be written to the currently defined DMA address
LD_SDRD:  CALL   _mapdsk              ; HLDE is now the physical offset into the SDCard
          ; Drop through to a raw read

; --------- LD_SDRD (CMD 8)
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

; --------- LD_SDWR (CMD 9)
; SDCard Write Block.
;   HLDE is the **SECTOR ADDRESS** (32 bits)
; The result will be written to the currently defined DMA address
LD_SDWR:  CALL   _mapdsk
          ; Drop through to a raw write

; --------- LD_RWWR (CMD 10)
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

; --------- _mapdsk
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
_mapdsk:  LD     A,H        ; The upper 8 bits contain logical drive, needs to be mapped
                            ; fully qualified upper 10 bits of the sector number.
          ADD    A          ; x2 to give offset into DRVMAP

          ; A is now the offset into DRVMAP
          PUSH   DE         ; Stack <= LSWord
          LD     HL,DRVMAP
          ADD    L
          LD     L,A
          LD     A,0
          ADC    H
          LD     H,A        ; HL points to the correct logical drive offset.
          LD     E,(HL)
          INC    HL
          LD     D,(HL)     ; DE is the content of the drive table: replacement upper 10 bits of sector address
          EX     DE,HL

          POP    DE
          LD     A,$1F
          AND    D          ; Bottom 5 bits of sector number
          OR     L          ; Merge in the drive offset
          LD     D,A        ; Put back into D
          LD     L,H        ; And HL comes straight from the offset table
          RET

_M_MONS:  DEFB   10,13,"Monitor...", 0

          DSEG

DMA_PAGE   DEFS    1              ; Page the application wants us to write SDcard data to
DMA_ADDR   DEFS    2              ; Offset into the page of the DMA buyffer

.END
