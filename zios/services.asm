import ../zlib/defs.asm
import config.asm
import pcb_def.asm

          extrn  SD_INIT,SD_RBLK,SD_WBLK,SD_PRES,SD_SEL
          extrn  PAGE_MP
          extrn  P_ADJ,P_MAPX
          extrn  PRTERR
          extrn  SDMPADD,SDMPDSK
          extrn  HASPIO,HASVDU,SW_CFG
          extrn  ADD8T16
          extrn  DEVMAP
          extrn  P_ALLOC,P_FREE
          extrn  SDTXLTD
          extrn  SD_BKRD,SD_BKWR,SD_PRG
          extrn  PRINT_LN

          ; For debug
          ; public LD_MON,LD_PGSEL,LD_NOP,LD_STDSK,LD_QSDMP,LD_STDMA
          ; public LD_SDRD,LD_RWRD,LD_SDWR,LD_RWWR

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
; 11: Hardware inventory
; 12: Device inventory
;
;
; 12: Video Card Status
; 13: PIO Card Status
;
; All other registers are command specific.
AP_DISP:: LD     A,C
          RLA
          LD     A,C
          JR     C,_dosys

          CP     MAX_CODE
          RET    NC             ; Command out of range.

          ADD    A,A            ; A x 2
          PUSH   HL
          PUSH   DE
          LD     HL,JTAB        ; Look in the jump table for vector
          CALL   ADD8T16
          LD     E,(HL)
          INC    HL
          LD     D,(HL)
          EX     DE,HL          ; HL points to the handler
          POP    DE
          EX     (SP),HL        ; Restore HL but don't lose our target vector.
          RET                   ; Target is now on the stack so just have to return

          ; System requests (internal calls from ZLoader context)
_dosys:   CP     86h
          JR     Z,LD_EDMA

          ; Unknown function
          RET

; Jump to monitor. Basically an abort from the application. ZIOS memory pages are
; already mapped into CPU space so just restart the monitor
LD_MON:   LD     HL,_M_MONS
          LD     SP,SP_STK
          EI
          CALL   PRINT_LN
          JR     WARMSTRT

; ---------- LD_PGSEL (CMD 1)
; Map memory page
; D:  Physical memory page to map into the bank (0-255)
; E:  The bank number into which to map the physical memory (0-3).
; NOTE: Mapping page 0 and 3 are likely to be a problem!
LD_PGSEL: LD     A,E
          CP     3
          RET    NC             ; There are 4 pages but the last one can't be changed by the application
          PUSH   HL
          PUSH   BC
          LD     HL,PAGE_MP
          ADD    L
          LD     L,A            ; PAGE_MP will always be low in data store and not on a 256 byte boundary

          ; HL points to the correct place in the PAGE_MP map
          LD     (HL),D

          ; E is the bank into which page D should be mapped.
          BANK_I E,D

          POP    BC
          POP    HL
          RET

; ---------- LD_PGALC (CMD 2)
; Allocate a new page to be used by this application. No parameters.
; Returns a page that's not already been allocated to another
; process. At the moment there's only one process so this is easy.
; RETURN:  A:  Page number allocated. Zero if no page available.
; LD_PGALC: JR     P_ALLOC

; ---------- LD_PGFRE (CMD 3)
; Free a page. A contains the page to be freed.
; LD_PGFRE: JR     P_FREE

; --------- RX_CHR (CMD 2)
; Transmit the character in E
TX_CHR:   LD     A,E
          RST    08h
          RET

; --------- RX_CHR (CMD 3)
; Return next character in A (and C)
RX_CHR:   RST    10h
          LD     C,A
          RET

; --------- CHK_CHR (CMD 4)
; If there are any characters in the buffer then return the first one in A
CHK_CHR:  RST    18h
          RET

; --------- LD_STDSK (CMD 5)
; Map an application disk number (0-15) to a physical address (1024). By default each
; logical disk maps to a physical disk in the same location. This allows the map to change.
; This in turn allows an application to 'mount' logical drives
; E  - the physical device. 0: SDCard 1, 1: SDCard 2
; HL - the target slot on the SD card to be mapped (0-1023)
; D  - the logical disk to map to this physical block. 0-15
; Translate into a page and offset and store in local memory
LD_STDSK: LD     A,D           ; Check drive number in range
          CP     16
          RET    NC
          LD     D,E           ; Which SDCard
          JP     SDMPDSK

; --------- LD_QSDMP (CMD 6)
; Return current drive map.
; HL - Points to memory to receive the current map. Must provide sufficient
;      memory to receive the map table.
; Format for the output table is 3 bytes per entry.
;
; DB   dev_id - Matches the ID returned from the device map. If zero then no device mapped.
; DW   drive  - If the device supports multiple disks then the mapped virtual drive number
;
; The map table will contain up to 16 entries (and on entries HL MUST point to
; a memory region with at least 64 bytes available to accept the data)
LD_QSDMP: PUSH     BC
          PUSH     DE
          PUSH     HL

          ; Make application space HL pointer usable here.
          CALL    P_MAPX

          ; Clear the output address space.
          LD       B,64
          XOR      A
_clr:     LD       (HL),A
          INC      HL
          DJNZ     _clr
          POP      HL          ; Rewind to start of output area.
          PUSH     HL

          LD       BC,1000h    ; B: counter, C: drive number

_nxdrv:   EX       DE,HL       ; Save target address for output into DE
          ; Map drive referenced in disk 0.
          LD       A,C
          CALL     SDTXLTD     ; HL contains the selected drive, A the physical disk (sd0/1).
          EX       DE,HL       ; Make target address more usable
          LD       (HL),A      ; Store the device ID
          INC      HL
          LD       (HL),E
          INC      HL
          LD       (HL),D
          INC      HL
          INC      C           ; Next drive.
          DJNZ     _nxdrv

          POP      HL
          POP      DE
          POP      BC
          RET                  ; TBD

; --------- LD_STDMA (CMD 12)
; Record the address (in application space) of the SD Card DMA buffer
; HL - Address into application pages.
; Translate into a page and offset and store in local memory
LD_STDMA: CALL   P_ADJ
          LD     (DMA_PAGE),A
          LD     (DMA_ADDR),HL
          RET

; --------- LD_STDMA (CMD 86)
; Set the DMA address to a specific page and offset within that page.
;
; INPUTS: B   - The page holding the target buffer
;         HL  - The offset into that page
LD_EDMA:  LD     A,B
          LD     (DMA_PAGE),A
          ; Address will be mapped into bank 1 so add 4000h
          LD     A,40h
          ADD    H
          LD     H,A
          LD     (DMA_ADDR),HL
          RET

; --------- LD_SDRD (CMD 13)
; SDCard Read Block.
;   HLDE is the logical address (H: virtual disk 0-15, DE sector offset into drive)
; The result will be written to the currently defined DMA address. Note that for
; logical addresses there's no need to specify an SDCard number!
LD_SDRD:  CALL   SDMPADD
          ; HLDE is now the physical offset into the SDCard
          ; A contains the SDCard number for this mapping so move that to 'B'
          LD     B,A
          ; Drop through to a raw read

; --------- LD_RWRD (CMD 14)
; SDCard Read Block.
;   HLDE is the raw **SECTOR ADDRESS** (32 bits)
;   B is the physical SDCard to read.
;
; The result will be written to the currently defined DMA address
LD_RWRD:  ; Select the SDCard referenced in B
          LD     A,B                  ; Physical SDCard
          CALL   SD_SEL
          LD     BC,(DMA_ADDR)
          BANK   1,(DMA_PAGE)         ; Get the buffer page into bank 1
          CALL   SD_INIT
          CALL   SD_RBLK              ; Get the SDCard data
          RET


; --------- LD_SDRD (CMD 17)
; Read a 128 byte CP/M sized block from a 512 byte SDCard sector. This
; operation does the blocking/deblocking along with caching making CP/M
; faster and the BIOS smaller. NOTE: The CP/M 128 byte buffer to receive
; the data MUST NOT cross a 16K bank boundary!
; INPUTS:  HLDE   - Logical SDCard address (mapped drive)
;
; NOTE: The OFFSET portion of the logical address is in 128 byte blocks NOT
; 512 byte sectors as with the standard SDCard routines. This means the offset
; is 15 bits to address 4MB rather than 13 bits.
;   +------------+------------+------------+------------+
;   |  virtdisk  |  Not Used  | N |<---15 bit offset--->|
;   +------------+------------+------------+------------+
;
; LD_CPRD:  JP   SD_BKRD

; --------- LD_SDRD (CMD 17)
; Write a 128 byte CP/M sized block from a 512 byte SDCard sector. This
; operation does the blocking/deblocking along with caching making CP/M
; faster and the BIOS smaller. NOTE: The CP/M 128 byte buffer to receive
; the data MUST NOT cross a 16K bank boundary!
;
; NOTE: The OFFSET portion of the logical address is in 128 byte blocks NOT
; 512 byte sectors as with the standard SDCard routines. This means the offset
; is 15 bits to address 4MB rather than 13 bits.
;   +------------+------------+------------+------------+
;   |  virtdisk  |  Not Used  | N |<---15 bit offset--->|
;   +------------+------------+------------+------------+
; LD_CPWR:   JP   SD_BKWR

; --------- LD_CPFLSH (CMD 20)
; Purge dirty cache data to SDCard and discard all read cache data
; and reload from the device.
LD_CPPRG:  XOR  A
           JP   SD_PRG

; --------- LD_CPFLSH (CMD 20)
; Purge dirty cache data to SDCard and discard all read cache data
; and reload from the device.
LD_CPFLS:  LD    A,1
           JP   SD_PRG






; --------- LD_SDWR (CMD 10)
; SDCard Write Block.
;   HLDE is the **SECTOR ADDRESS** (32 bits)
; The result will be written to the currently defined DMA address
LD_SDWR:  CALL   SDMPADD
          ; A contains the SDCard number for this mapping so move that to 'B'
          LD     B,A
          ; Drop through to a raw write

; --------- LD_RWWR (CMD 11)
; SDCard Write Block.
;   HLDE is the **SECTOR ADDRESS** (32 bits)
; The result will be written to the currently defined DMA address
LD_RWWR:  LD     A,B
          CALL   SD_SEL
          PUSH   BC
          LD     BC,(DMA_ADDR)
          BANK   1,(DMA_PAGE)         ; Get the buffer address into page 1
          CALL   SD_WBLK
          POP    BC
          RET


; --------- SD_INV (CMD 11)
; Hardware inventory. Return basic system inventory information.
; The result is a bitmask returned in DE with the following bits
; currently defined:
;   bit  0: SDCard 1 present
;   bit  1: SDCard 2 present
;   bit  2: PIO Card installed
;   bit  3: Video card installed
;   bit  8: Config switch 0
;   bit  9: Config switch 1
;   bit 10: Config switch 2
SD_INV:   PUSH   HL
          CALL   SD_PRES     ; Sets up bit 0 and 1 for SD Card
          LD     E,A
          CALL   HASPIO
          JR     NZ,_chkvdu
          LD     A,4
          OR     E
          LD     E,A
_chkvdu:  CALL   HASVDU
          JR     NZ,_novdu
          LD     A,8
          OR     E
          LD     E,A
_novdu:   CALL   SW_CFG
          LD     D,A
          POP    HL
          RET

; --------- SD_DINV (CMD 12)
; Device inventory. This operation returns a set of logical devices available
; to the application along with information for each of those devices.
; INPUTS:
;    HL:   Pointer to memory to receive the device table. NOTE: this must point
;          to an area of memory AT LEAST 256 bytes. The structure returned
;          comprises a set of records, each 16 bytes in size with a maximum of
;          16 entries.
; OUTPUTS:
;     A:   Contains the number of entries in the output table (max 16)
;
; The output table comprises UP TO 16 entries, each 16 bytes in size. Each entry
; is a record with the following structure:
;    Byte:bit   Description
;     0-7       Name of the device (eg sd0)
;       8       System ID number to identify this device
;       9       Device type. Defined types:
;                  1: Block storage device (disk)
;                  2: Console input device
;                  3: Console output device
;      10       Flags (TBD - currently zero)
;   11-15       Device specific information (currently unused)
;
; This will become much more dynamic in future. Devices should register.
;
SD_DINV:  PUSH   BC
          PUSH   DE
          PUSH   HL
          CALL   P_MAPX     ; Get the target address mapped into memory (into HL)
          LD     DE,DEVMAP  ; System device map

          ; Only copy entries that are not empty
          LD     B,16       ; Maximum number of devices supported
          EX     DE,HL
_nxtdev:  LD     A,(HL)
          OR     A          ; If first byte is zero then there's no device
          JR     Z,_skpdev
          INC    A
          JR     Z,_enddv   ; 0xFF marks the end of the OS table.

          ; Copy this device to the destination
          PUSH   BC
          LD     BC,16
          LDIR
          POP    BC
_mvon:    DJNZ   _nxtdev

          ; Device slot empty. Skip this in HL
_skpdev:  LD     A,16
          CALL   ADD8T16
          JR     _mvon

          ; Put an end of table marker
_enddv:   XOR    A
          LD     (DE),A

          POP    HL
          POP    DE
          POP    BC
LD_NOP:   RET

JTAB:     DW     LD_MON       ; CMD 0  - Jump back to ZLoader
          DW     LD_PGSEL     ; CMD 1  - RAM Page Select (map)
          DW     P_ALLOC      ; CMD 2  - Allocate a new memory page
          DW     P_FREE       ; CMD 3  - Allocate a new memory page
          DW     TX_CHR       ; CMD 4  - Send character to serial port A
          DW     RX_CHR       ; CMD 5  - Rx character from serial port A, wait if none available
          DW     CHK_CHR      ; CMD 6  - Check for a character, return if available
          DW     LD_NOP       ; CMD 7  - NOP
          DW     LD_NOP       ; CMD 8  - NOP
          DW     LD_NOP       ; CMD 9  - NOP
          DW     LD_STDSK     ; CMD 10 - Map logical disk number
          DW     LD_QSDMP     ; CMD 11 - Query disk map
          DW     LD_STDMA     ; CMD 12 - Set memory address to receive SDCard data
          DW     LD_SDRD      ; CMD 13 - SDCard Read
          DW     LD_RWRD      ; CMD 14 - SDCard Raw read - absolute sector (512 byte) address
          DW     LD_SDWR      ; CMD 15 - SDCard Write
          DW     LD_RWWR      ; CMD 16 - SDCard Write to raw sector (unmapped)
          DW     SD_BKRD      ; CMD 17 - SDRead CP/M - optimised cached for CP/M 128 byte reads
          DW     SD_BKWR      ; CMD 18 - SDRead CP/M - optimised cached for CP/M 128 byte writes
          DW     LD_CPPRG     ; CMD 19 - Purge CP/M buffers
          DW     LD_CPFLS     ; CMD 20 - Purge and flush CP/M buffers
          DW     LD_NOP       ; CMD 21 - NOP
          DW     LD_NOP       ; CMD 22 - NOP
          DW     LD_NOP       ; CMD 23 - NOP
          DW     SD_INV       ; CMD 24 - Hardware inventory
          DW     SD_DINV      ; CMD 25 - Inventory of logical devices

MAX_CODE: EQU          ($ - JTAB) << 1

_M_MONS:  DEFB   10,13,"Monitor...", 0

.END
