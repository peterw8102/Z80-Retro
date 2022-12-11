import ../zlib/defs.asm
import config.asm

          extrn  SD_INIT, SD_RBLK, SD_WBLK
          extrn  APP_STK, PAGE_MP
          extrn  PGADJ

          extrn  PGREST
          extrn  ENDDIS,ERRDIS
          extrn  DRVMAP

          public DISPATCH

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
DISPATCH: LD     (APP_STK),SP   ; Save the application stack
          LD     SP,SP_STK      ; Replace with supervisor stack
          EI
          ; A selects the function.
          DEC    C
          JR     Z,LD_PGSEL     ; CMD 1 - RAM Page Select (map)
          DEC    C
          JR     Z,TX_CHR       ; CMD 2 - Send character to serial port A
          DEC    C
          JR     Z,RX_CHR       ; CMD 3 - Rx character from serial port A, wait if none available
          DEC    C
          JR     Z,CHK_CHR      ; CMD 4 - Check for a character, return if available
          DEC    C
          JR     Z,LD_STDSK     ; CMD 5 - Map logical disk number
          DEC    C
          JR     Z,LD_STDMA     ; CMD 6 - Set memory address to receive SDCard data
          DEC    C
          JR     Z,LD_SDRD      ; CMD 7 - SDCard Read
          DEC    C
          JR     Z,LD_SDWR      ; CMD 8 - SDCard Write

          ; Unknown function
          JR     ENDDIS

; Transmit the character in E
TX_CHR:   LD     A,E
          RST    08h
          JR     ENDDIS

; Return next character in A
RX_CHR:   RST    10h
          LD     C,A
          JR     ENDDIS

; If there are any characters in the buffer then return the first one in A
CHK_CHR:  RST    18h
          JR     ENDDIS

; ---------- LD_PGSEL (CMD 1)
; Map memory page
; D:  Physical memory block to map into the page (0-255)
; E:  The page number into which to map the physical memory (0-3).
; NOTE: Mapping page 0 is likely to be a problem!
LD_PGSEL: LD     A,E
          CP     4
          JR     NC,ERRDIS      ; Only 4 pages. Store the page we're selecting
          LD     HL,PAGE_MP
          ADD    L
          LD     L,A             ; PAGE_MP will always be low in data store and not on a 256 byte boundary
          LD     A,D
          LD     (HL),A         ; Save the page we're about to map
          LD     A,PG_PORT0     ; Calculate the right port number for the requested block
          ADD    E
          LD     C,A            ; C has the port number
          LD     A,D            ; The page to select
          OUT    (C),A          ; Make the change
          JR     ENDDIS

; --------- LD_STDSK (CMD 5)
; Map an application disk number (0-15) to a physical address (1024). By default each
; logical disk maps to a physical disk in the same location. This allows the map to change.
; This in turn allows an application to 'mount' logical drives
; HL - the target slot on the SD card to be mapped (0-511)
; D  - the logical disk to map to this physical block. 0-15
; Translate into a page and offset and store in local memory
LD_STDSK: LD     A,D
          CP     16
          JR     NC,ENDDIS

          EX     DE,HL         ; DE <= physical block number
          LD     HL,DRVMAP     ; Mapping table base
          ADD    A,A           ; *2
          ADD    L             ; Find the correct offset
          LD     L,A
          LD     A,0
          ADC    H
          LD     H,A           ; HL now points to where to store the new physical block map for this logical drive

          ; Need to multiply DE by 64 (<<6)
          XOR    A
          RR     D
          RR     E             ; LSB of D now in MSB of E and LSB E in Carry (divide by2)
          RRA
          RR     D
          RR     E
          RRA                  ; A now contains the new content of E and E contains the new content for D
          LD     D,E
          LD     E,A           ; *64

          LD     (HL),E        ; Store in the address map
          INC    HL
          LD     (HL),D
          JR     ENDDIS

; --------- LD_STDMA (CMD 6)
; Record the address (in application space) of the SD Card DMA buffer
; A  - Physical page number into which to write data
; HL - Offset (14 bits) into the physical page number (WHY?!?!!??!?!)
; Translate into a page and offset and store in local memory
LD_STDMA: CALL   PGADJ
          LD     (DMA_PAGE),A
          LD     (DMA_ADDR),HL
          JR     ENDDIS

; --------- LD_SDRD (CMD 7)
; SDCard Read Block.
;   HLDE is the **SECTOR ADDRESS** (32 bits)
; The result will be written to the currently defined DMA address
LD_SDRD:  CALL   _mapdsk              ; HLDE is now the physical offset into the SDCard

          LD     BC,(DMA_ADDR)
          LD     A,(DMA_PAGE)         ; Get the buffer page into block 1
          OUT    (PG_PORT0+1),A

          CALL   SD_INIT
          CALL   SD_RBLK             ; Get the SDCard data

          CALL   PGREST              ; Reset the application memory page
          JR     ENDDIS

; --------- LD_SDWR (CMD 8)
; SDCard Write Block.
;   HLDE is the **SECTOR ADDRESS** (32 bits)
; The result will be written to the currently defined DMA address
LD_SDWR:  CALL   _mapdsk

          LD     BC,(DMA_ADDR)
          LD     A,(DMA_PAGE)         ; Get the buffer address into page 1
          OUT    (PG_PORT0+1),A

          CALL   SD_INIT
          CALL   SD_WBLK

          CALL   PGREST               ; Reset the application memory page
          JR     ENDDIS

; --------- _mapdsk
; Application is writing to logical 32 bit address in HLDE. The upper 10 bits is the logical
; disk number which needs to be mapped to a physical 4MB block. The logical disk number is
; only 4 bits allowing 16 logical drives. Need H bits 0-1 and L bits 6-7 (unfortunate)
_mapdsk:  LD     A,$C0
          AND    L
          RR     H          ; LS bit of H
          RRA               ; Into MSB of A
          RR     H          ; And again
          RRA               ; A = logical disk * 16. Need *2 to get offset into drive table.
          RRCA
          RRCA
          RRCA              ; Divide by 8 to get offset into DRVMAP table
          ; A is now the offset into DRVMAP
          PUSH   DE         ; Stack <= LSWord
          PUSH   HL         ; Stack <= MSWord
          LD     HL,DRVMAP
          ADD    L
          LD     L,A
          LD     A,0
          ADC    H
          LD     H,A        ; HL points to the correct logical drive.
          LD     E,(HL)
          INC    HL
          LD     D,(HL)
          POP    HL
          LD     A,L
          AND    $3F
          OR     E
          LD     L,A
          LD     H,D
          POP    DE
          ; HLDE is now the modified absolute disk address
          RET

          DSEG

DMA_PAGE   DEFS    1              ; Page the application wants us to write SDcard data to
DMA_ADDR   DEFS    2              ; Offset into the page of the DMA buyffer

.END
