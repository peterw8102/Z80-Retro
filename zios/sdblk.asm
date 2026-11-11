; Wrapper around SD access to allow efficient 128 byte read/writes within 512 byte sectors. This
; is a specific optimisation to help with CP/M implementations. This blocking and de-blocking
; was originally done directly in the BIOS however that reduces the size of the TPA. By moving it
; here there that overhead is removed from application code space AND the additional meory available
; to the supervisor allows caching more data. Specifically two cached sectors allows file
; copies to be much more efficient.
import defs.asm
import config.asm
import pcb_def.asm

IS_DEBUG EQU 0
IS_TRACE EQU 0

      extrn  SD_INIT,SD_RBLK,SD_WBLK,SD_PRES,SD_SEL,SDMPADD,ADD8T16

      public SD_BKRD,SD_BKWR,SD_PRG,SD_STAT,SD_CINV,ENDBLK

      ; --------------- DEBUG SYMBOLS --------------
if IS_DEBUG || IS_TRACE
      extrn  PRINT,PRINT_LN,NL,RESINP
      extrn  WRITE_D,WRITE_8,WRITE_16,INHEX,INHEX_2,INHEX_4
      public CACHE,META,LRU,DIRTY
endif

      CSEG

; --------- HLDE contains the virtual drive and offset in 128 byte blocks:
;   +------------+------------+------------+------------+
;   |  virtdisk  |  Not Used  | N |<---15 bit offset--->|
;   +------------+------------+------------+------------+
; Unlike the native calls the offset in this case is in 128 byte CP/M
; like sectors and needs to be divided down to get to a 512 byte block
; INPUTS: HLDE   - Sector address
;
; The output buffer must have been set with a call to A_DSKDM
SD_BKRD:  ; Translate the logical address into an SDCard address we can use.
if IS_TRACE
          PUSH   HL
          PUSH   AF
          LD     HL,_m19
          CALL   PRINT_LN
          POP    AF
          POP    HL
          CALL   _showlog
endif
          LD     B,0        ; It's a read operation so need the data.
          CALL   prep

if IS_DEBUG
          ; Display resolved address
          ;---------------- DEBUG ---------------
          PUSH   HL
          PUSH   AF
          LD     HL,_m9
          CALL   PRINT
          LD     A,(SDCARD)
          CALL   WRITE_8
          LD     A,':'
          RST    08h
          LD     HL,(SECTOR+2)
          CALL   WRITE_16
          LD     A,':'
          RST    08h
          LD     HL,(SECTOR)
          CALL   WRITE_16
          CALL   NL
          POP    AF
          POP    HL
          ;---------------- DEBUG ---------------
endif


          ; Get the right block into a buffer area. BC points to the loaded
          ; block on return.
          CALL   primebuf
          RET    NZ         ; Read error: A = 1 (it used to be ignored and data
                            ; from another sector was returned)

          ; Write the data to the application address space.
          CALL   cpout
          XOR    A          ; Z and A = 0: success
          RET

if IS_DEBUG
_m9       DEFB   'SDCard Sector Address: ',0
endif

if IS_TRACE
_m19      DEFB   '------------- READ ------------',0
endif

; --------- HLDE contains the virtual drive and offset in 128 byte blocks:
;   +------------+------------+------------+------------+
;   |  virtdisk  |  Not Used  | N |<---15 bit offset--->|
;   +------------+------------+------------+------------+
; Unlike the native calls the offset in this case is in 128 byte CP/M
; like sectors and needs to be divided down to get to a 512 byte block
; INPUTS: HLDE   - Sector address
;         B      - 00: deferable write,
;                  01: non-deferable (directory) write - purge data
;                  02: first write to unallocated block (no read required)
;
; The values in B is the CP/M BIOS write mode parameter (passed in 'C'
; in the BIOS call). I don't know how to deal with 02 at the moment. What
; I think CAN happed, but not yet, is that a value of 00 doesn't require
; immediate disk writes so can lazy write when cache page is required
; but also purge all pages when B=01
;
; The output buffer must have been set with a call to A_DSKDM
SD_BKWR:  ; Translate the logical address into an SDCard address we can use.
if IS_TRACE
          PUSH   HL
          PUSH   AF
          LD     HL,_m20
          CALL   PRINT
          LD     A,B
          CALL   WRITE_8
          CALL   NL
          POP    AF
          POP    HL
          ; CALL   _showlog
endif
          CALL   prep

if IS_DEBUG
          ;---------------- DEBUG ---------------
          PUSH   HL
          PUSH   AF
          LD     HL,_m9
          CALL   PRINT
          LD     A,(SDCARD)
          CALL   WRITE_8
          LD     A,':'
          RST    08h
          LD     HL,(SECTOR+2)
          CALL   WRITE_16
          LD     A,':'
          RST    08h
          LD     HL,(SECTOR)
          CALL   WRITE_16
          CALL   NL
          POP    AF
          POP    HL
          ;---------------- DEBUG ---------------
endif

          ; Get the right block into a buffer area. BC points to the loaded
          ; block on return.
          CALL   primebuf
          RET    NZ            ; Error reading or freeing a slot: A = 1

          ; BC points to the data currently held on the SDCard. Overlay our 128 bytes
          CALL   cpin

          ; Mark this cache page dirty
          LD     A,(SLOT)
          CALL   MARK

if IS_DEBUG
          ;---------------- DEBUG ---------------
          PUSH   AF
          PUSH   BC
          PUSH   HL
          LD     HL,_m14
          CALL   PRINT
          LD     HL,DIRTY
          LD     B,NUMSECT
_nxtdrt2: LD     A,(HL)
          CALL   WRITE_8
          LD     A,':'
          RST    08h
          INC    HL
          DJNZ   _nxtdrt2
          CALL   NL
          POP    HL
          POP    BC
          POP    AF
          ;---------------- DEBUG ---------------
endif

          ; Purge data to disk. NOTE: This may seem inefficient at the moment
          ; but I'm aiming for a write cache model to avoid CP/M writing the
          ; same sector to the SDCard 4 times to write each of the 4 contained
          ; 128 byte CP/M sectors.
          LD     A,(WRT_MDE)
          DEC    A             ; If it was 1 then purge.
          JR     NZ,_defer
          XOR    A
          JR     SD_PRG        ; Returns the write result (A=0 ok, 1 error)

_defer:   XOR    A             ; Z and A = 0: success
          RET

if IS_DEBUG
_m14:     DEFB   'DIRTY PAGES: ',0
_m16:     DEFB   'Purge complete...',0
endif
if IS_TRACE
_m20      DEFB   '------------- WRITE------------',0
endif

; -------- MARK --------
; Mark the cache page referenced in A as DIRTY by setting the matchng
; byte in the DIRTY array.
MARK:     PUSH   HL
          LD     HL,DIRTY
          CALL   ADD8T16
          LD     (HL),1         ; Set dirty flag
          POP    HL
          RET


; -------- SD_PRG --------
; Walk through all cache pages and write any that are dirty out
; to the appropriate SDCard. If 'A' contains a non-zero value then
; also discard all cached data from memory and force reload from
; disk.
; INPUTS: A - if non-zero then ALSO flush all data from the cache
;             and force a reload from the device.
; OUTPUT: A = 0 and Z if everything was written; A = 1 and NZ if a write
;         failed (those pages stay marked dirty).
SD_PRG:   PUSH    AF
          LD      HL,IDX
          LD      DE,DIRTY
          LD      BC,NUMSECT << 8   ; B = count, C = 0: no errors
_nxtdrt:  LD      A,(DE)
          OR      A
          CALL    NZ,DOPURG   ; HL points to IDX entry,
          JR      Z,_prgok    ; Z: not dirty, or written successfully
          LD      C,1         ; The write failed: remember it
_prgok:   INC     HL          ; Next IDX
          INC     HL
          INC     HL
          INC     HL
          INC     DE          ; Next DIRTY flag
          ; And go aroubd again
          DJNZ    _nxtdrt
          POP     AF
          OR      A
          JR      Z,_prgret   ; Don't invalidate the cached data

if IS_DEBUG
          LD      HL,_m13
          CALL    PRINT_LN
endif

          ; Invalidate all stored data and force reload from device.
          PUSH    BC          ; C = write result (the LDIRs clobber it)
          LD      HL,META
          LD      DE,META+1
          LD      A,2
          LD      (HL),A
          LD      BC,METASZ * NUMSECT - 1   ; Was N: wrote one byte too many (into LRU)
          LDIR

          ; Pages that could not be written are dropped: they no longer
          ; belong to any sector and must not be written later.
          LD      HL,DIRTY
          LD      B,NUMSECT
_clrdrt:  LD      (HL),0
          INC     HL
          DJNZ    _clrdrt

          ; Clear entire cache area to zero so there's no data laying around
          LD      HL,CACHE
          LD      DE,CACHE+1
          XOR     A
          LD      (HL),A
          LD      BC,SECTSZ * NUMSECT - 1   ; Was N: wrote one byte too many
          LDIR
          POP     BC
_prgret:  LD      A,C
          OR      A
          RET

if IS_DEBUG
_m12      DEFB    'Purging dirty pages...',0
_m13      DEFB    'Flushing stored cache data',0
endif

; -------- SD_STAT --------
; Copy out the disk stats to the buffer proviced (if not null). If B is zero
; then also clear the stats after they are copied.
; INPUTS:  HL  - Points to buffer to receive stats (or zero for no copy)
;          B   - if not zero then clear the stats after the copy
; HL,A not preserved
SD_STAT:  LD      A,L
          OR      H
          JR      Z,.nocopy

          ; Copy stats to the buffer pointed to by HL
          LD      DE,ST_READ
          EX      DE,HL
          PUSH    BC
          LD      BC,ENDSTAT-ST_READ
          LDIR
          POP     BC

.nocopy:  LD      A,B      ; Clear stats?
          OR      A
          RET     Z        ; No clear required

          XOR     A
          LD      HL,ST_READ
          LD      B,ENDSTAT-ST_READ
.clr      LD      (HL),A
          INC     HL
          DJNZ    .clr
          RET



; -------- SD_CINV --------
; Invalidate the cache entry holding this sector (if any). Used after a
; direct write (services 15/16) so the cache does not keep returning the old
; version of the sector.
; INPUTS:  A    - Physical SDCard
;          HLDE - Absolute sector
; Preserves BC, DE and HL.
SD_CINV:  PUSH    BC
          PUSH    DE
          PUSH    HL
          LD      (SDCARD),A
          LD      (SECTOR),DE
          LD      (SECTOR+2),HL
          LD      HL,META
          LD      C,NUMSECT
_cinxt:   PUSH    HL
          LD      DE,SDCARD
          LD      B,TESTSZ
_cicmp:   LD      A,(DE)
          CP      (HL)
          JR      NZ,_cino
          INC     DE
          INC     HL
          DJNZ    _cicmp
          POP     HL
          LD      (HL),2      ; Card 2: never matches
          PUSH    HL
          LD      A,NUMSECT   ; If still dirty (an earlier write failed) drop it:
          SUB     C           ; the direct write is the good version and the
          LD      HL,DIRTY    ; old one must not be written over it
          CALL    ADD8T16
          LD      (HL),0
_cino:    POP     HL
          LD      DE,METASZ
          ADD     HL,DE
          DEC     C
          JR      NZ,_cinxt
          POP     HL
          POP     DE
          POP     BC
          RET

; -------- DOPURG --------
; Purge cache page 'C' to SDCard.
; INPUTS:  HL  - Points to IDX entry
;          DE  - Points to the dirty marker which needs to be cleared
; A not preserved
DOPURG:   PUSH    BC
          PUSH    DE
          PUSH    HL

if IS_DEBUG
          PUSH    HL
          LD      HL,_m15
          CALL    PRINT
          POP     HL
          PUSH    HL
          CALL    WRITE_16
          CALL    NL
          POP     HL
endif

          LD      E,(HL)         ; Load META data pointer from IDX
          INC     HL
          LD      D,(HL)
          INC     HL
          LD      C,(HL)         ; Load CACHE data pointer from IDX+1
          INC     HL
          LD      B,(HL)
          EX      DE,HL          ; HL: Meta data pointer, BC: Cache data pointer
          CALL    PURGE
          POP     HL
          POP     DE
          JR      NZ,_dpfail     ; If the write failed the page stays dirty
          XOR     A              ; (it used to be marked clean BEFORE writing and
          LD      (DE),A         ; the error ignored: the data was lost)
_dpfail:  POP     BC
          RET

if IS_DEBUG
_m15      DEFB    'Purge to disk. PAGE: ',0
endif

; -------- PURGE --------
; Write the data pointed to by BC (CACHE[]) to the sector address
; pointed to by HL (META[]). Purge dirty data to the SDCard for a single
; cache entry.
; INPUTS: BC    - Address of data
;         HL    - Points to the meta data block (SDCard number then 4 byte sector address)
; Registers NOT preserved
PURGE:   LD     A,(HL)    ; SDCard number
         CALL   SD_SEL    ; And select it...

         INC    HL
         LD     E,(HL)    ; Low 16 bits of SDCard sector address
         INC    HL
         LD     D,(HL)
         PUSH   DE
         INC    HL
         LD     E,(HL)    ; High 16 bits of SDCard sector address
         INC    HL
         LD     D,(HL)
         EX     DE,HL
         POP    DE        ; Address data loaded (HLDE). BC already contains buffer address
         CALL   SD_WBLK
         PUSH   AF        ; Z if the card accepted the block

         ; Update stats
         LD     HL,ST_WRITE
         CALL   INC64
         POP    AF
         RET

; ----- PURGONE  ------
; Purge data in page 'A'. Data is written to the SDCard and the DIRTY flag cleared
; INPUTS: HL   - Points to the dirty flag
;         A    - Cache page number
PURGONE:  PUSH   HL
if IS_DEBUG
          PUSH   AF
          LD     HL,_m1
          CALL   PRINT
          POP    AF
          PUSH   AF
          CALL   WRITE_8
          CALL   NL
          POP    AF
endif
          LD     HL,IDX
          OR     A
          JR     Z,_fndisx  ; Zero slot so save the arithmatic
          RLCA              ; Multiple slot number by 4 (size of IDX record)
          RLCA
          CALL   ADD8T16    ; Get the IDX slot. HL points to the pointers to the data we want
_fndisx:  LD     E,(HL)     ; Address of the metadata
          INC    HL
          LD     D,(HL)
          INC    HL
          LD     C,(HL)     ; Address of the data cache
          INC    HL
          LD     B,(HL)
          INC    HL
          EX     DE,HL      ; BC=cache data area, HL=Mata data slot
          CALL   PURGE
          POP    HL
          RET    NZ         ; Failed: the page stays dirty
          LD     (HL),0     ; Written: no longer dirty (it was never cleared and
          RET               ; got written again later with stale data)

if IS_DEBUG
_m1:      DEFB   'PURGE ONE PAGE: ',0
endif

; ---- cpout
; Copy the data from the loaded, cached block into the application
; space 128 byte buffer. The buffer must have been set via a call to
; the A_DSKDM services API.
cpout:  LD     BC,(SLOTADD)

if IS_DEBUG
        ; ------------- DEBUG --------------
        LD     HL,_m3
        CALL   PRINT
        LD     A,(DMA_PAGE)
        CALL   WRITE_8
        LD     A,':'
        RST    08h
        LD     HL,(DMA_ADDR)
        CALL   WRITE_16
        LD     A,' '
        RST    08h
        LD     H,B
        LD     L,C
        CALL   WRITE_16
        ; ------------- DEBUG --------------
endif

        BANK   1,(DMA_PAGE)  ; Get the buffer page into bank 1
        BANK   2,(DMA_PG2)   ; and the next one into bank 2 in case the buffer
                             ; crosses a 16K boundary
        LD     A,(SCT_OFF)   ; Add blk offset to base in BC
        CALL   calcoff       ; HL points to the start of the 128 bytes wanted

if IS_DEBUG
        ; ------------- DEBUG --------------
        PUSH   AF
        PUSH   HL
        LD     A,'-'
        RST    08h
        LD     A,'>'
        RST    08h
        CALL   WRITE_16
        CALL   NL
        POP    HL
        POP    AF
        ; ------------- DEBUG --------------
endif

        LD     DE,(DMA_ADDR) ; Where to put the data
        LD     BC,APPSZ      ; Number of bytes to copy
        LDIR                 ; Copy the data
        RET

if IS_DEBUG
_m3     DEFB 'Copy Out: ',0
endif

; ---- cpin
; Copy the data into the loaded, cached block from the application
; space 128 byte buffer. The buffer must have been set via a call to
; the A_DSKDM services API.
cpin:   BANK   1,(DMA_PAGE)  ; Get the buffer page into bank 1
        BANK   2,(DMA_PG2)   ; Next page (buffer crossing 16K)
        LD     A,(SCT_OFF)   ; Add blk offset to base in BC
        LD     BC,(SLOTADD)

if IS_DEBUG
        PUSH   AF
        PUSH   HL
        LD     HL,_m18
        CALL   PRINT
        LD     H,B
        LD     L,C
        CALL   WRITE_16
        CALL   NL
        POP    HL
        POP    AF
endif

        CALL   calcoff       ; HL points to the start of the 128 bytes wanted

        LD     DE,(DMA_ADDR) ; Where to get the data
        EX     DE,HL         ; Change direction
        LD     BC,APPSZ      ; Number of bytes to copy
        LDIR                 ; Copy the data
        RET

if IS_DEBUG
_m18    DEFB   'Start of SDCard buffer: ',0
endif

; ----- prep -----
; Split out the data in HLDE into the information needed by this call and
; store in working data area (SCT_OFF, SDCARD, SECTOR+4). These values are also
; returned in registers as:
;    HLDE:    SDCard 512byte sector address
;    A:       The physical SDCard number
prep:     LD     A,B
          LD     (WRT_MDE),A  ; Save the write mode
          LD     A,11b
          AND    E         ; This tells us the 128 byte offset into a 512K sector we need
          LD     (SCT_OFF),A

          ; Divide the 15 bit blk address by 4 to get the sector offset address.
          LD     A,E
          SRL    D
          RRA
          SRL    D
          RRA
          LD     E,A
          LD     A,3Fh
          AND    D             ; Make sure it's not out of scope
          LD     D,A

          ; Convert to an ABSOLUTE address we can use to access
          ; the SDCard.
          CALL   SDMPADD
          LD     (SDCARD),A     ; Which SDCard
          LD     (SECTOR),DE    ; The sector within that SDCard
          LD     (SECTOR+2),HL  ; The sector within that SDCard

          RET


; -- primebuf
; Try and locate the requested SDCard page in the cache. If it 's
; there reuse otherwise load the block into the least recently
; used block.
; INPUTS:   HLDE - Absolute sector number required
;           A    - The SDCard number (0 or 1)
; OUTPUTS:  BC   - Address of the loaded block (also SLOTADD)
; Returns A = 0 and Z when the block is ready; A = 1 and NZ on error.
primebuf:  CALL    getslot         ; BC receiving buffer address
           JR      C,_pberr        ; Could not free a cache slot
           LD      (SLOTADD),BC    ; The cached block address

if IS_DEBUG
           ; ----------------- DEBUG ----------------
           PUSH   HL
           PUSH   AF
           LD     HL,_m6
           CALL   PRINT
           LD     A,(SLOT)
           CALL   WRITE_8
           LD     A,':'
           RST    08h
           LD     HL,(SLOTADD)
           CALL   WRITE_16
           CALL   NL
           POP    AF
           POP    HL
           ; ----------------- DEBUG ----------------
endif

           JR     Z,_pbmiss
           XOR    A                ; Already loaded
           RET

_pbmiss:
if IS_DEBUG
           ; ----------------- DEBUG ----------------
           PUSH   AF
           PUSH   HL
           LD     HL,_m7
           CALL   PRINT_LN
           POP    HL
           POP    AF

           LD      HL,_m2
           CALL    PRINT
           LD      A,(SDCARD)
           CALL    WRITE_8
           LD      A,':'
           RST     08h
           LD      HL,(SECTOR+2)
           CALL    WRITE_16
           LD      A,':'
           RST     08h
           LD      HL,(SECTOR)
           CALL    WRITE_16
           LD      A,'-'
           RST     08h
           LD      A,'>'
           RST     08h
           LD      H,B
           LD      L,C
           CALL    WRITE_16
           CALL    NL
           ; ----------------- DEBUG ----------------
endif
           ; If the access mode is '2' then no need to do a
           ; read before write.
           LD      A,(WRT_MDE)
           CP      2             ; Write to new block
           JR      NZ,_loadchc

           ; First write to an unallocated sector so fill
           ; with zeroes and return.
           LD     HL,ST_CLN
           CALL   INC64

           LD     H,B
           LD     L,C
           LD     D,H
           LD     E,L
           LD     (HL),0
           INC    DE
           PUSH   BC          ; Save address of buffer
           LD     BC,511      ; Size of buffer
           LDIR               ; Clear
           POP    BC
           XOR    A
           RET

           ; Select the correct SDCard for the read operation
_loadchc:  LD      A,(SDCARD)
           CALL    SD_SEL

           ; Update read stats
           PUSH    BC
           LD      HL,ST_READ
           CALL    INC64
           POP     BC

           ; Buffer address is in BC. Need the SDCard address
           LD      DE,(SECTOR)
           LD      HL,(SECTOR+2)

           CALL    SD_INIT
           CALL    SD_RBLK         ; Get the SDCard data
           LD      HL,(SLOTADD)    ; The cached block address
           RET     Z               ; A = 0: success

           ; The read failed. The entry used to stay tagged with the new
           ; sector while holding another sector's data, and later accesses
           ; took it as valid. Invalidate it (card 2 = never matches).
           LD      A,(SLOT)
           RLCA                    ; x4: size of an IDX entry
           RLCA
           LD      HL,IDX
           CALL    ADD8T16
           LD      E,(HL)          ; Pointer to this entry's META
           INC     HL
           LD      D,(HL)
           LD      A,2
           LD      (DE),A
_pberr:    LD      A,1
           OR      A               ; NZ: error
           RET

if IS_DEBUG
_m2        DEFB    "Load Address: ",0
_m6        DEFB    "Cache slot: ",0
_m7        DEFB    "Load from SDCard",0
endif

; ------- getslot -------
; Find a slot in the cache to be used for new data.
; EITHER:
;   The requested address is already in the cache so return that page
; OR
;   Use the least recently accessed cache page for the data
;
; INPUT:  A     - Physical SDCard target
;         SDCARD/CECTOR
;               - Retrieved from memory
; OUTPUT: BC    - Address of the 512 byte buffer to be used
getslot:  CALL  _chkcach

if IS_DEBUG
          ; --------------- DEBUG ------------
          PUSH  AF
          PUSH  HL
          JR    Z,_xx1
          LD    HL,_m4
          CALL  PRINT_LN
_xx1:     POP   HL
          POP   AF
          ; --------------- DEBUG ------------
endif
          RET   NZ         ; Found. BC already set

if IS_DEBUG
          ; --------------- DEBUG ------------
          PUSH  HL
          LD    HL,_m5
          CALL  PRINT_LN
          POP   HL
          ; --------------- DEBUG ------------
endif

          ; Reuse an existing slot. Choose by the contents of the
          ; LRU array. The entry with the highest value is the least
          ; recently used.
          CALL  cycslt
          RET   C          ; Error writing the page that was to be reused

          ; At this point 'A' is the best slot to re-use
          ; Copy the current target into this slots meta data area
          CALL  cpymeta

          LD    A,(SLOT)
          CALL  up_lru

          LD    A,(SLOT)
          CALL  getent
          XOR   A          ; Z: needs loading. Carry = 0
          RET

if IS_DEBUG
_m4       DB    "Found in cache",0
_m5       DB    "Not found in cache",0
endif

; ----- cycslt -----
; Find the least recently used cache entry and reuse it. If the page is DIRTY
; then purge data from memory to the SDCard before reuse.
; INPUTS:   none
; OUTPUTS:  C   - Slot number to use
; A is NOT preserved
;
cycslt:   PUSH  HL
          PUSH  DE
          PUSH  BC
          LD    HL,LRU
          XOR   A
          LD    D,A        ; Max value so far seen
          LD    E,A        ; Slot we're looking at
          LD    C,A        ; Best slot so far
          LD    B,NUMSECT  ; Number of slots to test
_nxtlru:  LD    A,(HL)
          CP    D          ; Is this value >= max so far?
          JR    C,_wrs     ; Not as good as the one we already have
          LD    C,E        ; Choosing this one
          LD    D,A        ; And store the max value
_wrs:     INC   HL         ; Next LRU entry
          INC   E          ; To this slot number
          DJNZ  _nxtlru    ; And compare with the next entry
          LD    A,C        ; Return the slot to use in A
          LD   (SLOT),A    ; Store for later

          ; Check to see whether this slot is DIRTY and purge if necessary.
          LD    HL,DIRTY
          CALL  ADD8T16
          LD    A,(HL)     ; Get the dirty flag
          OR    A
          LD    A,C        ; Get the slot back into A
          CALL  NZ,PURGONE
          JR    Z,_cyok

          ; The dirty page could not be written: do not reuse it (its data
          ; would be lost). Use the least recently used CLEAN page instead;
          ; only if every page is dirty is an error returned. This way a
          ; persistent write error does not leave CP/M without a cache
          ; (previously it could not even reload the CCP).
          XOR   A
          LD    (CY_MAX),A
          LD    C,0FFh        ; None yet
          LD    E,0           ; Page being checked
          LD    HL,LRU
          LD    B,NUMSECT
_cyfnx:   PUSH  HL
          LD    HL,DIRTY
          LD    A,E
          CALL  ADD8T16
          LD    A,(HL)
          POP   HL
          OR    A
          JR    NZ,_cyfsk     ; Dirty: not usable
          LD    A,(CY_MAX)
          LD    D,A
          LD    A,(HL)
          CP    D
          JR    C,_cyfsk
          LD    (CY_MAX),A
          LD    C,E
_cyfsk:   INC   HL
          INC   E
          DJNZ  _cyfnx
          LD    A,C
          INC   A             ; FF: no clean page
          JR    Z,_cyerr
          LD    A,C
          LD    (SLOT),A

_cyok:    POP   BC
          POP   DE
          POP   HL
          LD    A,(SLOT)
          OR    A           ; Carry = 0
          RET

_cyerr:   POP   BC
          POP   DE
          POP   HL
          SCF               ; Carry: no page can be reused
          RET

; ----- cpymeta -----
; Copy the current target into one of the cache meta data slots. This is
; used when a new slot has been chosen for the data.
; INPUT:   A - Slot number
; All registers preserved.
cpymeta:  PUSH HL
          PUSH DE
          PUSH BC
          LD   HL,META
          OR   A          ; Slot zero so we have the target address
          JR   Z,_docpy

          LD   B,A        ; Counter
          LD   DE,TESTSZ
_nxtmet:  ADD  HL,DE
          DJNZ _nxtmet

          ; HL points to the start of the meta data area
_docpy:   LD   DE,SDCARD
          EX   DE,HL
          LD   BC,TESTSZ
          LDIR
          POP  BC
          POP  DE
          POP  HL
          RET


; ----------- up_lru
; Update the LRU list. Increment all cache LRU values EXCEPT the one
; referenced in A (which is cleared to zero).
; INPUT: A - Slot number to clear
; Register not maintained: A
up_lru:   PUSH  HL
          PUSH  BC
          LD    HL,LRU

          ; Clear the slot to be used (set to FF)
          CALL  ADD8T16
          PUSH  HL          ; Keep the address of the currently used slot

          ; Increment all LRU entries
          LD    HL,LRU
          LD    B,NUMSECT
nxtlru:   INC   (HL)
          JR    NZ,_lruok   ; Do not wrap from FF to 0: an unused page would
          DEC   (HL)        ; suddenly look like the most recent
_lruok:   INC   HL
          DJNZ  nxtlru
          POP   HL
          XOR   A
          LD    (HL),A

if IS_DEBUG
          LD    HL,_m8
          CALL  PRINT
          LD    HL,LRU
          LD    B,NUMSECT
_dnxt:    LD    A,(HL)
          INC   HL
          CALL  WRITE_8
          LD    A,'-'
          RST   08h
          DJNZ  _dnxt
          CALL  NL
endif

          POP   BC
          POP   HL
          RET

if IS_DEBUG
_m8:      DEFB  'LRU Values: ',0
endif


; ----- _chkcach -----
; Look for the SDCARD/SECTOR address in the cached blocks.
; INPUTS: None
; OUTPUT: BC       - address of buffer to use
;         A & Z    - Z (set) if no entry in the cahce, NZ if found.
_chkcach:  LD   B,NUMSECT   ; Number of cached sectors
           LD   C,0         ; Which cache entry is being used?
           LD   DE,META
_nxtent:   PUSH BC          ; Reuse BC
           PUSH DE          ; Start of this meta block

           LD   B,TESTSZ    ; Number of bytes to check
           LD   HL,SDCARD   ; Test pattern to repeat
_nxtb:     LD   A,(DE)
           CP   (HL)
           INC  DE
           INC  HL
           JR   NZ,_miss
           DJNZ _nxtb

           ; If we get here then the target block is in this
           ; cache entry so won't need to load. Don't need to
           ; change the cache values but need to update the
           ; correct LRU and stats.

           ; Update cache hit stats
           LD   HL,ST_HITS
           CALL INC64

           POP  DE       ; Discard the start of meta data
           POP  BC       ; Get the block number stored in C
           LD   A,C
           LD   (SLOT),A ; Store for later

if IS_DEBUG
           PUSH  HL
           PUSH  AF
           LD    HL,_m10
           CALL  PRINT
           LD    A,C
           CALL  WRITE_8
           CALL  NL
           POP   AF
           POP   HL
endif
           CALL up_lru

           ; Get the address of the slot we're using. Need to return
           ; this in BC.
           LD   A,C
           CALL getent

           ; Clear the Z flag for the return result (cache miss)
           LD   A,1
           OR   A
           RET

           ; Wasn't this cache entry so try the next
_miss:     POP  DE          ; Start of last meta data
           POP  BC          ; Any more meta data blocks available?

           REPT METASZ
           INC  DE          ; Step to start of next meta data block
           ENDM

           INC   C          ; Next slot number

           DJNZ  _nxtent

if IS_DEBUG
           CALL  NL
endif
           ; Get to here then there's no match in the cache
           XOR   A          ; Return zero for cache miss
           RET

if IS_DEBUG
_m10:      DEFB  "Cache match in page: ",0
endif

; ----- getent -----
; Get the address of the cache slot we're using. Need to return
; this in BC. Slot number is in A
; INPUT:   A - Slot number
; OUTPUT  BC - Address of the start of the slot
; A NOT preserved.
getent:    PUSH HL
           LD   HL,CACHE
           OR   A        ; If slot zero then nothing to do
           JR   Z,_retchc
           LD   B,A
nxtchch:   REPT SECINC   ; Add 512 to HL
           INC  H
           ENDM
           DJNZ nxtchch
_retchc:   LD   C,L
           LD   B,H
           POP  HL
           RET



; -- calcoff
; Return HL as the offset into the blkbuf of the currently selected sector.
; INPUT  A  - offset
;        BC - address of source data
; OUTPUT HL  - Calculated buffer offset.
; A, HL not preserved
calcoff:    LD    HL,0
            AND   03h
            RRCA
            RR    L
            RRCA
            RL    H
            LD    A,C
            ADD   L
            LD    L,A
            LD    A,B
            ADC   A,H
            LD    H,A         ; HL: Start of sector data
            RET

; -- INC64
; Increment the 64 bit number pointed to by HL
; INPUT  HL -  pointer to the number
; A, HL, B not preserved
INC64:      LD    B,4
.nextb      LD    A,(HL)
            INC   A
            LD    (HL),A
            OR    A
            RET   NZ
            ; If it's wrapped to zero then next byte
            INC   HL
            DJNZ  .nextb
            RET

if IS_DEBUG || IS_TRACE

_showlog:   PUSH  AF
            PUSH  BC
            PUSH  DE
            PUSH  HL

            LD    (_scrt+1),DE
            LD    (_scrt+3),HL

            LD    HL,_m17
            CALL  PRINT
            LD    HL,(_scrt+3)
            CALL  WRITE_16
            LD    HL,(_scrt+1)
            CALL  WRITE_16
            CALL  NL

            POP   HL
            POP   DE
            POP   BC
            POP   AF
            RET

_m17   DEFB  'Logical address: ',0

_scrt  DEFS    16

endif





; SDCard addressing is a 32 bit number with the following format:
;   +------------+------------+------------+------------+
;   |  virtdisk  |  Not Used  | N/U |<--13 bit offset-->|
;   +------------+------------+------------+------------+

; Meta data for each cached sector. There can be any number of these.
; For now there are 2 (=1KB)
NUMSECT  EQU   8    ; Number of cache blocks.
METASZ   EQU   5    ; 5 per meta cache entry
TESTSZ   EQU   5    ; 5 of which are the address identifier
SECTSZ   EQU   512  ; Size of the SDCard sector (should never change!)
SECINC   EQU   SECTSZ >> 8     ; Sector size / 256
APPSZ    EQU   128  ; Number of bytes in an application block

; Working space for new requests being processed
WRT_MDE  DB    0    ; 0: deferable, 1=non-deferable, purge data
SLOT     DB    0    ; Which cache slot is being used for this request
CY_MAX   DB    0    ; cycslt work: highest LRU seen among clean pages
SLOTADD  DW    0    ; The address of the cached block currently being processed
SCT_OFF  DB    0    ; Offset into the 512 byte sector from which to return 128 bytes

; SDCARD and SECTOR will contain the values for the currently executing
; operation. They are not preserved and are only working values. NOTE
; these two values MUST stay together in this order to match the layput
; of the cache metadata entries.
SDCARD   DB    0    ; Which SDCard to read from
SECTOR   DW    0,0  ; Sector address (512 byte) within that SDCard

CHC_WK   EQU   SECTOR   ; Reuse SECTOR to hold a pointer into cache during purge ops
META_WK  EQU   SECTOR+2 ; And a temporary pointer into the meta data area


; IDX: Set of pointers to each set of cache data information. Each entry in
; this array comprises the following data:
;   Dirty flag
;   Pointer to META data
;   Pointer to CACHE data
; 5 bytes in total
t_cache    DEFL  CACHE
t_meta     DEFL  META
IDX:       REPT NUMSECT
           DW   t_meta        ; Pointer to the associated meta data entry
           DW   t_cache       ; Pointer to this cache entry

t_meta     +=   METASZ
t_cache    +=   SECTSZ

           ENDM



; The cache metadata. Format is:
;   +------------+------------+------------+------------+------------+
;   |  SDDisk    | <--------- 32 bit SDCard Sector Address --------> |
;   +------------+------------+------------+------------+------------+

META     DC    NUMSECT*TESTSZ,2 ; A disk value of 2 means this is never matched on a cache lookup

; ----------------------------------------------------------
; LRU list. This is implemented as follows:
; + There is one byte cache block
; + Initialised to zero
; + On each cache page allocation, all bytes are incremented AND
; + The byte representing the used location is cleared to zero
; In this way the cache entry with the highest LRU number is the least
; recently used and will be next used. The LRU byte values should never
; exceed the number of cache pages.
LRU      DC    NUMSECT,0

; DIRTY: One byte for each cache page. Zero means no writes, non-zero means
; writes have been made to the cache entry but nothing has been written
; to the SDCard. When a page has been written the matching dirty bits
; should be cleared.
DIRTY    DC    NUMSECT,0

; The cache data itself
CACHE    DEFS  NUMSECT*SECTSZ ; buffer to receive a single 512B block

ENDBLK   EQU   $

; Keep some stats
ST_READ  DEFW  0,0
ST_WRITE DEFW  0,0
ST_HITS  DEFW  0,0
ST_CLN   DEFW  0,0
ENDSTAT  EQU   $

  END
