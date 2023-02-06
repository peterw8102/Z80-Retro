; Z80 BIOS for CP/M 2.2
; heavily based on CP/M 2.2 information from:
;   http://cpuville.com/Code/CPM-on-a-new-computer.html

; Grab the API command codes
import ../../zloader/api.asm

           extrn  CCP,BDOS
           public BOOT,_endall

cdisk:     EQU  0004h      ;address of current disk number 0=a,... l5=p
iobyte:    EQU  0003h      ;intel i/o byte
disks:     EQU  04h        ;number of disks in the system
CR:        EQU  0DH
LF:        EQU  0AH
STK:       EQU  80H        ; Small stack between RST and CP/M buffers
; XCCP:       EQU  @@4

          ASEG
          ORG 0
          DI
          JP  BOOT


          CSEG
          ; ORG bios

; Calculate how many sectors we need to load for BDOS+CCP (BIOS must already be in memory)
; nsects:	EQU	($-CCP)/128	;warm start sector count

          ; JUMP TABLE (main BIOS entry). This must be located immediately after the end of BDOS

BIOS:
JMPTAB:   JP BOOT      ;-3: Cold start routine
WBOOTE:   JP WBOOT     ; 0: Warm boot - reload command processor
          JP CONST     ; 3: Console status
          JP CONIN     ; 6: Console input
          JP CONOUT    ; 9: Console output
          JP LIST      ;12: Printer output
          JP PUNCH     ;15: Paper tape punch output
          JP READER    ;18: Paper tape reader input
          JP HOME      ;21: Move disc head to track 0
          JP SELDSK    ;24: Select disc drive
          JP SETTRK    ;27: Set track number
          JP SETSEC    ;30: Set sector number
          JP SETDMA    ;33: Set DMA address
          JP READ      ;36: Read a sector
          JP WRITE     ;39: Write a sector

          ; CPM/2 extensions
          JP LISTST    ;42: Status of list device
          JP SECTRN    ;45: Sector translation for skewing

          ; CPM/3 extensions
          ; JP CONOST    ;48: Status of console output
          ; JP AUXIST    ;51: Status of auxiliary input
          ; JP AUXOST    ;54: Status of auxiliary output
          ; JP DEVTBL    ;57: Address of devices table
          ; JP DEVINI    ;60: Initialise a device
          ; JP DRVTBL    ;63: Address of discs table
          ; JP MULTIO    ;66: Read/write multiple sectors
          ; JP FLUSH     ;69: Flush host buffers
          ; JP MOVE      ;72: Move a block of memory
          ; JP TIME      ;75: Real time clock
          ; JP SELMEM    ;78: Select memory bank
          ; JP SETBNK    ;81: Select bank for DMA operation
          ; JP XMOVE     ;84: Preload banks for MOVE
          ; JP USERF     ;87: System-depedent functions
          ; JP RESERV1   ;90: Reserved
          ; JP RESERV2   ;93: Reserved

          CSEG
;
;  fixed data tables for four-drive standard
;  ibm-compatible 8" disks
;  no translations
;
;  disk Parameter header for disk 00
dpbase:  defw  0000h, 0000h
  defw  0000h, 0000h
  defw  dirbf, dpblk0
  defw  0000h, all00
;  disk parameter header for disk 01
  defw  0000h, 0000h
  defw  0000h, 0000h
  defw  dirbf, dpblk0
  defw  0000h, all01
;  disk parameter header for disk 02
  defw  0000h, 0000h
  defw  0000h, 0000h
  defw  dirbf, dpblk0
  defw  0000h, all02
;  disk parameter header for disk 03
  defw  0000h, 0000h
  defw  0000h, 0000h
  defw  dirbf, dpblk0
  defw  0000h, all03

  dpblk0:	;disk parameter block for all disks.
    defw	512   ;sectors per track                  SPT - 512 sectors per track
    defm	4     ;block shift factor 2K blocks       BSH - 2K allocation blocks
    defm	15    ;block mask                         BLM - Mask for 2K blocks
    defm	0     ;null mask                          EXM - Extent Mask for 2K blocks
    defw	2015  ;disk size-1 (2047-32).             DSM - 2015 allocation blocks (x2K so 4M disks minus first track)
    defw	511		;directory max                      DRM -
    defm	255		;alloc 0                            AL0 - 8 blocks. With BLS 2048, 64/block. 512/64=8
    defm	0     ;alloc 1                            AL1
    defw	0     ;check size                         CKS - Treat this as a fixed disk - no checking
    defw	1     ;track offset                       OFF - 1 track offset, so 256x128B sectors = 64K. Enough to boot.

  ; dpblk:	;disk parameter block for all disks.
  ;   defw	512   ;sectors per track                  SPT - 512 sectors per track
  ;   defm	4     ;block shift factor 2K blocks       BSH - 2K allocation blocks
  ;   defm	15    ;block mask                         BLM - Mask for 2K blocks
  ;   defm	0     ;null mask                          EXM - Extent Mask for 2K blocks
  ;   defw	2047  ;disk size-1                        DSM - 2048 allocation blocks (x2K so 4M disks)
  ;   defw	511		;directory max                      DRM -
  ;   defm	255		;alloc 0                            AL0 - 8 blocks. With BLS 2048, 64/block. 512/64=8
  ;   defm	0     ;alloc 1                            AL1
  ;   defw	0     ;check size                         CKS - Treat this as a fixed disk - no checking
  ;   defw	0     ;track offset                       OFF - 1 track offset, so 256x128B sectors = 64K. Enough to boot.

; ********* BASIC MEMORY MAP *********
; 0000 - restart address
; 0080 - Buffer space (128 bytes)
; 0100 - Start of programme (TPA)
; D900 - Base of CCP
; E112 - Base of BDOS
; EF0C - Base of BIOS
; ****** THESE VALUES VARY AND MAY BE OUT OF DATE ********

; ***** IMPLEMENTATION *****
; ****** COLD BOOT *******/
START:
BOOT:     XOR    a
          LD     (iobyte),A   ; clear the iobyte
          LD     (cdisk),A    ; select disk zero

          LD     SP,STK       ; Set a temporary small stack in spare space below the CPM buffer

          ; Configure the loader with our DMA buffer
          LD     HL,blkbuf
          LD     C,A_DSKDM
          RST    30h          ; Set DMA address

          ; Write out HELLO message (whatever's in the CP/M buffer)
          LD     HL,CCP+6
          CALL   _strout

          JP     gocpm        ; initialize and go to cp/m

; USED: SELDSK, HOME, SETDMA, READ, SETTRK

; ****** WARM BOOT *******/
; Load CCP and BDOS from the start of disk 0
WBOOT:    LD    SP, STK     ; Set a temporary small stack in spare space below the CP/M buffer
          LD    C,0         ; select disk 0
          CALL  SELDSK
          CALL  HOME        ; go to track 00
          CALL  CPMLOAD

          ; LD    HL,_STARTN
          ; CALL  _strout

          ; JP    gocpm

; _STARTN   DEFB  'Starting...',10,13,0

gocpm:    LD    a, 0c3h     ; c3 is a jmp instruction
          LD  	(0),A	      ; for jmp to wboot
          LD	  HL, wboote  ; wboot entry point
          LD  	(1),HL		  ; set address field for jmp at 0

          LD  	(5),A		    ; for jmp to bdos
          LD  	HL, bdos	  ; bdos entry point. SHOULDN'T KNOW THIS!!!!
          LD  	(6),HL		  ; address field of Jump at 5 to bdos

          ; Install shortcut to CONOUT at RST 08h
          LD  	(8),A
          LD    HL,CONOUT
          LD    (9),HL

          LD  	BC, 80h		  ; default dma address is 80h
          CALL  setdma

          LD    HL,WBOOTE
          LD    (0Bh),HL    ; Store the base of BIOS - needed to do low-level commands. NON STANDARD

          EI                ; enable the interrupt system

          LD    A,(cdisk)   ; get current disk number
          CP    disks       ; see if valid disk number
          JR    C,diskok    ; disk valid, go to CCP
          XOR   A           ; invalid disk, change to disk 0
diskok:   LD    C,A         ; send to the CCP

          JP	  CCP         ; go to cp/m for further processing, clearing the input buffer

; ****** CONST: Console status *******/
; Use ZIOS library and check for available character.
; Char available: return ff if character ready, 0 otherwise
CONST:    LD    C,A_CHKCH
          RST   30H      ; Check for waiting character ->
          RET   Z        ; No character -> 0
          XOR   A
          DEC   A        ; Ready -> 0xff
          RET

; ****** CONIN: Read character from console *******/
; Char available: return in A. Wait until there is  character
CONIN:    LD    C,A_RXCHR
          RST   30H  ; Character returned in A
          OR    A    ; set flags
          RET

; ****** CONOUT: Console output *******/
; Write character in A to console
CONOUT:   LD     E,C
          LD     C,A_TXCHR
          RST   30H
          RET

; ****** _strout: String output a string. String terminated by a 0, pointed by HL *******/
; Write character in A to console
_strout:  LD    A,(HL)
          OR    A
          RET   Z
          LD    E,A
          INC   HL
          LD    C,A_TXCHR
          RST   30H
          JR    _strout

LIST:     RET
PUNCH:    RET
READER:   RET

; CPM/2 extensions
LISTST:   XOR  A
          RET
SECTRN:   LD   H,B
          LD   L,C
          RET

; CPM/3 extensions
; CONOST:   RET
; AUXIST:   RET
; AUXOST:   RET
; DEVTBL:   RET
; DEVINI:   RET
; DRVTBL:   RET
; MULTIO:   RET
; FLUSH:    RET
; MOVE:     RET
; TIME:     RET
; SELMEM:   RET
; SETBNK:   RET
; XMOVE:    RET
; USERF:    RET
; RESERV1:  RET
; RESERV2:  RET


; Utility routines
; SELDSK, HOME, SETDMA, READ, SETTRK

; ********** SELDSK *********
; select disk given by register c
seldsk:   LD    HL, 0000h  ;error return code
          LD    A, C
          CP    disks    ; must be between 0 and 3
          RET   NC       ; no carry if 4, 5,...
          ; disk number is in the proper range
          ; compute proper disk parameter header address. Each disk descriptor is 16 bytes
          LD    (diskno),A

          ; Clear the disk buffer.
          CALL  _flush

          ; Return the disk parameter block to the caller (BDOS) so it knows the disk geometry.
          LD    L, A     ; l=disk number 0, 1, 2, 3
          LD    H, 0     ; high order zero
          ; Although there are only 4 disks, this allows for there to be many more
          ADD   HL,HL    ; *2
          ADD   HL,HL    ; *4
          ADD   HL,HL    ; *8
          ADD   HL,HL    ; *16 (size of each header)
          LD    DE, dpbase
          ADD   HL,DE    ;hl=,dpbase (diskno*16) Note typo here in original source. Points to the
                         ;start of the correct disk base block.
          RET

; ********** HOME *********
; Move to track 0 of the current drive. Translate this call
; into a settrk call with Parameter 00
home:     LD    BC, 0
          ; Fall through to: settrk

; ********** SETTRK *********
; set track given by register BC
settrk:   LD    (track),BC
          RET

; ********** SETDMA *********
; set  dma address given by registers b and c. The address is simply stored for later.
setdma:   LD  (dmaad),BC  ;save the address
          RET

; ********** SETSEC *********
; set sector given by register c
setsec:   LD  (sector),BC
          RET

; ********** READ *********
; Read one CP/M sector from disk.
; Return a 00h in register a if the operation completes properly, and
;          01h if an error occurs during the read.
; Disk number in 'diskno'
; Track number in 'track'
; Sector number in 'sector'
; Dma address in 'dmaad' (0-65535)
; Simple implementation:
;   + Work out which 512 byte block contains the sector
;   + Read the 512 byte block
;   + Copy out the 128 byte requested block
;
read:     CALL  _primebuf
          ; Copy the relevant portion of the buffered block.
          CALL  _calcoff
          LD    DE,(dmaad)
          LD    BC,128
          LDIR
          XOR   A            ; Never fails (!)
          RET

; ********** WRITE *********
; Write one CP/M sector to disk.
; Return a 00h in register A if the operation completes properly, and
; 0lh if an error occurs during the read or write.
; Disk number in 'diskno'
; Track number in 'track'
; Sector number in 'sector'
; Dma address in 'dmaad' (0-65535)
write:    CALL  _primebuf
          ; Calc offset into block buffer
          CALL  _calcoff
          EX    DE,HL        ; DE contains address in block buffer (where to put the data)
          ; Copy 128 bytes from DMA address into the block buffer
          LD    HL,(dmaad)   ; Where we want to copy FROM
          LD    BC,128
          LDIR
          ; Buffer contains new data. Write to the SD card.
          LD      DE,(blkstrt)
          LD      HL,(blkstrt+2)

          ; LD      BC,blkbuf
          ; CALL    SD_WBLK
          LD      C,A_DSKWR
          RST     30h         ; BLOCK WRITE
_werr:    XOR     A
          ; INC   A
          RET

; --- If CPM system load fails then jump back to the monitor
_errld:   LD    E,'X'
          LD    C,A_TXCHR
          RST   30h
          XOR   A
          OUT   (60h),A
freeze:   JR    freeze


CPMLOAD:  ; Work out how many blocks need to be loaded
          LD   HL,JMPTAB
          LD   BC,CCP
          OR   A            ; Clear carry
          SBC  HL,BC        ; Numer of bytes
          ADD  HL,HL        ; x2
          INC  H
          LD   A,H          ; A is going to be the block counter. B is already in use
          LD   BC, 0        ; BC has the next sector to read, starting from zero
          LD   HL, CCP      ; base of cp/m (initial load point)

load1:    ; load  one more sector
          PUSH  AF
          PUSH  BC          ; save blk count, current sector
          PUSH  HL          ; save dma address
          CALL  setsec      ; set sector address from register BC (16 bit)
          POP   BC          ; recall dma address to BC
          PUSH  BC          ; replace on stack for later recall
          CALL  setdma      ; set dma address from BC
          ;
          ;  drive set to 0, track set, sector set, dma address set
          CALL  read
          OR    A          ; Read error if A != 0
          JP    NZ,_errld  ; Retry the entire boot if an error occurs
          ;
          ;  no error, move to next sector
          POP   HL         ; recall dma address
          LD    DE, 80h    ; dma=dma+128
          ADD   HL,DE      ; new dma address is in HL
          POP   BC         ; recall sector number
          INC   BC         ; point to next sector
          POP   AF         ; Get the remaining block count back
          DEC   A          ; Any left to process?
          JR    NZ,load1   ; Yes
          RET


; _HEX_CHRS: DEFB  "0123456789ABCDEF"
; _HELLO:    DEFB  "CP/M 2.2 BIOS - Peter Wilson 2023"
;            DEFB   0


; -- _primebuf
; Check whether the page referencing disk/track/sector is already in the buffer. If not then load
; that page into the buffer and return.
_primebuf: CALL    _calcblk        ; blkstrt now contains the correct SD card address
           LD      DE,blkstrt
           LD      HL,lststrt
           LD      B,4
_chn:      LD      A,(DE)
           CP      (HL)
           JR      NZ,_nomtch
           INC     HL
           INC     DE
           DJNZ    _chn

           ; Requested block matches currently loaded bock.
           RET

_nomtch:   ; Address block mismatch. Load the new block. The loader already has
           ; the DMA address for our block buffer
           PUSH    HL
           PUSH    DE
           PUSH    BC

           LD      DE,(blkstrt)
           LD      HL,(blkstrt+2)
           LD      (lststrt),DE
           LD      (lststrt+2),HL

           LD      C,A_DSKRD
           RST     30h          ; BLK READ

           POP     BC
           POP     DE
           POP     HL
           RET

; The content of the cached sector can no longer be relied upon so invalidate it.
_flush:    LD      HL,$FFFF
           LD      (lststrt),HL
           LD      (lststrt+1),HL
           RET

; -- _calcblk
; Convert disk/track/sector into a logical sector address for the SD card. In
; this case. Each disk has 512 sectors per track. Each sector is 127 bytes so
; 32K per track.
;
; Store result in 'blkstrt'.
;
;   +------------+------------+------------+------------+
;   |  virtdisk  |  Not Used  | N/U |<--13 bit offset-->|
;   +------------+------------+------------+------------+
;
; Logical disk layout is 64 (2^5) tracks each comprising 512 sectors (2^9) = 2^14
;
;    (sector & 0fffCh >> 2) : Converts sector for SDCard block. Will be a value from 0..128
_calcblk:  LD     HL,(sector)     ; Sector is in the range 0 - 512 (9 bits).
           LD     A,0FCh          ; Round sector down to 512 byte SD block
           AND    L               ; By masking out the 2 least sig. bits

           ; ZLoader blocks are 512 bytes so need to divide CP/M sector by 4 to get the
           ; sector that contains the CP/M track

           ; Divide by 2. After this the value of H contains no useful information
           SRL    H
           RRA
           LD     L,A

           ; L now contains the partial ZL sector number. Get the track number into
           LD     A,(track)
           AND    3Fh             ; Mask the lower 6 bits (0-63)
           LD     H,A             ; Working register

           ; HL now addresses logical 256 byte area. Divide again to get 512 byte sector size
           SRL    H
           RR     L

           ; This is the least significant 16 biyes of the address (13 bits relevant)
           LD     (blkstrt),HL    ; Store LS 16 bit word of SD card address

           ; The upper 16 bits at the moment: top byte is disk number, lower is zeros

           ; CPM disk number is 0-3. Let ZLoader do the mapping
           EX     DE,HL
           LD     L,0
           LD     A,(diskno)
           LD     H,A
           LD     (blkstrt+2),HL  ; Store as next byte of offset.

           ; Full 32 bit logical SDCard address suitable for the ZLoader is in HLDE. The
           ; same address is stored in blkstrt.
           RET

; -- _calcoff
; Return HL as the offset into the blkbuf of the currently selected sector.
; HL: Calculated buffer offset.
; A not saved
_calcoff:   PUSH  DE
            LD    HL,0
            LD    A,(sector)
            AND   03h
            RRCA
            RR    L
            RRCA
            RL    H
            LD    DE,blkbuf
            LD    A,E
            ADD   L
            LD    L,A
            LD    A,D
            ADC   A,H
            LD    H,A         ; HL: Start of sector data
            POP   DE
            RET
;
;  the remainder of the cbios is reserved uninitialized
;  data area, and does not need to be a Part of the
;  system  memory image (the space must be available,
;  however, between"begdat" and"enddat").
;
; The next 4 bytes describe a sector request and are passed directly to the loader
; API. DO NOT CHANGE THE ORDER OF THESE BYTES!
diskno:  DEFS  1    ;disk number 0-15
track:   DEFS  2    ;two bytes for expansion
sector:  DEFS  2    ;up to 256 sectors per track
dmaad:   DEFS  2    ;direct memory address

; SDCard addressing is a 32 bit number with the following format:
;   +------------+------------+------------+------------+
;   |  virtdisk  |  Not Used  | N/U |<--13 bit offset-->|
;   +------------+------------+------------+------------+
blkstrt: DEFW  0, 0 ;4 byte SD card logical address for a 512 byte block
blkoff:  DEFB  0    ;Offset into blkstrt for requested block
blkbuf:  DEFS  512  ;buffer to receive a single 512B block

lststrt: DEFW  0FFFFh, 0FFFFh ; Which sector did we last read?

  DSEG
;  scratch ram area for bdos use
begdat:   EQU  $          ;beginning of data area
dirbf:    DEFS  128       ;scratch directory area
all00:    DEFS  513       ;allocation vector 0
all01:    DEFS  513       ;allocation vector 1
all02:    DEFS  513       ;allocation vector 2
all03:    DEFS  513       ;allocation vector 3
; chk00:    DEFS  128     ;check vector 0
; chk01:    DEFS  128     ;check vector 1
; chk02:    DEFS  128     ;check vector 2
; chk03:    DEFS  128     ;check vector 3
;
_endall:  EQU  $
