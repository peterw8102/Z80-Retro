; **********************************************
; Implements SDCard boot logic:
;   bos?              ; List bootable images
;   bos-[n]           ; Load but don't run image 'n'
;   bos [n]           ; Load and run image 'n'
;   wb [params]       ; Write an entry to the boot menu
; **********************************************
; Copyright Peter Wilson 2022
; https://github.com/peterw8102/Z80-Retro
; **********************************************
import defs.asm
import config.asm
import zapi.asm

import pcb_def.asm
import zlib.asm
import zios.asm
import zload.asm

  public SDLOAD,SDRUN,SDLDDEF
  public SBCALCS
  public SDDIR,WBOOT

  extrn  main
  extrn  SDPAGE,SADDR,SDMP_MD,SDMP_L,SETDMA,SDBREAD
  extrn  AUTO_RUN
  extrn  E_NEMPT,E_NOTF,E_BADPS,E_ERROR,E_PRTERR

  extrn  DMP16

ENTRIES    EQU     16
ENTSIZE    EQU     23
TABSIZE    EQU     ENTRIES * ENTSIZE
ENDMOFF    EQU     TABSIZE
CSUMOFF    EQU     TABSIZE + 1
NUMWDS     EQU     (ENDMOFF >> 1)

; ------- SDDIR
; List the set of bootable images
;
; Record format
; All records are ENTSIZE bytes and there are ENTRIES in the first SDCard sector;
;  0 - 00 (1): TYPE              : 0: unused, 1: used
;  1 - 01 (1): ID                : Numeric ID for this image. Must be unique and non-zero.
;  2 - 02 (8): NAME              : Printable
; 10 - 0A (1): DEVICE            : Qualifies SD_ADDR by identifying the SDCard device number
; 11 - 0B (4): SD_ADDR           : Byte offset into SD Card (SD card address)
; 15 - 0F (2): LOAD_ADD          : Address in application space to load this image
; 17 - 11 (2): LENGTH            : Number of bytes to load
; 19 - 13 (2): EXEC_ADDR         : Once loaded, exectre from this address
; 21 - 15 (1): FLAGS             : Bit 0. If bit 0 set then load libraries, otherwise DON'T
SDDIR:    CALL  BTPREP
          LD    HL,_BADCS
          JR    NZ,E_PRTERR    ; Invalid checksum in boot memory so no boot available

_dumpbs:  ; List all known boot sectors
          LD    HL,_SDINFO
          CALL  PRINT_LN
          LD    HL,SDPAGE
          LD    B,ENTRIES
_nxsec:   PUSH  BC
          PUSH  HL
          LD    A,(HL)      ; Record type.
          DEC   A
          JR    NZ,_sksec
          DEC   A           ; Only understand record type 1
          JR    Z,_sksec
          INC   HL
          LD    A,(HL)      ; Unique image ID. Must be non-zero
          OR    A
          JR    Z,_sksec
          INC   HL

          ; Usable so display
          CALL  WRITE_8
          WRITE_CHR ' '
          LD    B,8         ; 8 byte name
_nxc:     LD    A,(HL)
          INC   HL
          OR    A
          JR    NZ,_noch    ; Zero bytes as a spacer
          LD    A,' '
_noch:    RST   08H
          DJNZ  _nxc
          WRITE_CHR ' '

          ; Next byte is the device ID (context for SDAddr)
          LD    A,(HL)
          INC   HL
          CALL  WRITE_8
          WRITE_CHR ' '

          ; SD Card Address is 4 bytes (32 bits) LSW first so need to reverse
          LD    E,(HL)
          INC   HL
          LD    D,(HL)
          INC   HL
          PUSH  DE
          LD    E,(HL)
          INC   HL
          LD    D,(HL)
          INC   HL
          EX    DE,HL
          CALL  WRITE_16
          POP   HL
          CALL  WRITE_16
          EX    DE,HL
          LD    B,3            ; Write next three 16 bit values
_nxsd:    WRITE_CHR ' '
          LD    E,(HL)
          INC   HL
          LD    D,(HL)
          INC   HL
          EX    DE,HL
          CALL  WRITE_16
          EX    DE,HL
          DJNZ  _nxsd

          WRITE_CHR ' '       ; And display the boot flags
          LD    A,(HL)
          CALL  WRITE_8

          CALL  NL
_sksec:   POP   HL
          LD    A,ENTSIZE
          CALL  ADD8T16
          POP   BC
          DJNZ  _nxsec

          JR    main



; ---- FNDIMG
; Find an image with the ID stored in the C register.
;
; If the image is NOT found then HL returns the address of the
; first free slot.
;
; INPUT:   C  - ID of imaged wanted
; OUTPUT: HL  - Address of boot structure entry (if found)
;             - If not found address of first free slot or zero if table full
;          Z  - Set if found (NZ if not found)
;
FNDIMG:   LD    HL,SDPAGE
          LD    DE,0
          LD    B,ENTRIES+1    ; (Should this be just ENTRIES?)

          ; Check this block - should countain 01 if it's usable
_again:   LD    A,(HL)
          DEC   A
          JR    Z,_usdbs

          ; Empty slot
          LD    A,D
          OR    A
          JR    NZ,_nxtbl
          LD    E,L            ; Save as free slot
          LD    D,H
          JR    _nxtbl

_usdbs:   INC   HL             ; Usable block - is this the OS we want
          LD    A,(HL)
          DEC   HL
          CP    C              ; The one we want?
          RET   Z              ; Yes. HL points to start of block descriptor

_nxtbl:   LD    A,ENTSIZE      ; Step on to next slot
          CALL  ADD8T16
          DJNZ  _again
          LD    L,E            ; Not found if we reach here. Return first free slot in HL
          LD    H,D
          XOR   A              ; HL to available slot
          INC   A
          RET                  ; Return NZ for Not Found

; ---- SDLDDEF
; Load SDCard boot image with tag ID 1.
SDLDDEF:  CALL  BTPREP
          LD    HL,_BADCS
          JR    NZ,E_PRTERR    ; Invalid checksum in boot memory so no boot available
          LD    C,1
          JR    _bsload


; ------- SDLOAD
; Load a bootable image but DON'T run it!
SDLOAD:   XOR   A
          LD    (AUTO_RUN),A   ; Auto-run off
          JR    _sdcont

; ------- SDLOAD
; SD Card Load. Options:
;    No parameters - Load OS type 01 - Default
;    With a digit (0-9) look for and load a specific type
;    With '?' - display all available options and exit
SDRUN:    ; Default to auto-run
          LD    A,1
          LD    (AUTO_RUN),A

_sdcont:  CALL  BTPREP

          ; Calculate the checksum
          JR    NZ,E_ERROR

          CALL  WASTESPC
          LD    C,1
          JR    Z,_bsload      ; Boot OS with ID 01

_sdauto:  CALL  GET_DEC        ; Get the decimal type number to boot
          JR    C,E_BADPS
          LD    A,H
          OR    A
          JR    NZ,E_BADPS
          LD    C,L            ; 'A' contains the image ID to boot

_bsload:  LD    HL,_BSD
          CALL  PRINT
          CALL  FNDIMG
          JR    NZ,E_NOTF

          ; Found the one we want so load
_doload:  INC   HL    ; Over the use flag
          INC   HL    ; Over the ID

          ; At start of name. Print name as we scan past it.
          LD    B,8
_nchr:    LD    A,(HL)
          INC   HL
          OR    A
          JR    Z,_na
          RST   08h
_na:      DJNZ  _nchr
          CALL  NL

          ; Rest of the entry:
          ; 1 byte drive/device number
          ; 4 byte SD card block address
          ; 2 byte load address
          ; 2 byte length
          ; 2 byte exec address
          ; 1 byte flags
          LD    A,(HL)
          INC   HL
          LD    (SDDRV),A

          LD    E,(HL)
          INC   HL
          LD    D,(HL)
          INC   HL
          LD    (SDADDR), DE

          LD    E,(HL)
          INC   HL
          LD    D,(HL)
          INC   HL
          LD    (SDADDR+2), DE   ; SDCard address configured. This is EITHER an
                                 ; absolute sector address or relative to a virtual
                                 ; drive depending on bit 7 of the flag byte.

          LD    E,(HL)           ; Next two bytes are the application space load address
          INC   HL
          LD    D,(HL)
          INC   HL
          LD    (LOADADD),DE

          LD    E,(HL)
          INC   HL
          LD    D,(HL)           ; DE is the length
          INC   HL
          PUSH  DE               ; Store length on the stack

          LD    E,(HL)           ; Store the exec address
          INC   HL
          LD    D,(HL)
          LD    (EXECADD),DE
          INC   HL
          LD    A,(NVRAM)        ; Default flag byte
          AND   ~CF_DEBUG        ; Clear the lower bits and MSB
          OR    A,(HL)           ; Add in our own flags (OS, break handler, debugger)
          LD    (P_FLAGS),A      ; Store in the PCB

          ; Stored address is always absolute but if it was specified as relative when
          ; the boot record was created then map the address into logical disk drive A
          RRA
          JR    NC,_absadd

          ; Relative so MAP the logical drive number into virtual drive zero
          ;  (the A drive). Get the most significant 16 bits of the address.
          LD    HL,(SDADDR+1)   ; Logical disk number * 64
          LD    A,(SDDRV)       ; Get the physical SDCard number...
          LD    E,A             ; ... into E
          LD    D,0             ; Want to map drive A
          LD    B,1             ; Use raw mapping optimisation
          LD    C,A_DSKMP       ; Map disk
          RST   30h             ; Map the drive

_absadd:  ; Need to work out how many whole blocks we need to load
          POP   DE              ; Get the length back from the stack
          LD    HL,(LOADADD)    ; And where to start loading.

          ; Always going to be reading raw for this function
          LD     A,A_DSKRW
          CALL   SDBREAD

          ; Load complete, auto-execute?
          LD     HL,(EXECADD)

          LD    A,(AUTO_RUN)
          OR    A
          JR    NZ,PR_RUN
          JR    main

; ---- WBOOT
; Write a boot image to the boot manager. There are two options:
;   WB id DELETE     - Remove boot information with specified ID
;   WB id SDaddr laddr len execaddr MODE "name"
; id:       (dec) The unique boot ID. If there is already an image with this ID it must first be deleted
; DELETE:   If specified, delete any image with this ID
; SDaddr:   Where is this on the SDCard? Absolute or virtual drive relative.
; laddr:    (hex) Where to start loading into application space
; len:      (hex) Number of bytes to load
; execaddr: (hex) Run address
; MODE:     (chr) Set of characters identifying flags:
;                   S:  Standalone - doesn't use Zloader services. Launch and forget
;                   A: Image uses ZLoader API but not break handler
;                   B: Image uses Zloader API and serial I/O break handler should be installed (debugger)

PBUF        EQU   SCRATCH
DFLAG       EQU   SCRATCH+32
IMGPOS      EQU   SCRATCH+34

; WB 7 00006000 D600 2800 ECCD 01 CP/M
; WB 9 0:C D600 2800 ECCD 1 CPM TEST
; WB 1 0:m DB00 2300 F10C 1 ZIOS
; wb 1 0:b db00 2300 f10c 01 cpm
            ; Prepare/check the boot records
WBOOT:      CALL  BTPREP
            JR    NZ,BADCSWR      ; Invalid checksum in boot memory so no boot available

            LD    HL,PBUF
            LD    BC,32
_xxx:       LD    (HL),0
            INC   HL
            DJNZ  _xxx


            CALL  GET_DEC         ; MUST be an image ID, returned in HL
            JR    C,E_BADPS

            LD    IX,PBUF
            LD    C,L             ; 'C' contains the ID of the image we want
            LD    (IX+1),C
            LD    A,1
            LD    (IX),A

            CALL  WASTESPC
            LD    HL,(INPTR)
            LD    DE,_DEL
            CALL  STRCMP
            LD    (DFLAG),A       ; True if it's a delete request.

            CALL  FNDIMG
            LD    A,(DFLAG)     ; Get the delete flag back
            JR    Z,_fnd

            ; Not found so can be insert but delete leads to not found.
            OR    A
            JR    Z,E_NOTF

            ; Inserting. Need the data. HL currently points at the start of the new
            ; record. Save this and build a dummy in the SCRATCH buffer.
            LD    (IMGPOS),HL

            ; Get the parameters
            ; 1: SDAddr
            CALL WASTESPC
            CALL SADDR
            JR   C,E_BADPS



            ; The read command is either relative (for a mapped drive reference) or
            ; raw for an absolute sector address.
            LD   A,(SDMP_MD)
            CP   A_DSKRD        ; Relative read
            LD   A,0
            LD   (IX+10),A      ; Clear drive number (abs can only reference SDCard 1)
            LD   DE,(SDMP_L)
            LD   HL,(SDMP_L+2)


            JR   NZ,_sdabs
            LD   C,A_DSKTL
            RST  30H

            PUSH DE
            PUSH HL
            PUSH AF
            CALL NL
            CALL WRITE_16
            EX   DE,HL
            CALL WRITE_16
            CALL NL
            POP  AF
            POP  HL
            POP  DE



            LD   (IX+10),B      ; Store drive number
            LD   A,$80          ; It's a logical address so translate to segment
_sdabs:     LD   (IX+11),E      ; SDCard address
            LD   (IX+12),D
            LD   (IX+13),L
            LD   (IX+14),H
            LD   (IX+21),A      ; Flags


            ; 2: Load address
            CALL WASTESPC
            CALL INHEX_4
            JR   C,E_BADPS
            LD   (IX+15),L
            LD   (IX+16),H

            ; 3: Length
            CALL WASTESPC
            CALL INHEX_4
            JR   C,E_BADPS
            LD   (IX+17),L
            LD   (IX+18),H

            ; 4: Execute address
            CALL WASTESPC
            CALL INHEX_4
            JR   C,E_BADPS
            LD   (IX+19),L
            LD   (IX+20),H

            ; 5: Flag
            CALL WASTESPC
            CALL INHEX_2
            JR   C,E_BADPS

            AND  $7F           ; Clear bit 7 for our flag
            OR   A,(IX+21)     ; Mix in the virtual disk flag
            LD   (IX+21),A     ; and store....

            ; Rest of the line (max 8 characters) is the image name, pad with zeroes
            CALL WASTESPC
            LD   B,8
            LD   HL,PBUF+2
_nname:     CALL BUFCHR
            LD   (HL),A
            INC  HL
            DJNZ _nname

            LD   HL,PBUF+22
            LD   B,10
            XOR  A
_clr1:      LD   (HL),A
            INC  HL
            DJNZ _clr1

_ename:     ; Write into the available slot.
            LD    HL,PBUF
            LD    DE,(IMGPOS)
            LD    BC,ENTSIZE
            LDIR


SAVEBT:     CALL  SBCALCS

            ; Write checksum into page buffer
            LD    A,$FF
            LD    (SDPAGE+ENDMOFF),A     ; End marker
            LD    (SDPAGE+CSUMOFF),HL    ; Checksum

            ; Patched checksum, back to the SDCard
            CALL  SETDMA
            LD    HL,0
            LD    DE,0
            LD    C,A_DSKWW    ; Write raw
            RST   30h

            JR    _dumpbs

            ; Image exists. Allowed to be a delete. Error if insert
_fnd:       OR    A

            ; Zero means it's a delete request
            JR    NZ,E_NEMPT

            ; It's a delete request
_dosdel:    PUSH  HL
            LD    HL,_TODEL
            CALL  PRINT
            POP   HL
            PUSH  HL
            INC   HL
            INC   HL
            LD    B,8
_nc1:       LD    A,(HL)
            RST   08h
            INC   HL
            DJNZ  _nc1
            CALL  NL
            ; Clear the flag
            POP   HL
            XOR   A
            LD    (HL),A
            JR    SAVEBT


; ----- BADCSWR
; Checksum is invalid on an attempt to write to the SDCard boot sector. Ask
; the user whether they want to initialise the data. If so then allow them
; to continue with the write operation.
BADCSWR:  LD      HL,_BADCS
          CALL    PRINT_LN
          LD      HL,_QINIT
          CALL    PRINT
          RST     10h           ; Get next character from keyboard. Must be uppercasse Y
          CP      'Y'
          LD      HL,_NC
          JR      NZ,E_PRTERR
          CALL    NL

          ; Initialise boot sector - fill SDPage with zeroes and an end of table marker
          LD      A,$FF
          LD      HL,SDPAGE
          LD      (HL),A
          INC     HL
          INC     A          ; A now zero
          LD      (HL),A     ; Clear first byte after marker
          LD      D,H
          LD      E,L
          INC     DE
          LD      BC,TABSIZE
          LDIR
          ; And save sector back to SDCard
          JR    SAVEBT




; ---- BTPREP
; Prepare the way for managing the ZLoader boot menu. Load the first 512 bytes from the SDCard
; and set the DMA buffer.
BTPREP:   ; Always need to load the first 512 byte block that includes the boot information
          CALL  SETDMA
          LD    HL,0          ; Sector 0
          LD    DE,0
          LD    B,0           ; SDCard 0
          LD    C,A_DSKRW     ; Raw read
          RST   30h
          ; DROP THROUGH TO CALCULATE THE Checksum

; ----- SBCALCS
; Calculate checksum. Uses a simple shift and add algorithm.
; Result 16 bits. Compare calculated value with existing value and set Z if
; they are the same.
; INPUT:  SDPAGE contains the boot block (SDCard 1, sector 0)
; OUTPUT: HL     contains the calculated checksum for the boot menu data
;         Z      Z is true if the caluclate CS matches the stored CS, NZ otherwise
SBCALCS:  LD    HL,0         ; the calculated partial CS
          LD    DE,SDPAGE    ; Byte we're working on
          LD    B,NUMWDS     ; Number of 16 bit words to calculate sum over

_nxt_wd:  ; Add in the next 16 bits
          LD    A,(DE)
          ADD   L
          LD    L,A
          INC   DE
          LD    A,(DE)
          ADC   H
          LD    H,A
          INC   DE
          DJNZ  _nxt_wd

          ; HL is the new checksum. Compare it to the current value
          ; and set the Z flag if it matches.
          INC   DE         ; Skip end entry marker
          LD    A,(DE)
          CP    L
          RET   NZ
          INC   DE
          LD    A,(DE)
          CP    H
          RET

_SDINFO:     DEFB "ID NAME     DV SDADDR   LOAD  LEN EXEC FL",NULL
_BSD         DEFB "Image: ", 0
_SDEV:       DEFB "sd",NULL
_DEL:        DEFB "DELETE", NULL
_TODEL:      DEFB "Delete image: ", NULL
_BADCS:      DEFB "Invalid checksum in boot menu", NULL
_QINIT:      DEFB "Initialise boot sector? (Y/N)", NULL
_NC:         DEFB 10,13,"Boot data unchanged",NULL

        DSEG


LOADADD:   DEFW    0              ; For the load command, where to start loading binary data
