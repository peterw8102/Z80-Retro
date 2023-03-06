;==================================================================================
; Mount a virtual disk onto a logical drive
;
; Syntax: MOUNT Drive diskNum
;
; Drive:    CP/M letter from A-D tp accept the virtual disk
; DiskNum:  The virtual disk to mount (1-1023)
;
; NOTE: This utility bypasses CP/M and talks to ZIOS directly.
;==================================================================================
import zapi.asm

TPA     .EQU  100H
RESTRT  .EQU  0H
BDOS    .EQU  5H

; BDOS FUNCTION CODES
CONIO   .EQU  6      ; C_RAWIO:        Read a character from the console without ehoing ()
CONINP  .EQU  1      ; C_READ:         Read one character from the console and echo
CONOUT  .EQU  2      ; C_WRITE(E):     Write a character to the console
PSTRING .EQU  9      ; C_WRITESTR(DE)  Write the string (DE) to console. Strin terminated by a $ character
CLOSEF  .EQU  16     ; F_CLOSE:        Close file (DE) = FCB
DELF    .EQU  19     ; F_DELETE:       Delete a file (with wildcards)
WRITES  .EQU  21     ; F_WRITE:        Write data to (DE) = FCB. Data set by previous call to set DMS addr, length 128 bytes
MAKEF   .EQU  22     ; F_MAKE:         Create a file. (DE) is the FCB
SETUSR  .EQU  32     ; F_USERNUM(E)    Set the user number 0-15 from E, E: 255 -> return current user number in A
DMAOFF  .EQU  26     ; F_DMAOFF        Set the address (DE) for the next file I/O operation

CR      .EQU  0DH
LF      .EQU  0AH

FCB     .EQU  05CH    ; Address of FCB???
BUFF    .EQU  080H    ; Buffer at address 80h

; Define the fields in the FCB at address FCB (5CH) - 36 byte structure
FCB_DRV .EQU  FCB     ; 1 byte drive number
FCB_FN  .EQU  FCB+1   ; 8 byte file name in 7bit ASCII. 8th bit of each byte is a control flag (!!)
                      ; F1'-F4' - User defined
                      ; F5'-F8' - Interface attributes - controls how BDOS calls work on the file
FCB_TYP .EQU  FCB+9   ; File type (3 characters, file name extension) - 7bit ASCII with bit 8 as control:
                      ; T1' Read Only flag
                      ; T2' System (hidden) file
                      ; T3' Archive - Set if file NOT changed
FCB_EX  .EQU  FCB+12  ; Set to 0 on open - controlled by BDOS
FCB_S1  .EQU  FCB+13  ; Reserved for BDOS
FCB_S2  .EQU  FCB+14  ; Reserved for BDOS
FCB_RC  .EQU  FCB+15  ; Set to zero when opening a file then controlled by BDOS
FCB_AL  .EQU  FCB+10H ; FAT - controlled by BDOS. 16 bytes
FCB_CR  .EQU  FCB+20H ; Current record within the extent. Initialse to zero then let BDOS manage it
FCB_Rn  .EQU  FCB+21H ; Random Access record number - 2 bytes

STACK   .EQU  7FF0h   ; Give ourselves a LOT of stackspace
OSSTACK .EQU  7FF8h   ; Place to save old stack before return

        ASEG
        .ORG TPA

        LD    (OSSTACK),SP
        LD    SP,STACK

        ; Clear initial values
        LD    A,(BUFF)
        OR    A
        LD    (buffCnt),A
        LD    HL,BUFF+1
        LD    (buffPtr),HL
        LD    A,$FF
        LD    (DRV_LET),A

START:  ; Drive letter first
        CALL  SKPSPC
        OR    A
        JR    Z,MNTTAB
        SUB   'A'
        JR    C,_baddrv
        CP    4
        JR    NC,_baddrv

        ; A is the drive letter.
        LD    (DRV_LET),A

        ; Must be followed by a ':'
        CALL  GETCHR
        JR    Z,_baddrv
        CP    ':'
        JR    NZ,_baddrv

        ; And then (after optional whitespace) an '=' sign. If not present then display this drive's mount point.
        CALL  SKPSPC
        CP    '='
        JR    NZ,MNTTAB

        ; Mounting a new drive. Format for this release is 'sd?:nnnn' where ? is 0 or 1 (drive number) and
        ; `nnn` is the logical volume or drive number.
        CALL  SKPSPC
        CP    'S'
        JR    NZ,_badt
        CALL  GETCHR
        CP    'D'
        JR    NZ,_badt
        CALL  GETCHR
        SUB   '0'
        JR    C,_badt
        CP    2
        JR    NC,_badt

        ; Have a drive
        LD    (DRV_NUM),A

        ; The a ':'
        CALL  GETCHR
        CP    ':'
        JR    NZ,_badt

        ; Next, want a decimal number (=> HL).
        CALL  GET_DEC
        JR    C,_baddsk

        LD    (DSK_NO),HL

        ; Must be less than 1024
        LD    A,H
        AND   $FC
        JR    NZ,_baddsk
        LD    A,H
        OR    L
        JR    Z,_baddsk     ; Don't allow disk zero (0) - reserved

        ; Explain what we're doing
        LD    DE,mountMsg
        LD    C,PSTRING
        CALL  BDOS

        LD    A,(DRV_NUM)
        ADD   '0'
        CALL  PUTCHR
        LD    A,':'
        CALL  PUTCHR
        LD    HL,(DSK_NO)
        CALL  WRITE_D

        LD    DE,onDrive
        LD    C,PSTRING
        CALL  BDOS

        LD    A,(DRV_LET)
        ADD   'A'
        CALL  PUTCHR
        LD    A,':'
        CALL  PUTCHR
        LD    A,0Dh
        CALL  PUTCHR
        LD    A,0Ah
        CALL  PUTCHR

        ; Call ZIOS (bypasses CP/M)
        LD    A,(DRV_NUM)
        LD    E,A
        LD    A,(DRV_LET)
        LD    D,A
        LD    HL,(DSK_NO)
        LD    C,10           ; Map logical disk number
        RST   30h            ; Call ZIOS

        JP    REBOOT


_badt:   LD    DE,badtarg
         JR    _doerr

_baddsk: LD    DE,dskerr
         JR    _doerr

_baddrv: LD    DE,drverr
         JR    _doerr

_error:  LD    DE,error
_doerr:  LD    C,PSTRING
         CALL  BDOS
         JP    REBOOT

; -------- MNTTAB
; If there were no parameters then list the current mount table.
MNTTAB:  LD    C,A_DVINV
         LD    HL,devmap
         RST   30h

         ; And get the current mount table
         LD    B,0
         LD    C,A_QDSKMP
         LD    HL,drvmap
         RST   30h

         LD    DE,devs
         LD    C,PSTRING
         CALL  BDOS

         ; Walk the first 4 entries of the drive map and display (ZLoader
         ; supprts 16 drives but this CP/M BIOS supports only drives A thru D)
         LD    HL,drvmap
         LD    B,4           ; Count
         LD    A,'A'

_nxtdrv: PUSH  AF
         ; PUSH  HL
         CALL  PUTCHR        ; Write drive name
         LD    A,':'
         CALL  PUTCHR
         LD    A,'='
         CALL  PUTCHR

         LD    A,(HL)        ; Device ID - match devmap
         INC   HL
         CALL  TXLATEDEV
         LD    A,':'
         CALL  PUTCHR

         LD    E,(HL)
         INC   HL
         LD    D,(HL)
         INC   HL
         EX    DE,HL
         CALL  WRITE_D
         EX    DE,HL
         LD    A,10
         CALL  PUTCHR
         LD    A,13
         CALL  PUTCHR
         POP   AF
         INC   A
         DJNZ  _nxtdrv
         ; CALL  NL
         JP    REBOOT

         ; Get the table
;          LD    HL,devmap
; _nxtdev: PUSH  HL
;          LD    E,(HL)       ; Device ID in E
;          LD    A,E
;          OR    A            ; Test for end of table
;          INC   HL
;          LD    D,(HL)       ; Device type
;          JR    Z,_enddev
;
;          ; Only display block devices
;          LD    A,D
;          CP    DM_BLK
;          JR    Z,_use
;          CP    DM_MBLK
;          JR    NZ,_skip
;
; _use:    LD    A,7
;          CALL  ADD8T16     ; Skip to name
;          LD    B,8          ; Display device name
; _nc1:    LD    A,(HL)
;          INC   HL
;          OR    A
;          JR    Z,_endn
;          CALL  PUTCHR
;          DJNZ  _nc1
         ; Output device info
         ; TBD
         ; And then a new line
; _endn:   CALL  NL
; _skip    POP   HL
;          LD    A,16
;          CALL  ADD8T16
;          JR    _nxtdev

; Device in A - convert to a string. Device map is broken at the moment
; so this version is a literal translaction. Needs to be fixed when ZIOS
; does something more sensible with the device table.
TXLATEDEV: PUSH AF
           PUSH BC
           PUSH DE
           PUSH HL
           ADD  '0'
           PUSH AF
           LD   A,'s'
           CALL PUTCHR
           LD   A,'d'
           CALL PUTCHR
           POP  AF
           CALL PUTCHR
           POP  HL
           POP  DE
           POP  BC
           POP  AF
           RET

_enddev: POP   HL
         JP    REBOOT

;         ; Output whatever is in the command buffer.
;         LD    A,(BUFF)    ; Number of characters
;         LD    B,A
;         LD    HL,BUFF+1
;
; lp:     LD    A,(HL)
;         INC   HL
;         PUSH  HL
;         CALL  PUTCHR
;         POP   HL
;         DJNZ  lp
;         JP    REBOOT

SKPSPC:  CALL  GETCHR
          CP    20h
          JR    Z,SKPSPC
          RET

WASTSPC:  LD    A,(buffCnt)
          OR    A
          RET   Z
          LD    HL,(buffptr)
          LD    A,(HL)
          CP    ' '
          RET   NZ
          ; Not a space so step on
          INC   HL
          LD    (buffptr),HL
          LD    A,(buffCnt)
          DEC   A
          LD    (buffCnt),A
          JR    SKPSPC

; Get the next character from the input buffer
GETCHR: LD    A,(buffCnt)
        OR    A
        RET   Z            ; No more characters
        DEC   A
        LD    (buffCnt),A
        PUSH  HL
        LD    HL,(buffPtr)
        LD    A,(HL)
        INC   HL
        LD    (buffPtr),HL
        POP   HL
        OR    A
        RET

; -------- GET_DEC - Read a (16 bit) decimal number into HL with C flag on error
; If there is no valid decimal number then the:
;    HL returned as zero
;    C  flag SET
; otherwise the 16 bit number is returned in HL
GET_DEC:  PUSH  BC
          LD    B,A           ; Save A
          XOR   A
          LD    H,A           ; Working value in HL, set to zero
          LD    L,A
          LD    C,A           ; Count number of characters
          CALL  SKPSPC
next_dc:  JR    Z, _fin       ; Same end caseas GET_HEX
cont_dc:  CALL  DEC2BIN       ; Decimal character?
          JR    C,_fin
          PUSH  DE            ; Muliply HL by 10
          ADD   HL, HL        ; x2
          LD    E,L
          LD    D,H
          ADD   HL, HL        ; x4
          ADD   HL, HL        ; x8
          ADD   HL,DE         ; x10
          POP   DE

          CALL  ADD8T16

          INC   C
          CALL  GETCHR
          JR    next_dc

_fin:     LD    A,C
          OR    A
          LD    A,B
          POP   BC
          RET

; -------- DEC2BIN
; Convert the (ASCII) character in A to a binary number. A contains ASCII '0' to '9' inclusive
; Carry flag set if A contains an invalid character (out of range). Result in A.
DEC2BIN:    SUB   '0'         ; Minimum value
            JR    C,inv
            CP    10
            JR    NC,inv
            OR    A           ; Clear carry
            RET
inv:        SCF
            RET

; ------ ADD8T16
; Add A to HL and correctly deal with carry from L to H. HL and A changed. No
; other registers used or changed.
ADD8T16:  ADD   L
          LD    L,A
          RET   NC
          INC   H
          RET

; Write A to output
PUTCHR: PUSH  HL
        PUSH  BC
        PUSH  DE
        PUSH  AF
        LD    C,CONOUT
        LD    E,A
        CALL  BDOS
        POP   AF
        POP   DE
        POP   BC
        POP   HL
        RET

; NL
NL:     LD    A,10
        CALL  PUTCHR
        LD    A,13
        CALL  PUTCHR
        RET

; ------ WRITE_D
; Convert a 16 bit number in HL to an ASCII decimal number and print out
WRITE_D:  PUSH   HL
          PUSH   DE
          PUSH   BC
          LD     A,L
          OR     H
          JR     NZ,_dodec
          LD     A,'0'
          CALL   PUTCHR
          JR     _andret
_dodec:   CALL   _NUM2D
_andret:  POP    BC
          POP    DE
          POP    HL
          RET

_NUM2D:   LD     D,' '
          LD     BC,-10000
          CALL   NUM1
          LD     BC,-1000
          CALL   NUM1
          LD     BC,-100
          CALL   NUM1
          LD     C,-10
          CALL   NUM1
          LD     C,B

NUM1      LD     A,'0'-1
NUM2      INC    A
          ADD    HL,BC
          JR     C,NUM2
          SBC    HL,BC
          CP     A,'0'
          JR     NZ,_isnotz
          LD     A,D
          CP     ' '
          RET    Z
          CALL   PUTCHR
          RET

_isnotz:  LD     D,'0'
          CALL   PUTCHR
          RET


; ------ TOUPPER
; Take the character in A. If it's a lower case letter then promote to uppercase.
TOUPPER:  CP    'a'                ; Lower case -> upper case
          RET    C
          CP    'z'+1
          RET   NC
          ADD   A,'A'-'a'
          RET


; ------ WRITE_16
; Convert the 16 bit value in HL to 4 ASCII caracters and send to the console.
; All registers preserved.
WRITE_16:  PUSH AF
           LD   A,H
           CALL WRITE_8
           LD   A,L
           CALL WRITE_8
           POP  AF
           RET

; ------ WRITE_8
; Convert the 8 bit number in A into HEX characters in write to the console
; A: number to write (not preserved)
WRITE_8:   PUSH HL
           CALL HEX_FROM_A
           LD   A,H
           CALL PUTCHR
           LD   A,L
           CALL PUTCHR
           POP  HL
           RET

; ------------- HEX_FROM_A
; IN  - A:  Number to convert to HEX
; OUT - HL: Two character converted value - H MSB
; HL and A NOT preserved
HEX_FROM_A: PUSH  DE
            PUSH  AF
            LD    HL, _HEX_CHRS
            PUSH  HL
            ; LSB first
            AND   0Fh
            ADD   A,L
            LD    L,A
            JR    NC,_hs1
            INC   H
_hs1:       LD    E,(HL)
            POP   HL
            ; MSB
            POP   AF
            RRA
            RRA
            RRA
            RRA
            AND   $0F
            ADD   A,L
            LD    L,A
            JR    NC,_hs2
            INC   H
_hs2:       LD    D,(HL)
            EX    DE,HL
            POP   DE
            RET

REBOOT:      LD    SP,(OSSTACK)
             ; JP    REBOOT
             JP    RESTRT

; DATA AREA
buffCnt       .DB     00H
buffPtr       .DW     0000H

DRV_LET       .DB     00H
DSK_NO        .DW     0000H
DRV_NUM       .DB      0

printCount    .DB     0H
OKMess        .BYTE  " - OK$"
mountMsg      .BYTE  "Mounting volume: sd$"
onDrive       .BYTE  " on virtual drive: $"
error         .BYTE  "Missing paramerers$"
drverr        .BYTE  "Invalid drive (A-D)$"
dskerr        .BYTE  "Invalid disk (1-1023)$"
devs          .BYTE  "Mounted drives:",10,13,"$"
badtarg       .BYTE  "Bad target. Should be in the form sd?:nnnn",10,13,'$'
_HEX_CHRS     .BYTE  "0123456789ABCDEF"

devmap::         DS    256            ; For the list of available devices
drvmap::         DS     64            ; For list of mounted drives
  .END
