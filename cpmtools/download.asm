;==================================================================================
; Contents of this file are copyright Grant Searle
; HEX routine from Joel Owens.
;
; You have permission to use this for NON COMMERCIAL USE ONLY
; If you wish to use it elsewhere, please include an acknowledgement to myself.
;
; http://searle.hostei.com/grant/index.html
;
; eMail: home.micros01@btinternet.com
;
; If the above don't work, please perform an Internet search to see if I have
; updated the web page hosting service.
;
; ------
; Peter Wilson: This is basically Grant's code reformatted with a few tweaks,
; for example looking for Ctrl-C and returning to the conssole and ignoring
; carriage return/line feed.
;
;==================================================================================

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
        XOR   A
        LD    (buffPos),A
        LD    (checkSum),A
        LD    (byteCount),A
        LD    (printCount),A
        LD    HL,BUFF
        LD    (buffPtr),HL


WAITLT: CALL  GETCHR
        CP    'U'
        JP    Z,SETUSER
        CP    ':'
        JR    NZ,WAITLT

        ; Seen a ':' so now expecting data. Start by deleting the existing file if it exists
        LD    C,DELF      ; BDOS - F_DELETE file referenced by FCB???? What's being deleted?
        LD    DE,FCB
        CALL  BDOS

        ; And then creating it again
        LD    C,MAKEF
        LD    DE,FCB
        CALL  BDOS

GETHEX: CALL  GETCHR
        CP    '>'         ; After all the HEX data we expect a > terminator
        JR    Z,CLOSE
        LD    B,A         ; This should be the first character of the HEX pair
        PUSH  BC
        CALL  GETCHR
        POP   BC
        LD    C,A

        CALL  BCTOA

        LD    B,A
        LD    A,(checkSum)
        ADD   A,B
        LD    (checkSum),A
        LD    A,(byteCount)
        INC   A
        LD    (byteCount),A

        LD    A,B            ; Get the converted byte back

        LD    HL,(buffPtr)

        LD    (HL),A         ; Store in the next byte of the buffer
        INC    HL
        LD    (buffPtr),HL   ; Move buf pointer forward.

        LD    A,(buffPos)    ; Move buffPos forward
        INC   A
        LD    (buffPos),A
        CP    80H            ; End of buffer (128 bytes)?

        JR    NZ,GETHEX      ; Can fit in more bytes

        LD    C,WRITES
        LD    DE,FCB
        CALL  BDOS
        LD    A,'.'          ; Write out a '.' on each completed block
        CALL  PUTCHR

        ; New line every 32 blocks (4K)
        LD    A,(printCount)
        INC   A
        CP    32
        JR    NZ,noCRLF
        LD    A,CR
        CALL  PUTCHR
        LD    A,LF
        CALL  PUTCHR
        XOR   A
noCRLF: LD    (printCount),A

        LD    HL,BUFF
        LD    (buffPtr),HL    ; Reset to start of buffer

        XOR   A
        LD    (buffPos),A
NOWRITE:
        JR    GETHEX


CLOSE:  LD    A,(buffPos)
        OR    A
        JR    Z,NOWRITE2

        LD    C,WRITES        ; Partial block - write to disk
        LD    DE,FCB
        CALL  BDOS

NOWRITE2:
        LD    A,'|'           ; Write terminal character
        CALL  PUTCHR
        LD    C,CLOSEF
        LD    DE,FCB
        CALL  BDOS

        ; Byte count (lower 8 bits)
        CALL  GETCHR
        LD    B,A
        PUSH  BC
        CALL  GETCHR
        POP   BC
        LD    C,A             ; BC = ASCII Hex Pair
        CALL  BCTOA           ; Convert hex BC to binary in A
        LD    B,A             ; Store in B
        LD    A,(byteCount)   ; The number of bytes we actually read
        CP    B               ; Compare with the number specified (in B)
        JR    Z,byteCountOK

        LD    A,CR
        CALL  PUTCHR
        LD    A,LF
        CALL  PUTCHR

        LD    DE,countErrMess
        LD    C,PSTRING
        CALL  BDOS

        JR    FINISH

byteCountOK:
        ; Next two byts should be the checksum
        CALL  GETCHR
        LD    B,A
        PUSH  BC
        CALL  GETCHR
        POP   BC
        LD    C,A
        CALL  BCTOA
        LD    B,A         ; B = checksum byte
        LD    A,(checkSum)
        CP    B
        JR    Z,checksumOK

        LD    A,CR
        CALL  PUTCHR
        LD    A,LF
        CALL  PUTCHR

        LD    DE,chkErrMess
        LD    C,PSTRING
        CALL  BDOS
        JR    FINISH

checksumOK:
        LD    A,CR
        CALL  PUTCHR
        LD    A,LF
        CALL  PUTCHR

        LD    DE,OKMess
        LD    C,PSTRING
        CALL  BDOS

FINISH: LD    C,SETUSR
        LD    E,0
        CALL  BDOS

        JP    REBOOT


SETUSER:CALL  GETCHR
        CALL  HEX2VAL
        LD    E,A
        LD    C,SETUSR
        CALL  BDOS
        JP    WAITLT


; Wait for a char into A (no echo)
GETCHR: LD    E,$FF
        LD    C,CONIO
        CALL  BDOS
        OR    A
        JR    Z,GETCHR
        CP    03H
        JP    Z,REBOOT   ; Restart CP/M on restart
        CP    0DH        ; Ignore carriage return...
        JR    Z,GETCHR
        CP    0AH        ; ...and line feeds
        JR    Z,GETCHR
        RET

; Write A to output
PUTCHR: PUSH  BC
        PUSH  DE
        LD    C,CONOUT
        LD    E,A
        CALL  BDOS
        POP   DE
        POP   BC
        RET

;------------------------------------------------------------------------------
; Convert ASCII characters in B C registers to a byte value in A
;------------------------------------------------------------------------------
BCTOA   LD    A,B       ; Move the hi order byte to A
        SUB   $30       ; Take it down from Ascii
        CP    $0A       ; Are we in the 0-9 range here?
        JR    C,BCTOA1  ; If so, get the next nybble
        SUB   $07       ; But if A-F, take it down some more
BCTOA1  RLCA            ; Rotate the nibble from low to high
        RLCA            ; One bit at a time
        RLCA            ; Until we
        RLCA            ; Get there with it
        LD    B,A       ; Save the converted high nibble
        LD    A,C       ; Now get the low order byte
        SUB   $30       ; Convert it down from Ascii
        CP    $0A       ; 0-9 at this point?
        JR    C,BCTOA2  ; Good enough then, but
        SUB   $07       ; Take off 7 more if it's A-F
BCTOA2  ADD   A,B       ; Add in the high order nybble
        RET

; Change Hex in A to actual value in A
HEX2VAL SUB   $30
        CP    $0A
        RET   C
        SUB   $07
        RET

REBOOT:      LD    SP,(OSSTACK)
             JP    RESTRT

; DATA AREA
buffPos       .DB     0H
buffPtr       .DW     0000H
printCount    .DB     0H
checkSum      .DB     0H
byteCount     .DB     0H
OKMess        .BYTE  "OK$"
chkErrMess    .BYTE  "======Checksum Error======$"
countErrMess  .BYTE  "======File Length Error======$"
_HEX_CHRS:    .BYTE  "0123456789ABCDEF"

  .END
