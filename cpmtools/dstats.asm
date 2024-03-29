;==================================================================================
; Display/clear current disk stats.
;
; Syntax: DSTATS [CLEAR]
;
; If 'CLEAR' is specified then the counters will be displayed and then cleared to
; zero after display.
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

        ; Clear our copy of the stats buffer
        LD    HL,statbuf
        XOR   A
        LD    B,Z_BUFSZ
.clr:   LD    (HL),A
        INC   HL
        DJNZ  .clr

        ; Get a copy of the command line as a null terminated string
        LD     DE,cmdline
        LD     (buffPtr),DE
        LD     HL,BUFF+1
        LD     A,(BUFF)      ; Number of bytes
        LD     (buffCnt),A
        OR     A
        JR     Z,.empty
        LD     B,A
.copy:  LD     A,(HL)
        LD     (DE),A
        INC    DE
        INC    HL
        DJNZ   .copy

        ; Add the null terminator
.empty: XOR    A
        LD     (DE),A

        CALL   WASTSPC

        ; Check the command line. If CLEAR is specified then the stats
        ; will be cleared after read.
        LD   DE,m_intro
        LD   C,PSTRING
        CALL BDOS

        LD   DE,clear
        LD   HL,(buffPtr)
        LD   DE,clear
        CALL STRCMP
        LD   A,0
        JR   NZ,.noclr

        LD   DE,m_clr
        LD   C,PSTRING
        CALL BDOS
        LD   A,1

.noclr: LD   B,A          ; If not zero then clear stats
        LD   HL,statbuf   ; Where we want stats from ZIOS
        LD   C,A_CPSTS
        RST  30h

        ; Display the stats (32 bit counters)
        LD    DE,m_reads
        LD    C,PSTRING
        CALL  BDOS
        LD    DE,(statbuf+2)
        LD    HL,(statbuf)
        CALL  PRTDECK

        LD    DE,m_writes
        LD    C,PSTRING
        CALL  BDOS
        LD    DE,(statbuf+6)
        LD    HL,(statbuf+4)
        CALL  PRTDECK

        LD    DE,m_cln
        LD    C,PSTRING
        CALL  BDOS
        LD    DE,(statbuf+14)
        LD    HL,(statbuf+12)
        CALL  PRTDECK

        LD    DE,m_cache
        LD    C,PSTRING
        CALL  BDOS
        LD    DE,(statbuf+10)
        LD    HL,(statbuf+8)
        CALL  PRTDEC

        JP    REBOOT

SKPSPC:  CALL  GETCHR
          CP    20h
          JR    Z,SKPSPC
          RET

WASTSPC:  LD    A,(buffCnt)
          OR    A
          RET   Z
          LD    HL,(buffPtr)
          LD    A,(HL)
          CP    ' '
          RET   NZ
          ; Not a space so step on
          INC   HL
          LD    (buffPtr),HL
          LD    A,(buffCnt)
          DEC   A
          LD    (buffCnt),A
          JR    WASTSPC


; Get the next character from the input buffer
GETCHR: PUSH  HL
        LD    HL,(buffPtr)
        LD    A,(HL)
        OR    A
        JR    Z,.empty        ; End of line
        INC   HL
        LD    (buffPtr),HL
.empty: POP   HL
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
NL:     PUSH  AF
        LD    A,10
        CALL  PUTCHR
        LD    A,13
        CALL  PUTCHR
        POP   AF
        RET


; ---- STRCMP
; Compare the string in HL with the test string in DE. This is a limited test as follows:
;  + Both strings are zero terminated.
;  + The strings are considered to be the same if HL points to a string at least as long as DE
;    AND that all characters pointed to by HL match those pointed to by DE
;
; Comparison is case insensitive.
;
; HL: String to test
; DE: Required test string
; A:      Zero if both strings match, Non-zero if the strings do NOT match
; Z flag: Matches state of 'A'
STRCMP:     LD    A,(DE)
            INC   DE
            OR    A
            RET   Z         ; At the end of DE and no failures
            LD    B,A       ; Save character
            LD    A,(HL)
            CALL  TOUPPER
            CP    B         ; Match the test string?
            RET   NZ        ; String pointed to doesn't match
            LD    A,(HL)
            OR    A         ; End of HL string?
            JR    Z,_sfail
            INC   HL
            JR    STRCMP

_sfail:     XOR   A
            INC   A
            RET

; ------ TOUPPER
; Take the character in A. If it's a lower case letter then promote to uppercase.
TOUPPER:  CP    'a'                ; Lower case -> upper case
          RET    C
          CP    'z'+1
          RET   NC
          ADD   A,'A'-'a'
          RET


; Combined routine for conversion of different sized binary numbers into
; directly printable ASCII(Z)-string
; Input value in registers, number size and -related to that- registers to fill
; is selected by calling the correct entry:
;
;  entry  inputregister(s)  decimal value 0 to:
;   B2D8             A                    255  (3 digits)
;   B2D16           HL                  65535   5   "
;   B2D24         E:HL               16777215   8   "
;   B2D32        DE:HL             4294967295  10   "
;   B2D48     BC:DE:HL        281474976710655  15   "
;   B2D64  IX:BC:DE:HL   18446744073709551615  20   "
;
; The resulting string is placed into a small buffer attached to this routine,
; this buffer needs no initialization and can be modified as desired.
; The number is aligned to the right, and leading 0's are replaced with spaces.
; On exit HL points to the first digit, (B)C = number of decimals
; This way any re-alignment / postprocessing is made easy.
; Changes: AF,BC,DE,HL,IX
; P.S. some examples below

; by Alwin Henseler
B2D8:    LD H,0
         LD L,A
B2D16:   LD E,0
B2D24:   LD D,0
B2D32:   LD BC,0
B2D48:   LD IX,0          ; zero all non-used bits
B2D64:   LD (B2DINV),HL
         LD (B2DINV+2),DE
         LD (B2DINV+4),BC
         LD (B2DINV+6),IX ; place full 64-bit input value in buffer
         LD HL,B2DBUF
         LD DE,B2DBUF+1
         LD (HL)," "
B2DFILC: EQU $-1         ; address of fill-character
         LD BC,18
         LDIR            ; fill 1st 19 bytes of buffer with spaces
         LD (B2DEND-1),BC ;set BCD value to "0" & place terminating 0
         LD E,1          ; no. of bytes in BCD value
         LD HL,B2DINV+8  ; (address MSB input)+1
         LD BC,$0909
         XOR A
B2DSKP0: DEC B
         JR Z,B2DSIZ     ; all 0: continue with postprocessing
         DEC HL
         OR (HL)         ; find first byte <>0
         JR Z,B2DSKP0
B2DFND1: DEC C
         RLA
         JR NC,B2DFND1   ; determine no. of most significant 1-bit
         RRA
         LD D,A          ; byte from binary input value
B2DLUS2: PUSH HL
         PUSH BC
B2DLUS1: LD HL,B2DEND-1  ; address LSB of BCD value
         LD B,E          ; current length of BCD value in bytes
         RL D            ; highest bit from input value -> carry
B2DLUS0: LD A,(HL)
         ADC A,A
         DAA
         LD (HL),A       ; double 1 BCD byte from intermediate result
         DEC HL
         DJNZ B2DLUS0    ; and go on to double entire BCD value (+carry!)
         JR NC,B2DNXT
         INC E           ; carry at MSB -> BCD value grew 1 byte larger
         LD (HL),1       ; initialize new MSB of BCD value
B2DNXT:  DEC C
         JR NZ,B2DLUS1   ; repeat for remaining bits from 1 input byte
         POP BC          ; no. of remaining bytes in input value
         LD C,8          ; reset bit-counter
         POP HL          ; pointer to byte from input value
         DEC HL
         LD D,(HL)       ; get next group of 8 bits
         DJNZ B2DLUS2    ; and repeat until last byte from input value
B2DSIZ:  LD HL,B2DEND    ; address of terminating 0
         LD C,E          ; size of BCD value in bytes
         OR A
         SBC HL,BC       ; calculate address of MSB BCD
         LD D,H
         LD E,L
         SBC HL,BC
         EX DE,HL        ; HL=address BCD value, DE=start of decimal value
         LD B,C          ; no. of bytes BCD
         SLA C           ; no. of bytes decimal (possibly 1 too high)
         LD A,"0"
         RLD             ; shift bits 4-7 of (HL) into bit 0-3 of A
         CP "0"          ; (HL) was > 9h?
         JR NZ,B2DEXPH   ; if yes, start with recording high digit
         DEC C           ; correct number of decimals
         INC DE          ; correct start address
         JR B2DEXPL      ; continue with converting low digit
B2DEXP:  RLD             ; shift high digit (HL) into low digit of A
B2DEXPH: LD (DE),A       ; record resulting ASCII-code
         INC DE
B2DEXPL: RLD
         LD (DE),A
         INC DE
         INC HL          ; next BCD-byte
         DJNZ B2DEXP     ; and go on to convert each BCD-byte into 2 ASCII
         SBC HL,BC       ; return with HL pointing to 1st decimal
         RET

PRTDEC:  CALL  B2D32
         LD    A,'$'
         LD    (B2DEND),A
         LD    DE,B2DBUF
         LD    C,PSTRING
         CALL  BDOS
         RET

PRTDECK: PUSH  DE
         PUSH  HL
         CALL  B2D32
         LD    A,'$'
         LD    (B2DEND),A
         LD    DE,B2DBUF
         LD    C,PSTRING
         CALL  BDOS
         LD    DE,m_open
         LD    C,PSTRING
         CALL  BDOS

        ; Divide number by 2 to get number of KB
         POP   HL
         POP   DE
         OR    A      ; Clear carry

         ; Divide by 2
         RR    D
         RR    E
         RR    H
         RR    L

         ; Print KB count
         CALL  B2D32
         LD    A,'$'
         LD    (B2DEND),A

         ; Skip leading spaces for the KB count
         LD    HL,B2DBUF
.skpspc: LD    A,(HL)
         INC   HL
         CP    ' '
         JR    Z,.skpspc
         DEC   HL
         EX    DE,HL

         ; LD    DE,B2DBUF
         LD    C,PSTRING
         CALL  BDOS



         LD    DE,m_close
         LD    C,PSTRING
         CALL  BDOS

         RET

REBOOT:      LD    SP,(OSSTACK)
             ; JP    REBOOT
             JP    RESTRT


B2DINV:  DC 8,0          ; space for 64-bit input value (LSB first)
B2DBUF:  DC 20,0         ; space for 20 decimal digits
B2DEND:  DB '$'          ; space for terminating 0


clear         .BYTE  "CLEAR",0
m_intro       .BYTE  "SDCard Statistics",10,13,"$"
m_reads       .BYTE  "Blocks read:    $"
m_writes      .BYTE  "Blocks written: $"
m_cache       .BYTE  "Cache hits:     $"
m_cln         .BYTE  "Clean writes:   $"
m_clr         .BYTE  "Clearing stats",10,13,"$"
m_open        .BYTE  " ( $"
m_close       .BYTE  "K )",10,13,"$"

DRV_LET       .DB     00H
DSK_NO        .DW     0000H

; DATA AREA
buffCnt::     .DB     00H
buffPtr::     .DW     0000H

cmdline::      DS    128            ; Null terminated copy of the command line
statbuf        DS    Z_BUFSZ        ; This is where the stats will go
  .END
