import zlib.asm

; Where to put the supervisor stack/
_STK     EQU   $3FFE


; The number of digits in the MAX2719 daisy chain. Generally 4 for
; a single 'block' and 8 for two blocks. The maximum number of displays
; supported is 31 - so in practice a maximum of 7 of the four
; character display blocks.
NUMDIGITS   EQU   8

; Number of bytes to hold all the LED states for NUMDIGITS plus
; an additional 8 bytes for the character currently being
; scrolled into view. This can't be more than 256 bytes, hence
; the max valud of NUMDIGITS of 31.
DATASIZE    EQU   (NUMDIGITS+1)*8

; The opcodes recognised by the MAX7219. These are sent as the first byte in each 16 bit transaction.
; Codes 1-8 are the row data for that device.
M_NOP       EQU   0x00
M_DMODE     EQU   0x09
M_INT       EQU   0x0A
M_SCAN      EQU   0x0B
M_SDWN      EQU   0x0C
M_TEST      EQU   0x0F

; -------------------------------------------
; ----------------- CODE --------------------
; -------------------------------------------
            ASEG
            ORG   $0
            JR    START

            CSEG

;-------------------------------------------------------------
; START - Main entry point.
START:      LD    SP,_STK     ; Set a usable stack

            CALL  INITDEV     ; Initialise each of the displays
            CALL  INITDATA    ; and clear th data area.

_again:     LD    HL,STRMSG   ; The message we want to scroll
            CALL  SCRMSG
            JR    _again      ; And keep sending the same message...


;-------------------------------------------------------------
; SCRMSH
; Scroll the null terminated string acroos the display one
; pixel at a time. Return once the null terminator has
; been reached.
;    HL: String to be displayed, not preserved
SCRMSG:     LD    A,(HL)        ; The next character from STRMSG
            OR    A             ; String is null terminated
            RET   Z
            INC   HL            ; Ready for the next character
            LD    B,NUMDIGITS   ; Character index into LEDDATA
            CALL  PCHR          ; Put the character...
            CALL  SCRLFT        ; Scroll the whole of that character into the display
            JR    SCRMSG

;-------------------------------------------------------------
; SCRLFT - Scroll the character in the staging area into the right
; most display. All other digits scroll left. This involves 8
; 'scrolls' one per vertical pixel.
SCRLFT:     PUSH  BC
            LD    B,8           ; Number of columns/pixels in a character

_scnxt:     CALL  SCRCOL        ; Scroll
            CALL  REFRESH       ; Display what's there
            CALL  DELAY         ; And pause....

            DJNZ  _scnxt
            POP   BC
            RET

;-------------------------------------------------------------
; DELAY - Slow things down a bit so the human eye can
; actuall see what's going on...
DELAY:      PUSH  BC
            LD    B,80h
l0:         LD    C,0
l1:         DEC   C
            JR    NZ,l1
            DJNZ  l0
            POP   BC
            RET

;-------------------------------------------------------------
; MAX2719
; Macro to send a 16 bit value to one of the registers in the
; MAX2719. The same data is sent to all of the devices in the
; chain via the SENDALL subroutine.
MAX2719     MACRO reg,data
            LD    DE,reg << 8 | data
            CALL  SENDALL
            ENDM

;-------------------------------------------------------------
; INITDEV - Initialise each MAX7219 in the chain. No parameters.
; Call this before sending any characters to the device. Registers
; not preserved. A, B and DE will be corrupted.
INITDEV:    LD    A,2        ; Select the correct SPI device
            CALL  SPI_SEL

            MAX2719 M_TEST,0        ; Operational mode
            MAX2719 M_DMODE,0       ; Decode mode
            MAX2719 M_INT,0         ; Intensity
            MAX2719 M_SCAN,0x0f     ; San frequency
            MAX2719 M_SDWN,0x01     ; SHUTDOWN

            RET

;-------------------------------------------------------------
; SENDALL - Send the 16 bit value in DE to ALL displays. All
; this really involves is sending the same 16 bits out of the
; SPI serial port NUMDIGIT times then removing the chip select
; so all devices see the same command.
;   DE: 16 bit value to send
;   A and B not preserved.
SENDALL:    CALL  SPI_STRT
            LD    B,NUMDIGITS
_nxt1:      LD    A,D
            CALL  SPI_TXB
            LD    A,E
            CALL  SPI_TXB
            DJNZ  _nxt1
            CALL  SPI_FIN
            RET

;-------------------------------------------------------------
; INITDATA - Initialise the data area to all zeroes.
; Registers A, B and HL will be corrupted.
; not preserved.
INITDATA:   LD    HL,LEDDATA
            LD    B,DATASIZE
            XOR   A
_clrma:     LD    (HL),A
            INC   HL
            DJNZ  _clrma
            RET

;-------------------------------------------------------------
; REFRESH - Write data to ALL LEDs in the matrix from the LED
; data area. Involves writing out all 8 rows one at a time for
; all of the devices (eg NUMDIGITS).
; 'A' not preserved.
REFRESH:    PUSH  HL
            PUSH  DE
            PUSH  BC
            LD    HL,LEDDATA
            LD    D,1         ; Next row number to write (MAX2719 indexes based at 1, not zero)
            LD    B,8         ; Number of rows to write
_nrow:      LD    C,NUMDIGITS ; Number of bytes in this row
            CALL  SPI_STRT    ; Tell the devices to start listening
_ndev:      LD    A,D         ; The row number
            CALL  SPI_TXB
            LD    A,(HL)      ; Pixel data
            CALL  SPI_TXB
            INC   HL
            DEC   C
            JR    NZ,_ndev
            CALL  SPI_FIN
            INC   D           ; Next row
            INC   HL          ; Skip the staging byte
            DJNZ  _nrow
            POP   BC
            POP   DE
            POP   HL
            RET

;-------------------------------------------------------------
; SCRCOL - Scroll the LED data one column to the left. This
; involves shifting the bits left in every byte in the LEDDATA
; area.
SCRCOL:     PUSH  HL
            PUSH  BC
            LD    HL,LEDDATA+DATASIZE-1
            LD    B,DATASIZE
            OR    A           ; Clear carry
_nc:        RL    (HL)        ; MSB Is now in the carry ready to move to next byte
            DEC   HL          ; Next byte
            DJNZ  _nc
            POP   BC
            POP   HL
            RET

;-------------------------------------------------------------
; PCHR - Write the char code in A to character position in B.
; B will be a value between 0 and NUMDIGITS, where NUMDIGITS
; (the last index) is the hidden position used as a staging
; area for the next character to be scrolled onto the display.
PCHR::      PUSH  HL
            PUSH  DE
            PUSH  BC

            ; Find the font data for the character in C
            LD    L,A
            LD    H,0
            ADD   HL,HL
            ADD   HL,HL
            ADD   HL,HL       ; HL x 8
            LD    DE,FONT     ; Add to the FONT base address
            ADD   HL,DE       ; HL points to start of character to render. Decide where it's going
            PUSH  HL          ; Save for later

            ; Work out where the font data needs to go in the LEDDATA area
            LD    E,B         ; Character position (0-NUMDIGITS)
            LD    D,0
            LD    HL,LEDDATA  ; Start of the data area
            ADD   HL,DE       ; HL points to the first row of pixels

            POP   DE          ; Start of character font data => DE
            LD    B,8         ; Number of bytes to copy

.nxtscn:    LD    A,(DE)
            LD    (HL),A
            INC   DE           ; Next scan row

            ; The next row for this character will be
            ; NUMDIGITS+1 past the current location. The data
            ; is arranged as the whole of one row together.
            REPT  NUMDIGITS+1
            INC   HL
            ENDM

            DJNZ  .nxtscn
            POP   BC
            POP   DE
            POP   HL
            RET


;-------------------------------------------------------------
; LEDDATA - Data contains all the pixel data for the whole
; array. This is large enough to hold NUMDIGITS to display
; and one extra digit as a staging area to hold the character
; about to be scrolled into the right most display digit.
LEDDATA:   DS    DATASIZE

STRMSG:    DEFB  "Z80 Retro Computers Are REALLY Cool  -+*+- ",0

; The font data is a set of 8 bytes to define each
; character. The first byte is the top row of pixels.
; I extracted this data from:
;    https://github.com/dhepper/font8x8/blob/master/font8x8_basic.h
; converted it to assembler data then 'flipped' all the bits in
; each byte to give me the data I want for the display.
FONT: DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00   ; Char 0x00
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00   ; Char 0x08
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00   ; Char 0x10
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00   ; Char 0x18
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00   ; Char 0x20 - Space
      DEFB 0x18, 0x3c, 0x3c, 0x18, 0x18, 0x00, 0x18, 0x00   ; !
      DEFB 0x6c, 0x6c, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00   ; "
      DEFB 0x6c, 0x6c, 0xfe, 0x6c, 0xfe, 0x6c, 0x6c, 0x00   ; #
      DEFB 0x30, 0x7c, 0xc0, 0x78, 0x0c, 0xf8, 0x30, 0x00   ; $
      DEFB 0x00, 0xc6, 0xcc, 0x18, 0x30, 0x66, 0xc6, 0x00   ; %
      DEFB 0x38, 0x6c, 0x38, 0x76, 0xdc, 0xcc, 0x76, 0x00   ; &
      DEFB 0x60, 0x60, 0xc0, 0x00, 0x00, 0x00, 0x00, 0x00   ; '
      DEFB 0x18, 0x30, 0x60, 0x60, 0x60, 0x30, 0x18, 0x00   ; (
      DEFB 0x60, 0x30, 0x18, 0x18, 0x18, 0x30, 0x60, 0x00   ; )
      DEFB 0x00, 0x66, 0x3c, 0xff, 0x3c, 0x66, 0x00, 0x00   ; *
      DEFB 0x00, 0x30, 0x30, 0xfc, 0x30, 0x30, 0x00, 0x00   ; +
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x30, 0x30, 0x60   ; ,
      DEFB 0x00, 0x00, 0x00, 0xfc, 0x00, 0x00, 0x00, 0x00   ; -
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x30, 0x30, 0x00   ; .
      DEFB 0x06, 0x0c, 0x18, 0x30, 0x60, 0xc0, 0x80, 0x00   ; /
      DEFB 0x7c, 0xc6, 0xce, 0xde, 0xf6, 0xe6, 0x7c, 0x00   ; 0
      DEFB 0x30, 0x70, 0x30, 0x30, 0x30, 0x30, 0xfc, 0x00
      DEFB 0x78, 0xcc, 0x0c, 0x38, 0x60, 0xcc, 0xfc, 0x00
      DEFB 0x78, 0xcc, 0x0c, 0x38, 0x0c, 0xcc, 0x78, 0x00
      DEFB 0x1c, 0x3c, 0x6c, 0xcc, 0xfe, 0x0c, 0x1e, 0x00
      DEFB 0xfc, 0xc0, 0xf8, 0x0c, 0x0c, 0xcc, 0x78, 0x00
      DEFB 0x38, 0x60, 0xc0, 0xf8, 0xcc, 0xcc, 0x78, 0x00
      DEFB 0xfc, 0xcc, 0x0c, 0x18, 0x30, 0x30, 0x30, 0x00
      DEFB 0x78, 0xcc, 0xcc, 0x78, 0xcc, 0xcc, 0x78, 0x00
      DEFB 0x78, 0xcc, 0xcc, 0x7c, 0x0c, 0x18, 0x70, 0x00   ; 9
      DEFB 0x00, 0x30, 0x30, 0x00, 0x00, 0x30, 0x30, 0x00   ; :
      DEFB 0x00, 0x30, 0x30, 0x00, 0x00, 0x30, 0x30, 0x60   ; ;
      DEFB 0x18, 0x30, 0x60, 0xc0, 0x60, 0x30, 0x18, 0x00   ; <
      DEFB 0x00, 0x00, 0xfc, 0x00, 0x00, 0xfc, 0x00, 0x00   ; =
      DEFB 0x60, 0x30, 0x18, 0x0c, 0x18, 0x30, 0x60, 0x00   ; >
      DEFB 0x78, 0xcc, 0x0c, 0x18, 0x30, 0x00, 0x30, 0x00   ; ?
      DEFB 0x7c, 0xc6, 0xde, 0xde, 0xde, 0xc0, 0x78, 0x00   ; @
      DEFB 0x30, 0x78, 0xcc, 0xcc, 0xfc, 0xcc, 0xcc, 0x00   ; A
      DEFB 0xfc, 0x66, 0x66, 0x7c, 0x66, 0x66, 0xfc, 0x00
      DEFB 0x3c, 0x66, 0xc0, 0xc0, 0xc0, 0x66, 0x3c, 0x00
      DEFB 0xf8, 0x6c, 0x66, 0x66, 0x66, 0x6c, 0xf8, 0x00
      DEFB 0xfe, 0x62, 0x68, 0x78, 0x68, 0x62, 0xfe, 0x00
      DEFB 0xfe, 0x62, 0x68, 0x78, 0x68, 0x60, 0xf0, 0x00
      DEFB 0x3c, 0x66, 0xc0, 0xc0, 0xce, 0x66, 0x3e, 0x00
      DEFB 0xcc, 0xcc, 0xcc, 0xfc, 0xcc, 0xcc, 0xcc, 0x00
      DEFB 0x78, 0x30, 0x30, 0x30, 0x30, 0x30, 0x78, 0x00
      DEFB 0x1e, 0x0c, 0x0c, 0x0c, 0xcc, 0xcc, 0x78, 0x00
      DEFB 0xe6, 0x66, 0x6c, 0x78, 0x6c, 0x66, 0xe6, 0x00
      DEFB 0xf0, 0x60, 0x60, 0x60, 0x62, 0x66, 0xfe, 0x00
      DEFB 0xc6, 0xee, 0xfe, 0xfe, 0xd6, 0xc6, 0xc6, 0x00
      DEFB 0xc6, 0xe6, 0xf6, 0xde, 0xce, 0xc6, 0xc6, 0x00
      DEFB 0x38, 0x6c, 0xc6, 0xc6, 0xc6, 0x6c, 0x38, 0x00
      DEFB 0xfc, 0x66, 0x66, 0x7c, 0x60, 0x60, 0xf0, 0x00
      DEFB 0x78, 0xcc, 0xcc, 0xcc, 0xdc, 0x78, 0x1c, 0x00
      DEFB 0xfc, 0x66, 0x66, 0x7c, 0x6c, 0x66, 0xe6, 0x00
      DEFB 0x78, 0xcc, 0xe0, 0x70, 0x1c, 0xcc, 0x78, 0x00
      DEFB 0xfc, 0xb4, 0x30, 0x30, 0x30, 0x30, 0x78, 0x00
      DEFB 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0xfc, 0x00
      DEFB 0xcc, 0xcc, 0xcc, 0xcc, 0xcc, 0x78, 0x30, 0x00
      DEFB 0xc6, 0xc6, 0xc6, 0xd6, 0xfe, 0xee, 0xc6, 0x00
      DEFB 0xc6, 0xc6, 0x6c, 0x38, 0x38, 0x6c, 0xc6, 0x00
      DEFB 0xcc, 0xcc, 0xcc, 0x78, 0x30, 0x30, 0x78, 0x00
      DEFB 0xfe, 0xc6, 0x8c, 0x18, 0x32, 0x66, 0xfe, 0x00   ; Z
      DEFB 0x78, 0x60, 0x60, 0x60, 0x60, 0x60, 0x78, 0x00   ; [
      DEFB 0xc0, 0x60, 0x30, 0x18, 0x0c, 0x06, 0x02, 0x00   ; \
      DEFB 0x78, 0x18, 0x18, 0x18, 0x18, 0x18, 0x78, 0x00   ; ]
      DEFB 0x10, 0x38, 0x6c, 0xc6, 0x00, 0x00, 0x00, 0x00   ; ^
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff   ; _
      DEFB 0x30, 0x30, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00   ; ``
      DEFB 0x00, 0x00, 0x78, 0x0c, 0x7c, 0xcc, 0x76, 0x00   ; a
      DEFB 0xe0, 0x60, 0x60, 0x7c, 0x66, 0x66, 0xdc, 0x00
      DEFB 0x00, 0x00, 0x78, 0xcc, 0xc0, 0xcc, 0x78, 0x00
      DEFB 0x1c, 0x0c, 0x0c, 0x7c, 0xcc, 0xcc, 0x76, 0x00
      DEFB 0x00, 0x00, 0x78, 0xcc, 0xfc, 0xc0, 0x78, 0x00
      DEFB 0x38, 0x6c, 0x60, 0xf0, 0x60, 0x60, 0xf0, 0x00
      DEFB 0x00, 0x00, 0x76, 0xcc, 0xcc, 0x7c, 0x0c, 0xf8
      DEFB 0xe0, 0x60, 0x6c, 0x76, 0x66, 0x66, 0xe6, 0x00
      DEFB 0x30, 0x00, 0x70, 0x30, 0x30, 0x30, 0x78, 0x00
      DEFB 0x0c, 0x00, 0x0c, 0x0c, 0x0c, 0xcc, 0xcc, 0x78
      DEFB 0xe0, 0x60, 0x66, 0x6c, 0x78, 0x6c, 0xe6, 0x00
      DEFB 0x70, 0x30, 0x30, 0x30, 0x30, 0x30, 0x78, 0x00
      DEFB 0x00, 0x00, 0xcc, 0xfe, 0xfe, 0xd6, 0xc6, 0x00
      DEFB 0x00, 0x00, 0xf8, 0xcc, 0xcc, 0xcc, 0xcc, 0x00
      DEFB 0x00, 0x00, 0x78, 0xcc, 0xcc, 0xcc, 0x78, 0x00
      DEFB 0x00, 0x00, 0xdc, 0x66, 0x66, 0x7c, 0x60, 0xf0
      DEFB 0x00, 0x00, 0x76, 0xcc, 0xcc, 0x7c, 0x0c, 0x1e
      DEFB 0x00, 0x00, 0xdc, 0x76, 0x66, 0x60, 0xf0, 0x00
      DEFB 0x00, 0x00, 0x7c, 0xc0, 0x78, 0x0c, 0xf8, 0x00
      DEFB 0x10, 0x30, 0x7c, 0x30, 0x30, 0x34, 0x18, 0x00
      DEFB 0x00, 0x00, 0xcc, 0xcc, 0xcc, 0xcc, 0x76, 0x00
      DEFB 0x00, 0x00, 0xcc, 0xcc, 0xcc, 0x78, 0x30, 0x00
      DEFB 0x00, 0x00, 0xc6, 0xd6, 0xfe, 0xfe, 0x6c, 0x00
      DEFB 0x00, 0x00, 0xc6, 0x6c, 0x38, 0x6c, 0xc6, 0x00
      DEFB 0x00, 0x00, 0xcc, 0xcc, 0xcc, 0x7c, 0x0c, 0xf8
      DEFB 0x00, 0x00, 0xfc, 0x98, 0x30, 0x64, 0xfc, 0x00   ; z
      DEFB 0x1c, 0x30, 0x30, 0xe0, 0x30, 0x30, 0x1c, 0x00   ; {
      DEFB 0x18, 0x18, 0x18, 0x00, 0x18, 0x18, 0x18, 0x00   ; |
      DEFB 0xe0, 0x30, 0x30, 0x1c, 0x30, 0x30, 0xe0, 0x00   ; }
      DEFB 0x76, 0xdc, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00   ; ~
      DEFB 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00   ; DEL
