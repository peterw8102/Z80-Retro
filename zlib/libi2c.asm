import ../zlib/defs.asm

; i2c driver code.
;
; I2C_INIT:  Initialise the I2C hardware
; I2C_STRT:  Send an I2C START condition
; I2C_STOP:  Send an I2C STOP condition
; I2C_WBY:   Write a byte to the I2C interface
; I2C_RBY:   Read a byte from the I2C interface

          public I2C_INIT, I2C_STRT, I2C_STOP
          public I2C_WBY, I2C_RBY

          extrn  PAUSE

          CSEG

; ------ I2C_DLY
; Short delay between I2C bus edges. PAUSE (~4100 cycles, 0.55 ms at
; 7.37 MHz) was used on every edge, making the bus ~100 times slower than
; needed (boot spent ~1.1 s reading the RTC). With the I2C_CHDATA/I2C_CHCLK
; calls (~110 cycles each) plus this delay (~90 cycles) every clock phase still
; lasts >= 13 us even with the CPU at 14.7 MHz: the DS1307 needs >= 4.7 us low
; and >= 4.0 us high (100 kHz).
I2C_DLY:    PUSH  BC
            LD    B,3
_idly:      DJNZ  _idly
            POP   BC
            RET

; Definitions
I2CDATA   EQU 02h
I2CDEF    EQU 03h
I2CCLK    EQU 04h
I2C_RD    EQU 1
I2C_WR    EQU 0

; ------ I2C_INIT
; Initialise the I2C interface - call before using other functions to set
; the hardware to a known state
I2C_INIT:   PUSH  AF
            LD    A, I2CDEF
            LD    (I2C_VAL),A
            OUT   (I2CPORT_H),A   ; Data HIGH and clock HIGH
            CALL  PAUSE
            POP   AF
            RET


; I2C_STRT
; Send an I2C start condition. Wait for both lines to go high
; Registers changed: A,
I2C_STRT:   ; Bus is IDLE. Send the start condition
            LD    A,1
            CALL  I2C_CHCLK     ; Clock HIGH
            LD    A,1
            CALL  I2C_CHDATA    ; Data HIGH
            CALL  I2C_DLY
            XOR   A
            CALL  I2C_CHDATA    ; Data LOW
            CALL  I2C_DLY
            XOR   A
            CALL  I2C_CHCLK     ; Clock LOW
            CALL  I2C_DLY
            RET

; I2C_STOP
; Send an I2C stop condition.
I2C_STOP:   XOR   A
            CALL  I2C_CHDATA    ; Data LOW
            CALL  I2C_DLY
            LD    A,1
            CALL  I2C_CHCLK     ; Clock HIGH
            CALL  I2C_DLY
            LD    A,1
            CALL  I2C_CHDATA   ; Data HIGH
            CALL  I2C_DLY
            XOR   A
            CALL  I2C_CHCLK     ; Clock HIGH
            CALL  I2C_DLY
            RET

; _WBI
; Write bit 0 of A to the bus. A NOT preserved
_WBI:       CALL  I2C_CHDATA    ; Data to required value
            CALL  I2C_DLY
            LD    A,1
            CALL  I2C_CHCLK     ; Clock HIGH
            CALL  I2C_DLY
            XOR   A
            CALL  I2C_CHCLK     ; Clock LOW
            CALL  I2C_DLY
            RET

; I2C_WBY
; Write a whole byte and return the ACK bit. Byte to write is in the A register. Assumes
; start condition has already been set. Shift out each bit, MSB first.
;
; All registers except A are preserved. On return A contains the ACK bit
I2C_WBY:    PUSH  BC
            LD    B,8
            LD    C,A
_by_nxtw:   RLC   C         ; Bit 7 -> Bit 0
            LD    A,C
            CALL  _WBI
            DJNZ  _by_nxtw

            CALL  _RBI      ; Byte sent so read the ACK bit -> A
            POP   BC
            RET

; _RBI
; Write bit 0 of A to the bus
_RBI:  LD    A,1
            CALL  I2C_CHDATA    ; Data HIGH (inactive so slave can drive)
            CALL  I2C_DLY
            LD    A,1
            CALL  I2C_CHCLK     ; Clock HIGH
            CALL  I2C_DLY
            ; Read the data bit now for device value
            IN    A,(I2CIN)
            AND   I2CDATA       ; (data bit)
            PUSH  AF
            XOR   A
            CALL  I2C_CHCLK     ; Clock LOW
            CALL  I2C_DLY
            POP   AF
            SRA   A
            RET


; I2C_RBY
; Read a byte from the i2c bus. A contains the ACK bit (bit 0). The byte read is returned in A
;
; All registers except A are preserved.
I2C_RBY:    PUSH  BC
            PUSH  AF         ; A contains the ACK bit (continue or end). Save for later
            LD    C,0        ; Build in C
            LD    B,8
_by_nxtr:   LD    A,C
            ADD   A,A        ; Shift A to make room for the next bit
            LD    C,A
            CALL  _RBI  ; Read bit is in A:0
            OR    C          ; Add into the working result
            LD    C,A
            DJNZ  _by_nxtr

            ; C has the build result. Send the ACK bit (saved on the stack)
            POP   AF
            CALL  _WBI

            LD    A,C        ; And return the result in A
            POP   BC
            RET

; I2C_CHDATA
; Set or clear the I2C data bit according to the value in A (zero/not zero). Bit zero only is used
I2C_CHDATA: PUSH  BC
            AND   01h             ; Test bit 0 (Z flag set)
            LD    A,(I2C_CLK)     ; Will be 64h or 65h reflecting clock state
            LD    C,A
            LD    A,(I2C_VAL)     ; Current data. Bit 1 is the i2c output bit
            JR    Z,_i2c_1
            OR    A,I2CDATA       ; Set bit
            JR    _i2c_2
_i2c_1:     AND   ~I2CDATA        ; Clear bit
_i2c_2:     LD    (I2C_VAL),A     ; Save
            OUT   (C),A
            POP   BC
            RET

; I2C_CHCLK
; Set clock high or low according to bit 0 of A. A is NOT preserved.
I2C_CHCLK:  PUSH  BC
            RRCA                  ; Bit 0 into carry flag
            LD    A,I2CPORT
            ADC   A,0             ; A=64h (clock low) or 65h (high)
            LD    (I2C_CLK),A     ; Save current clock state (port number)
            LD    C,A
            LD    A,(I2C_VAL)     ; Set clock state using current data
            OUT   (C),A
            POP   BC
            RET

I2C_VAL:    DB    0FFh
I2C_CLK:    DB    I2CPORT

.END
