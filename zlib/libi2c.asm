import ../zlib/defs.asm

; import libsio.asm

; External dependancies
; RST 08h - Write a single character to the terminal, character in A
; RST 10h - Read one character from the terminal. Block until there is a character. Returned in A
; RST 18h - Check whether there is a character available. Non blocking. Z flag set if there is NOT a character waiting.

          public I2C_INIT, I2C_STRT, I2C_STOP
          public I2C_WBY, I2C_RBY, I2C_WT

          CSEG

; Definitions
I2CPORT   EQU 64h
I2CPORT_H EQU 65h
I2CIN     EQU 64h
I2CDATA   EQU 02h
I2CDEF    EQU 03h
I2CCLK    EQU 04h
I2C_RD    EQU 1
I2C_WR    EQU 0

I2C_INIT:   PUSH  AF
            LD    A, I2CDEF
            LD    (I2C_VAL),A
            OUT   (I2CPORT+1),A   ; Data HIGH and clock HIGH
            CALL  I2C_WT
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
            CALL  I2C_WT
            XOR   A
            CALL  I2C_CHDATA    ; Data LOW
            CALL  I2C_WT
            XOR   A
            CALL  I2C_CHCLK     ; Clock LOW
            CALL  I2C_WT
            RET

; I2C_STOP
; Send an I2C start condition. Wait for both lines to go high
I2C_STOP:   XOR   A
            CALL  I2C_CHDATA    ; Data LOW
            CALL  I2C_WT
            LD    A,1
            CALL  I2C_CHCLK     ; Clock HIGH
            CALL  I2C_WT
            LD    A,1
            CALL  I2C_CHDATA   ; Data HIGH
            CALL  I2C_WT
            XOR   A
            CALL  I2C_CHCLK     ; Clock HIGH
            CALL  I2C_WT
            RET

; _WBI
; Write bit 0 of A to the bus. A NOT preserved
_WBI:       CALL  I2C_CHDATA    ; Data to required value
            CALL  I2C_WT
            LD    A,1
            CALL  I2C_CHCLK     ; Clock HIGH
            CALL  I2C_WT
            XOR   A
            CALL  I2C_CHCLK     ; Clock LOW
            CALL  I2C_WT
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
            CALL  I2C_WT
            LD    A,1
            CALL  I2C_CHCLK     ; Clock HIGH
            CALL  I2C_WT
            ; Read the data bit now for device value
            IN    A,(I2CIN)
            AND   I2CDATA       ; (data bit)
            PUSH  AF
            XOR   A
            CALL  I2C_CHCLK     ; Clock LOW
            CALL  I2C_WT
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
            LD    A,(I2C_CLK)
            LD    C,A
            LD    A,(I2C_VAL)
            JR    Z,_i2c_1
            OR    A,I2CDATA       ; Set bit
            JR    _i2c_2
_i2c_1:     AND   ~I2CDATA        ; Clear bit
_i2c_2:     LD    (I2C_VAL),A     ; Save
            OUT   (C),A
            POP   BC
            RET

; I2C_CHCLK
; Set or clear the I2C data bit according to the value in A (zero/not zero). Bit zero only is used
I2C_CHCLK:  PUSH  BC
            RRCA                  ; Test bit 0 (Z flag set)
            LD    A,I2CPORT
            ADC   A,0
            LD    (I2C_CLK),A
            LD    C,A
            LD    A,(I2C_VAL)
            OUT   (C),A
            POP   BC
            RET

I2C_VAL:    DB    0FFh
I2C_CLK:    DB    I2CPORT

I2C_WT:     PUSH  HL
            LD    H,001H
del_1:      LD    L,0FFH
del_2:      DEC   L
            JR    NZ,del_2
            DEC   H
            JR    NZ,del_1
            POP   HL
            RET
.END
