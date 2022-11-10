import ../zlib/defs.asm


; Real Time Clock (RTC) driver
; The RTC chip (DS1307+) is connected to the I2C bus. As wells as providing a clock the chip
; also contains 56 bytes of non-volatile memory.
;
; DS1307+ Datasheet: https://www.mouser.co.uk/datasheet/2/256/DS1307-1513036.pdf
;
; Exports:
; RTC_INI:  Initialise the RTC interface. Call before first use. Currently just initialises I2C
; RTC_SOS:  Start the RTC oscilator at 1KHz
; RTC_GET:  Return the 7 byte encoded date/time from the DS1307+ chip
; RTC_SET:  Write the 7 byte date/time format to the DS1307+. See datasheet for format details
; RTC_MRD:  Read some/all of the NVRAM bytes from the DS1307+, skipping the clock bytes
; RTC_MWR:  Write to the NVRAM with protection from accidentally overwritting clock control byte
; RTC_CL:   Clear all 56 NVRAM bytes

          extrn  I2C_INIT, I2C_STRT, I2C_STOP
          extrn  I2C_WBY, I2C_RBY

          public RTC_INI, RTC_SOS, RTC_GET, RTC_SET, RTC_MWR, RTC_MRD, RTC_CL

          CSEG

; Definitions
RTC_ID    EQU 11010000b ; DS1307+ RTC address = 0xB0
I2C_RD    EQU 1
I2C_WR    EQU 0

_CFG      EQU 10010000b        ; Output enabled, 1Hz clock

; -- RTC_MWR
; Write a block of data to RTC memory. This protects the number of bytes that can be written to 56
; and prevents an overwrite of the clock data.
;    HL: Pointer to data to write
;    B:  Number of bytes to write
;    C:  Offset (byte) from user RAM area. 0 = first writable byte
; At end
;    BC not saved
;    HL points to first byte after the write block.
RTC_MWR:  LD       A,56
          SUB      C
          RET      C       ; Offset > 56 so can't write
          CP       B       ; A contains max number of bytes. B is requested number of bytes.
          JR       NC,_ok1 ; Requested number of bytes is < max
          LD       B,A     ; Limit to max available bytes
_ok1:     LD       A,8     ; Skip clock control bytes
          ADD      C       ; Where we want to be
          CALL     setrdpt ; Set pointer and start write operation
_wb:      LD       A,(HL)  ; Next byte to write
          INC      HL
          CALL     I2C_WBY
          DJNZ     _wb
          CALL     I2C_STOP
          RET

; -- RTC_MRD
; Read a block of data from RTC memory. This protects the number of bytes that can be written to 56
; and prevents an overwrite of the clock data.
;    HL: Pointer to buffer to receive data
;    B:  Number of bytes to read
;    C:  Offset (byte) from user RAM area. 0 = first writable byte
; At end
;    BC not saved
;    HL points to first byte after the write block.
RTC_MRD:  LD       A,56
          SUB      C
          RET      C                 ; Offset > 56 so nothing to read
          CP       B                 ; A contains max number of bytes. B is requested number of bytes.
          JR       NC,_ok2           ; Requested number of bytes is < max
          LD       B,A               ; Limit to max available bytes
_ok2:     LD       A,8               ; Skip clock control bytes
          ADD      C                 ; Where we want to be
          LD       C,A               ; which is 8 bytes further on.
          ; AND CONTIUE INTO _INTRD...

; Ready to read. Called to execute a read command to the RTC chip.
; Inputs: B:  Number of bytes of data to read
;         C:  The address in the RTC 64 byte address area from which to read
;         HL: Address to which to write the data from the chip
_INTRD:   LD       A,C               ; Set the read address
          CALL     setrdpt

          CALL     I2C_STRT          ; Start read operation
          LD       A,RTC_ID | I2C_RD
          CALL     I2C_WBY

_rb:      LD       A,B               ; Number of bytes left
          DEC      A                 ; Is it 1? If so this is the last byte
          JR       NZ, _nl
          INC      A                 ; Last byte so set the ack bit
          JR       _rb1
_nl       XOR      A                 ; Not the last byte so ack bit is zero
_rb1:     CALL     I2C_RBY
          LD       (HL),A            ; Write to output buffer
          INC      HL
          DJNZ     _rb
          CALL     I2C_STOP
          RET

; -- RTC_SET
; Set the time.
; HL: Points to a 'time' configuration structure which is 7 bytes.
RTC_SET:  PUSH     BC
          XOR      A
          CALL     setrdpt
          LD       B,7                ; Write out 7 bytes from (HL)
_nextb:   LD       A,(HL)             ; write the data
          CALL     I2C_WBY
          INC      HL
          DJNZ     _nextb
          LD       A,_CFG             ; Write a fixed 8th byte that turns on the clock
          CALL     I2C_WBY
          CALL     I2C_STOP
          POP      BC
          RET

; -- RTC_GET
; Get the time. This is returned as 7 buytes written to the buffer pointed at by HL.
; The format of the time is as encoded by the DS1307+ chip, check the datasheet
; referenced at the start of this file for details.
;
; HL: Points to a buffer to receive the 7 byte time structure from the RTC
RTC_GET:  PUSH     BC
          LD       BC,0700h           ; Block read 7 bytes (B) from address zero (C)
          CALL     _INTRD
          POP      BC
          RET


; -- RTC_INI
; Call once at start of use. Nothing to do at the moment.
RTC_INI:  JR       I2C_INIT

; -- RTC_CL
; Clear all user memory to zero. Doesn't affect time. No parameters.
; All registers except AF are preserved.
RTC_CL:   PUSH     BC
          LD       A,8
          CALL     setrdpt

          ; And write out 56 bytes of 55h
          LD       B,56

_clrnxt:  XOR      A
          CALL     I2C_WBY
          DJNZ     _clrnxt
          CALL     I2C_STOP
          POP      BC
          RET

; RTC_SOS
; Start oscillator - at 1Hz
RTC_SOS:  CALL     I2C_STRT
          LD       A,RTC_ID | I2C_WR
          CALL     I2C_WBY

          ; Write the byte we want to start from (0)
          XOR      A
          CALL     I2C_WBY

          ; And the data value
          XOR      A
          CALL     I2C_WBY
          CALL     I2C_STOP
          RET

; setrdpt
; Set the register address to the value in A
setrdpt:  PUSH     AF
          CALL     I2C_STRT
          LD       A,RTC_ID | I2C_WR
          CALL     I2C_WBY
          POP      AF
          CALL     I2C_WBY
          RET

          DSEG
.END
