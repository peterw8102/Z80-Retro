; DEFINITIONS
; Set of common definitions that apply to the zlib libraries and describe the;
; Simple Z80 memory architecture.

; Hardware release. 16 bit number: MMmmvv
_hwrel     .EQU      20200               ; 2.2

WRITE_CHR  MACRO     chr
    LD A,&chr
    RST 08H
    ENDM
WRITE_CRLF MACRO
    LD A,CR
    RST 08H
    LD A,LF
    RST 08H
    ENDM

; Character constants
NULL       .EQU     00H
CR:        .EQU     0DH
LF:        .EQU     0AH
FF:        .EQU     0CH
BS:        .EQU     08H             ; Backspace
TAB:       .EQU     09H             ; Tab
VT:        .EQU     0BH             ; Ctrl-K
DEL:       .EQU     7fH             ; Delete
CS:        .EQU     0CH             ; Clear screen
SPC:       .EQU     20H
ESC:       .EQU     1BH

; Serial port (SIO/0) ports and related definitions
SIOA_D          .EQU     $81
SIOA_C          .EQU     $83
SIOB_D          .EQU     $80
SIOB_C          .EQU     $82

RTS_HIGH        .EQU    11101000b ; 0E8H
RTS_LOW         .EQU    11101010b ; 0EAH

; I2C DEFINITIONS
I2C_RD    EQU 1
I2C_WR    EQU 0

; SPI DEFINITIONS
SPIPORT_L EQU 68h             ; With clock LOW
SPIPORT_H EQU 69h             ; With clock HIGH
SPIIN     EQU 64h             ; Input port (read only for SPI)
SPICS_P   EQU 64h             ; Input port (read only for SPI)
SPICS_EN  EQU 05h             ; Enable CS to SDCard
SPICS_DS  EQU 01h             ; Disable CS to SDCard
SPIMASK   EQU 00h             ; Keep I2C clock high and inactive!!
SPICMD    EQU SPIMASK         ; I2C clock OFF, CS active, CLK low

SPI_WRITE MACRO byte
    LD     A,&byte
    CALL   SPI_TXB
    ENDM

SPI_CWRITE MACRO byte
    LD     A,&byte|40h
    CALL   SPI_TXB
    ENDM

; SPI_SBI
; Read a single bit into the LSB of L. H contains the value to be written to the OP port (bit 1)
SPI_SBI   MACRO
          LD     A,H
          OUT    (SPIPORT_L),A      ; Set Data with clock LOW
          OUT    (SPIPORT_H),A      ; Make clock high. SD card *should* have shifted a bit out that we can sample.
          IN     A,(SPIIN)          ; Read in the next bit.
          RRA                       ; LSB into C...
          RL     L                  ; ...and from C to LSB of L
          ENDM

          ; A contains SPICMD on entry
; SPI_SBO
; Send the MSBit of D to the slave, leaving D shifted by one position.
SPI_SBO   MACRO
          local _spi_tx
          XOR    A
          SLA    D                ; MSB into Carry
          RLA                     ; Move that bit into the accumulator
_spi_tx:  OUT    (SPIPORT_L),A    ; Output data and clock LOW
          OUT    (SPIPORT_H),A    ; Clock high. SD car *should* have shifted out it's first bit that we can sample.
          ENDM
