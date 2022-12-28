; DEFINITIONS
; Set of common definitions that apply to the zlib libraries and describe the
; Simple Z80 SBC memory architecture.

; Hardware release. 16 bit number: MMmmvv
HWREL      .EQU      20200               ; 2.2

; Character constants
NULL       .EQU     00H
CR         .EQU     0DH
LF         .EQU     0AH
FF         .EQU     0CH
BS         .EQU     08H             ; Backspace
TAB        .EQU     09H             ; Tab
VT         .EQU     0BH             ; Ctrl-K
DEL        .EQU     7fH             ; Delete
CS         .EQU     0CH             ; Clear screen
SPC        .EQU     20H
ESC        .EQU     1BH

; Address of the MMU page control registers
PG_PORT0    EQU   $60
PG_CTRL     EQU   $64    ; switches paging on/off (bit 0). Off after boot. Write to 1 after initialisation.

; Serial port (SIO/0) ports and related definitions
SIOA_D          .EQU     $81
SIOA_C          .EQU     $83
SIOB_D          .EQU     $80
SIOB_C          .EQU     $82

RTS_HIGH        .EQU     11101000b ; 0E8H
RTS_LOW         .EQU     11101010b ; 0EAH

; Interuupt vector if operating in IM2. Make these the last 8 vectors in the table.
SIO_INTV        .EQU     0F8h

; Z80 CTC ports
CTC_IV          .EQU      $40
CTC_CH0         .EQU      $40
CTC_CH1         .EQU      $41
CTC_CH2         .EQU      $42
CTC_CH3         .EQU      $43

; Z80 PIO ports (PIO daughter card)
PIOA_D          .EQU      $C0
PIOB_D          .EQU      $C1
PIOA_C          .EQU      $C2
PIOB_C          .EQU      $C3

; Video card VSYNC port (bit 7 HIGH during vsync)
VGA_HSYN        .EQU      $E0

; I2C HARDWARE
I2CPORT   EQU 64h
I2CPORT_H EQU 65h
I2CIN     EQU 64h

; SPI DEFINITIONS
SPIPORT_L EQU 68h             ; With clock LOW
SPIPORT_H EQU 69h             ; With clock HIGH
SPIIN     EQU 64h             ; Input port (read only for SPI)
SPICS_P   EQU 64h             ; Input port (read only for SPI)
SPICS_EN  EQU 05h             ; Enable CS to SDCard
SPICS_DS  EQU 01h             ; Disable CS to SDCard
SPIMASK   EQU 00h             ; Keep I2C clock high and inactive!!
SPICMD    EQU SPIMASK         ; I2C clock OFF, CS active, CLK low

; INPUT SDCard values (read port 64)
SDC0_PR   EQU 08h             ; 1 if there's an SDCard present in SDCard 0
SDC1_PR   EQU 10h             ; 1 if there's an SDCard present in SDCard 1
import macros.asm
