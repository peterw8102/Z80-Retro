; **********************************************
; Implements: 'DT' command
; Syntax: DT [datetime]
; Displays or changes the date time
;
; 'datetime' is one of the two following forms:
;   YYMMDD
;   YYMMDD:HHMMSS
;   :HHMMSS
;
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

  extrn main,E_PRTERR

  public DTIME,SHDTIME


; ------------------- DTIME
; Display and eventually set date time
DTIME:    CALL     RTC_INI       ; Initialise RTC
          LD       HL,_TIME      ; and
          CALL     RTC_GET       ; get the current time
          CALL     WASTESPC
          JR       Z,shtime

          ; Expect a string in the format:
          ;   YYMMDD:HHMMSS
          ; OR
          ;   :HHMMSS
          ; Build the time up into the _TIME structure before writing to
          ; the RTC.
          ;
          ; Read current time first
          CP       ':'
          JR       Z,_settime

          LD       HL,_m1
          CALL     PRINT

          CALL     _GDECNUM     ; Expect a 2 digit year.
          JR       C,errtime
          LD       (_TIME+6),A

          CALL     _GDECNUM     ; 2 digit month
          JR       C,errtime
          LD       (_TIME+5),A

          CALL     _GDECNUM     ; 2 digit date
          JR       C,errtime
          LD       (_TIME+4),A

_stime:   CALL     BUFCHR
          JR       Z,_sdtim     ; End so only setting date
          CP       ':'
          JR       NZ,shtime

          CALL     _GDECNUM     ; Expect a 2 hours
          JR       C,errtime
          LD       (_TIME+2),A

          CALL     _GDECNUM     ; Expect a 2 mins
          JR       C,errtime
          LD       (_TIME+1),A

          CALL     _GDECNUM     ; Expect a 2 secs
          JR       C,_sdtim     ; seconds optional
          LD       (_TIME),A

_sdtim:   LD       HL,_TIME
          CALL     RTC_SET

shtime:   CALL     SHDTIME
          JR       main

errtime:  CALL     SHDTIME
          LD       HL,_baddt
          JR       E_PRTERR

_settime: LD       HL,_m2
          CALL     PRINT
          JR       _stime

_m1       DEFB     'Setting date and time',10,13,0
_m2       DEFB     'Setting time',10,13,0

; ------------------- _GDECNUM
; Read two decimal digits into the A register. The high nibble contains the first digit,
; the low nibble the second digit. So the string '23' would return A containing 23h.
; Carry flag set on error.
_GDECNUM: PUSH  BC
          CALL  BUFCHR
          JR    Z,_faildec
          CALL  DEC2BIN
          JR    C,_faildec
          ADD   A,A
          ADD   A,A
          ADD   A,A
          ADD   A,A
          LD    C,A
          CALL  BUFCHR
          JR    Z,_faildec
          CALL  DEC2BIN
          JR    C,_faildec
          OR    C
          POP   BC
          OR    A       ; Clear carry flag and set Z according to value.
          RET

_faildec: POP   BC
          SCF
          RET

; --------------------- SHDTIME
; Display date/time from RTC
SHDTIME:    CALL     RTC_INI
            LD       HL,_TIME
            CALL     RTC_GET

            ; Data read from the RTC
            LD       DE,_TIME

            ; Build up the display string
            LD       HL,_FMT+18
            LD       A,(DE)
            AND      7Fh
            CALL     _DWRD

            ; Minutes
            LD       A,(DE)
            CALL     _DWRD

            ; Hours
            LD       A,(DE)
            AND      3Fh
            CALL     _DWRD

            ; Hours
            INC      DE      ; Skip day of week
            LD       A,(DE)
            CALL     _DWRD

            ; Month
            LD       A,(DE)
            CALL     _DWRD

            ; Year
            LD       A,(DE)
            CALL     _DWRD

            LD       HL,_FMT
            CALL     PRINT_LN
            RET

_DWRD:      INC      DE
            CALL     _WRD
            DEC      HL
            RRA
            RRA
            RRA
            RRA
            CALL     _WRD
            DEC      HL
            DEC      HL
            RET


_WRD:       PUSH     AF
            PUSH     DE
            AND      0Fh
            LD       DE,_DIGITS
            ADD      A,E
            LD       E,A
            LD       A,(DE)
            LD       (HL),A
            POP      DE
            POP      AF
            RET



          DSEG

; Time format:
; 00: ESSSSSSS - E: 1 to disable clock. S - seconds
; 01: MINS
; 02: 0-X-Y-HRS
;       X: 0 - 12 hour, 1 - 24 hour reply
;       Y: 0 - AM/PM if 12 hour, or MSB of 24 hour clock
; 03: DAY OF WEEK - 1 to 7
; 04: DATE
; 05: MONTH
; 06: YEAR, 0-99
; 07: X00X00XX
;     |  |  ++ -> RS1,RS0  - 00: 1Hz, 01: 4KHz, 10: 8KHz, 11: 32KHz
;     |  +------> SQWE     - 1: Enable square wave output
;     +---------> OUT      - State of clock out when SQWE is disabled. 0 or 1
_TIME:    DEFB 00h             ; Secs + enable clock
          DEFB 22h             ; Mins
          DEFB 00000000b | 23h ; 24hr clock, 1am
          DEFB 01h             ; Day of week (?)
          DEFB 12h             ; Date
          DEFB 01h             ; Month
          DEFB 20h             ; Year
          DEFB 10010000b       ; Output enabled, 1Hz clock

_FMT:     DEFB "2019-01-01 00:00.00",NULL
_DIGITS:  DEFB "0123456789"

_baddt:   DEFB "Invalid date time format. Either YYMMDD, YYMMDD:HHMM[SS] or :HHMM[SS]",10,13
          DEFB "(seconds optional)",0
