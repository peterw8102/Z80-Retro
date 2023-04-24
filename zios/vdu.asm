import ../zlib/defs.asm
import ../zlib/zlib.asm
import config.asm
import pcb_def.asm
; vdu.asm
; Check whether there's a graphics card and a keyboard attached and if so
; start the local console (this runs alongside the serial port console)

            public VDU_INI,VDU_OFF,CTCPROC
            extrn  HASPIO,HASVDU

            CSEG

; ------ VDU_INI
; Programme up the Z80 CTC to interrupt periodically and scan for keyboard codes.
VDU_INI:    CALL  HASPIO
            RET   NZ
            CALL  HASVDU
            RET   NZ

            BANK    1,CSET_PG
            LD      A,MN_PG
            LD      HL,$4000
            CALL    V_INIT

            ; Have everything that's required for our own console (NOT: can't detect
            ; whether a keyboard is actually plugged in with the Omega keyboard interface).

            CALL    KBDINIT
            LD      HL,TMINT0
            LD      (0xC000+CTC_ICH3),HL       ; Install interrupt vector for all 4 channels
            CTC_VAL 0,$F0                      ; Set interrupt vector

            ; Use Time 3 to generate a periodic interrupt to scan the keyboard
            ; for key presses and to blink the cursor.
            ;
            ; ASSUME: The RTC (DS1307+) is configured to generate a 4KHz signal on
            ; SQW and that jumper P4 connects SQW to timer 3 input.
            CTC_VAL 3,11101111b,130      ; Channel 3 counts time 0 events so can divide down another 256

            ; Make sure the DS1307+ is configured to generate a 4KHz output signal on the SQW/OUT pin. This
            ; is used as the input (CLK/TRG3) for timer 3.
            CALL    RTC_SOS

            RET

; ------ VDU_OFF
; Disable timer interrupts. Stops scanning the keyboard and blinking the cursor. Only clear down
; timer 3 because that's all we're responsible for.
VDU_OFF:    CTC_VAL 3,00000011b      ; Channel 3 software reset and disable interrup[ts.
            RET
; Process CTC.
CTCPROC:: PUSH    HL
          PUSH    AF

          CALL    KBDSCAN            ; Scan the keyboard

          LD      A,(cursCount)
          DEC     A
          JR      NZ,.fin

          ; LD      A,(LED_V)
          ; XOR     $70
          ; OR      1
          ; LD      (LED_V),A
          ; OUT     ($64),A


          CALL    V_CTOG
          LD      A,16

.fin:     LD      (cursCount),A
          POP     AF
          POP     HL
          RET

; ------ TMINT0
; Normal ISR function from within the supervisor code.
TMINT0:   CALL    CTCPROC
          EI
          RETI


          DSEG

LED_V::   DB      1
TCOUNT::  DW      0
cursCount DB      8
