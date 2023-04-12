import ../zlib/defs.asm
import ../zlib/zlib.asm
import config.asm
import pcb_def.asm
; vdu.asm
; Check whether there's a graphics card and a keyboard attached and if so
; start the local console (this runs alongside the serial port console)

            public VDU_INI,CTCPROC
            extrn  HASPIO,HASVDU

            CSEG

; ------ VDU_INI
; Programme up the Z80 CTC to interrupt periodically and scan for keyboard codes.
VDU_INI:    CALL  HASPIO
            RET   NZ
            CALL  HASVDU
            RET   NZ

            ; LD      A,1
            BANK    1,CSET_PG
            LD      A,MN_PG
            LD      HL,$4000
            CALL    V_INIT

            ; Have everything that's required for our own console (NOT: can't detect
            ; whether a keyboard is actually plugged in with the Omega keyboard interface).

            CALL    KBDINIT
            LD      HL,TMINT0
            LD      (0xC000+CTC_ICH1),HL       ; Install interrupt vector for all 4 channels
            CTC_VAL 0,$F0                      ; Set interrupt vector

            ; Cascade channels 0 and 1. Channel 0 divides system clock
            ; to give an event approximately once every 2ms. Channel 1
            ; then divides this by 100 to give an event appromately evey
            ; 1/5th of a second (5 events per second)
            ; CTC_VAL 0,00100111b,60   ; Channel 0 divides down the system clock
            ; CTC_VAL 1,11101111b,48   ; Channel 1 counts time 0 events so can divide down another 256

            ; Working values:
            CTC_VAL 0,00100111b,60     ; Channel 0 divides down the system clock
            CTC_VAL 1,11101111b,30     ; Channel 1 counts time 0 events so can divide down another 256

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
