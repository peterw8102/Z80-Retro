import ../zlib/defs.asm
import ../zlib/zlib.asm
import config.asm
import pcb_def.asm
; console.asm
; Check whether there's a graphics card and a keyboard attached and if so
; start the local console (this runs alongside the serial port console).
; Note that SIO/B is NOT used as a console device and is effective reserved
; for talking to a Raspberry Pi via a specific block protocol.

            public CNS_INI,CNS_SET
            public CNS_OUT,CNS_IN,CNS_CHK

            extrn  HASPIO,HASVDU

            CSEG

; ------ CNS_INI
; Initialise device handler blocks for SIO and console. IF there is no hardware
; for the VDU console then that becomes the same device as the serial port.
CNS_INI:    ; Copy initial dispatch table (serial port)
            XOR   A
            CALL  CNS_SET

            CALL  HASPIO
            RET   NZ
            CALL  HASVDU
            RET   NZ

            ; Modify device 1 to reference the VDU dispatch table.
            LD    HL,VduPort
            LD    (dev1),HL

            RET

; ------ CNS_SET
; Select the console device to be used.
; INPUT:  A - zero (0) serial port, one (1) VDU if installed
; OUTPUT: A - the OLD console selection (can be used to restore later)
CNS_SET:    CP      2           ; Requested device must be 0 or 1
            JR      NC,.baddev
_set:       PUSH    HL
            PUSH    DE
            PUSH    BC

            LD      L,A         ; Is the device number changing?
            LD      A,(current)
            CP      L           ; If the same then no change
            JR      Z,.nochange

            ; Changing device, change dispatch table contents
            PUSH    AF          ; old value on stack
            LD      A,L         ; Requested device
            LD      (current),A ; Change device number
            LD      HL,(dev0)   ; Device 0
            DEC     A
            JR      NZ,.copy
            LD      HL,(dev1)
.copy:      LD      DE,CNS_OUT
            LD      BC,9        ; bytes to copy
            LDIR
            POP     AF

.nochange:  POP     BC
            POP     DE
            POP     HL
.baddev:    LD      A,(current) ; Make sure we return current device even in error
            RET

; ------------- DISPATCH FUNCTION -------------
; These are the three functions that get installed at the RST vectors
CNS_OUT:    JP      TXA
CNS_IN:     JP      RXA
CNS_CHK:    JP      CKINCHAR

SerPort::   JP      TXA
            JP      RXA
            JP      CKINCHAR

VduPort::   JP      V_PRT
            JP      KBDCHAR
            JP      KBDCHK

          DSEG

dev0::      DW      SerPort            ; Pointer to device 0 (SIO) dispatch table
dev1::      DW      SerPort            ; Pointer to device 1 dispatch table. Default to SIO but change on HW detection
current::   DB      $ff                ; Current device number (0 or 1)
