import ../zlib/defs.asm
import ../zlib/zlib.asm
import config.asm
import pcb_def.asm
; console.asm
; Check whether there's a graphics card and a keyboard attached and if so
; start the local console (this runs alongside the serial port console).
; Note that SIO/B is NOT used as a console device and is effective reserved
; for talking to a Raspberry Pi via a specific block protocol. This could
; change in future to make things more generaic.

            public CNS_INI,CNS_SET
            public CNS_OUT,CNS_IN,CNS_CHK
            public DEV_OUT,DEV_IN,DEV_CHK

            extrn  HASPIO,HASVDU

            CSEG

MaxDev      EQU    2

; ------ CNS_INI
; Initialise device handler blocks for SIO and console. IF there is no hardware
; for the VDU console then that becomes the same device as the serial port.
; INPUT: A - non-zero means ignore the hardware VDU
CNS_INI:    ; Copy initial dispatch table (serial port)
            OR    A
            JR    NZ,.novdu

            CALL  HASPIO
            JR    NZ,.novdu
            CALL  HASVDU
            JR    NZ,.novdu

            ; Modify device 1 to reference the VDU dispatch table.
            LD    HL,VduPort
            LD    (dev1),HL

.novdu:     XOR   A
            CALL  CNS_SET
            RET

; ------ CNS_SET
; Select the console device to be used.
; INPUT:  A - zero (0) serial port, one (1) VDU if installed
; OUTPUT: A - the OLD console selection (can be used to restore later)
CNS_SET:    CP      MaxDev      ; Requested device must be 0 or 1
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

; ------ GETDEV
; Get the dispatch table for the device referenced in the B register. If
; the device number is invalid then return the C flag CLEAR, otherwise
; return the address of the table in the HL register pair.
GETDEV:     LD      C,A           ; Save the character
            LD      A,B
            CP      MaxDev
            RET     NC
            LD      HL,dev0
            OR      A
.nxtdev:    JR      Z,.found
            INC     HL
            INC     HL
            DEC     A
            JR      .nxtdev

            ; Found
.found:     PUSH    DE
            LD      E,(HL)
            INC     HL
            LD      D,(HL)
            EX      DE,HL
            POP     DE
            SCF
            RET


; ------ DEV_OUT, DEV_IN, DEV_CHK
; Similar to the assigned CNS_XX functions but take a device number in the B register.
DEV_OUT:    CALL    GETDEV
            RET     NC
            JR      _EXEC

DEV_IN:     CALL    GETDEV
            RET     NC
            JR      _EXEC_IN

DEV_CHK:    CALL    GETDEV
            RET     NC
            JR      _EXEC_CHK

_EXEC_CHK   INC     HL
            INC     HL
            INC     HL
_EXEC_IN:   INC     HL
            INC     HL
            INC     HL

; ------ _EXEC
; HL points to a pointer (!). Load the address pointed to by HL then jump to that address.
_EXEC:      LD      A,C
            JP      (HL)


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
