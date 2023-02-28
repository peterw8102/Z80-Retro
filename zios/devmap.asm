import defs.asm
import config.asm
import zapi.asm

; Embryonic device map. Currently mostly fixed. At some point this will become
; more dynamic.

    public    DMAPINIT,DEVMAP

    CSEG

; ---------- DMAPINIT
; Called once at initialisation time. Currently a NOP.
DMAPINIT:  RET

    DSEG

; In the following table the device ID is an arbitrary, unique non-zero value.
; Format of each record is:
;    Byte:bit   Description
;       0       Device ID. A value of zero means an empty slot.
;       1       Device type: See the definitions in 'zapi.asm'
;       2       Flags - currently not defined
;     3-7       Device type specific. See below
;    8-15       Device name, if less than 8 characters right padded with zeroes
;
; The device specific elements currently defined are:
; DM_MBLK:
;     3-4       16 bit number of logical 'drives' hosted on this device
;         NOTE: The minimum logical drive number is '1'
;       5       Size of each logical drive in MB (all will be the same size)
;               This allows a maximum drive size of 256MB
;
; ZAPI contains definitions for these fields.
;
DEVMAP:    DB    1                ; Device ID for API
           DB    DM_CIN           ; It's a storage device
           DB    0                ; flags (zero at the moment)
           DB    0,0,0,0,0        ; NOT USED
           DB   "cin",0,0,0,0,0

           DB    2                ; Device ID for API
           DB    DM_CIN           ; It's a storage device
           DB    0                ; flags (zero at the moment)
           DB    0,0,0,0,0        ; NOT USED
           DB   "cout",0,0,0,0

           DB    3                ; Device ID for API
           DB    DM_MBLK          ; It's a multi-drive storage device
           DB    0                ; flags (zero at the moment)
           DB    $ff,$3           ; 3FF (1023). Maximum logical drive number
           DB    $04              ; 4MB drives
           DB    0,0              ; NOT USED
           DB   "sd1",0,0,0,0,0

           DB    4                ; Device ID for API
           DB    DM_MBLK          ; It's a multi-drive storage device
           DB    0                ; flags (zero at the moment)
           DB    $ff,$3           ; 3FF (1023). Maximum logical drive number
           DB    $04              ; 4MB drives
           DB    0,0              ; NOT USED
           DB   "sd2",0,0,0,0,0

           DB    $ff              ; End of table
