import ../zlib/defs.asm
import config.asm
import api.asm

; Embryonic device map. Currently mostly fixed. At some point this will become
; more dynamic.

    public    DMAPINIT,DEVMAP

    CSEG

; ---------- DMAPINIT
; Called once at initialisation time. Currently a NOP.
DMAPINIT:  RET

    DSEG

; In the following table the device ID is an arbitrary, unique non-zero value
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
           DB    0,0,0,0,0        ; NOT USED
           DB   "sd1",0,0,0,0,0

           DB    4                ; Device ID for API
           DB    DM_MBLK          ; It's a multi-drive storage device
           DB    0                ; flags (zero at the moment)
           DB    0,0,0,0,0        ; NOT USED
           DB   "sd2",0,0,0,0,0

           DB    $ff              ; End of table
