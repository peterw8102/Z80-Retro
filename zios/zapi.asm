;==================================================================================
; Contents of this file are copyright Peter Wilson https://github.com/peterw8102
;
; Please see the LICENSE file that accompanies this code.
;
; If you use this code then attribution appreciated but not required.
;
; eMail: petew@yellowhawk.co.uk
;==================================================================================
; Command codes for the RST 30h (or CALL FE00) system API. These are the values that go
; into the 'C' register

A_EXIT       EQU    0
A_PGSEL      EQU    1    ; Map a memory page into a block number
A_PGALC      EQU    2    ; Allocate a new memory page
A_PGFRE      EQU    3    ; Free a memory page
A_TXCHR      EQU    4    ; Send character to stdout
A_RXCHR      EQU    5    ; Wait for/return next character (stdin)
A_CHKCH      EQU    6    ; Check for character (stdin)

A_DSKMP      EQU    10    ; Map logical disk number
A_QDSKMP     EQU    11    ; Query disk map
A_DSKDM      EQU    12    ; Disk set DMA address
A_DSKRD      EQU    13    ; Disk read
A_DSKRW      EQU    14    ; Disk raw-read
A_DSKWR      EQU    15    ; Disk write
A_DSKWW      EQU    16    ; Disk raw-write
A_CPMRD      EQU    17    ; Read 128 byte block (optimised for CP/M)
A_CPMWR      EQU    18    ; Write 128 byte block (optimised for CP/M)
A_CPPRG      EQU    19    ; Purge write data to device
A_CPFLS      EQU    20    ; Purge write data to device and clear RAM cache
A_CPSTS      EQU    21    ; Return and optionally clear CP/M request stats

A_HWINV      EQU    24    ; Hardware inventory
A_DVINV      EQU    25    ; Device inventory

A_DTIME      EQU    28    ; Return current date time (via a buffer)

; ZLoader private calls
S_DSKDM      EQU    $86    ; Set DMA address to ZLoader buffer (no params)

; --------------- Define types for A_DVINV --------------
; Device inventory device types.
DM_CIN:    EQU   1
DM_COUT:   EQU   2
DM_BLK:    EQU   3
DM_MBLK:   EQU   4

; --------------- Z_BUFSZ --------------
; Some operations require ZIOS to copy data into the application's
; address space. In these cases the application should provide a
; pointer to an area of memory AT LEAST this number of bytes long.
Z_BUFSZ    EQU   64

; ------------ TIME OFFSETS ------------
; Offsets of values into the buffer given to command 28 (date/time retrieval)
ZT_SEC     EQU   0
ZT_MIN     EQU   1
ZT_HOUR    EQU   2
ZT_DATE    EQU   3
ZT_MNTH    EQU   4
ZT_YEAR    EQU   5
