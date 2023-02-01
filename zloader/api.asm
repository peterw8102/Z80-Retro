; Command codes for the RST 30h system API. These are the values that go into the 'C' register
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

A_HWINV      EQU    20    ; Hardware inventory
A_DVINV      EQU    21    ; Device inventory

; ZLoader private calls
S_DSKDM      EQU    $86    ; Set DMA address to ZLoader buffer (no params)

; --------------- Define types for A_DVINV --------------
; Device inventory device types.
DM_CIN:    EQU   1
DM_COUT:   EQU   2
DM_BLK:    EQU   3
DM_MBLK:   EQU   4
