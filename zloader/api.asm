; Command codes for the RST 30h system API. These are the values that go into the 'C' register
A_PGSEL      EQU    1h    ; Map a memory page into a block number
A_TXCHR      EQU    2h    ; Send character to stdout
A_RXCHR      EQU    3h    ; Wait for/return next character (stdin)
A_CHKCH      EQU    4h    ; Check for character (stdin)
A_DSKMP      EQU    5h    ; Disk drive map
A_DSKDM      EQU    6h    ; Disk set DMA address
A_DSKRD      EQU    7h    ; Disk read
A_DSKRW      EQU    8h    ; Disk raw-read
A_DSKWR      EQU    9h    ; Disk write

; ZLoader private calls
S_DSKDM      EQU    86h   ; Set DMA address to ZLoader buffer (no params)
