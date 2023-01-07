; Command codes for the RST 30h system API. These are the values that go into the 'C' register
A_PGSEL      EQU    $01    ; Map a memory page into a block number
A_TXCHR      EQU    $02    ; Send character to stdout
A_RXCHR      EQU    $03    ; Wait for/return next character (stdin)
A_CHKCH      EQU    $04    ; Check for character (stdin)
A_DSKMP      EQU    $05    ; Disk drive map
A_DSKDM      EQU    $06    ; Disk set DMA address
A_DSKRD      EQU    $07    ; Disk read
A_DSKRW      EQU    $08    ; Disk raw-read
A_DSKWR      EQU    $09    ; Disk write
A_DSKWW      EQU    $0A    ; Disk raw-write
A_HWINV      EQU    $0B    ; Hardware inventory

; ZLoader private calls
S_DSKDM      EQU    $86    ; Set DMA address to ZLoader buffer (no params)
