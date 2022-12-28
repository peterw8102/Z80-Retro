; Command codes for the RST 30h system API. These are the values that go into the 'C' register
A_PGSEL      EQU    $1    ; Map a memory page into a block number
A_TXCHR      EQU    $2    ; Send character to stdout
A_RXCHR      EQU    $3    ; Wait for/return next character (stdin)
A_CHKCH      EQU    $4    ; Check for character (stdin)
A_DSKMP      EQU    $5    ; Disk drive map
A_DSKDM      EQU    $6    ; Disk set DMA address
A_DSKRD      EQU    $7    ; Disk read
A_DSKRW      EQU    $8    ; Disk raw-read
A_DSKWR      EQU    $9    ; Disk write
A_DSKWW      EQU    $A    ; Disk raw-write
A_SDSTAT     EQU    $B    ; SDCard status
A_VSTAT      EQU    $C    ; VDU card status

; ZLoader private calls
S_DSKDM      EQU    $86   ; Set DMA address to ZLoader buffer (no params)
