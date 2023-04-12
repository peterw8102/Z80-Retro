SCRATCH      EQU      $C500     ; Reserve 256 byutes for a scratch area

; Some shared use of the scatch area
LASTB      EQU     SCRATCH      ; Size of last block (2 bytes)
DDMP_MDE   EQU     SCRATCH+2    ; Whether to use raw or logical addressing
SDDRV      EQU     SCRATCH+3    ; Physical SDCard number (0 or 1)
SDADDR     EQU     SCRATCH+4    ; SDCard sector address
EXECADD    EQU     SCRATCH+8    ; Execution address
