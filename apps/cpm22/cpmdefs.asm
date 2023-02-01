IOBYTE          EQU 3           ;i/o definition byte.
TDRIVE          EQU 4           ;current drive name and user number.
ENTRY           EQU 5           ;entry point for the cp/m bdos.
TFCB            EQU 5CH         ;default file control block.
TBUFF           EQU 80H         ;i/o buffer and command line storage.
TBASE           EQU 100H        ;transiant program storage area.

;   Set control character equates.
;
CNTRLC          EQU 03H         ;control-c
CNTRLE          EQU 05H         ;control-e
BS              EQU 08H         ;backspace
TAB             EQU 09H         ;tab
LF              EQU 0AH         ;line feed
FF              EQU 0CH         ;form feed
CR              EQU 0DH         ;carriage return
CNTRLP          EQU 10H         ;control-p
CNTRLR          EQU 12H         ;control-r
CNTRLS          EQU 13H         ;control-s
CNTRLU          EQU 15H         ;control-u
CNTRLX          EQU 18H         ;control-x
CNTRLZ          EQU 1AH         ;control-z (end-of-file mark)
DEL             EQU 7FH         ;rubout
