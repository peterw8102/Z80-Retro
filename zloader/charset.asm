import defs.asm
import config.asm
import zapi.asm

import pcb_def.asm
import zlib.asm
import zios.asm
import zload.asm

if CSET
  public INITCSET


; Only need this code if we're configured to load a user defined default character set to
; the graphics card. If there is no graphic card then the monitor image can be smaller.
; ----------- INITCSET
; Initialise the VGA character set
INITCSET:   PUSH     AF
            PUSH     BC
            PUSH     DE
            PUSH     HL

            BANK     2,$FF     ; Character set in flash
            BANK     1,CSET_PG ; Where to get the character set data

            LD       BC,$1000  ; The character set is 4K
            LD       HL,$4000  ; Source address
            LD       DE,$8000  ; Desitnation address
            LDIR

            ; Set up test data in video memory
            LD       HL,$A000  ; Start of display memory
            XOR      A         ; Character to write
            LD       B,$10     ; Outer loop

_c_cs1:     LD       C,0
_c_cs2:     LD       (HL),A
            INC      A
            INC      HL
            DEC      C
            JR       NZ,_c_cs2
            DJNZ     _c_cs1

            POP      HL
            POP      DE
            POP      BC
            POP      AF
            RET
endif
