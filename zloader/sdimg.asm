import defs.asm
import config.asm
import zapi.asm

import pcb_def.asm
import zlib.asm
import zios.asm
import zload.asm

  public IMG

  extrn  main,MORE
  extrn  BADPS
  extrn  SADDR,SDMP_L,SDMP_MD


; ---------------- IMG
; Write a memory image to the SDCard virtual disk. Parameters are:
;   WI ssss:d start end
;      ssss:d - location on SDCard
;      start  - 16 bit address for start of image (in application space)
;      end    - last byte to write
;   WB - Write a boot image
;
IMG:        CALL  SADDR      ; Sets up the target write address
            JR    C,BADPS
            CALL  WASTESPC
            CALL  GET_HEX   ; Start address in application RAM
            JR    Z,BADPS
            EX    DE,HL
            CALL  WASTESPC
            CALL  GET_HEX    ; End address => HL
            JR    Z,BADPS

            ; DE: Start address
            ; HL: End address
            ; How many sectors?
            SBC   HL,DE   ; HL is number of bytes. H/2 is number of sectors. Check for partial segments.
            LD    A,L
            OR    A
            JR    NZ,_rndup
            INC   A
            AND   H
            JR    Z,_nornd

_rndup:     INC   H
            INC   H
            JR    NZ,_nornd

_nornd:     SRL   H               ; H is now the number of 512 blocks to write. Write data...
            JR    NZ,_dowr
            LD    H,80h           ; Trying to write the maximum 64K which causes an overflow. Max is 80h pages.
_dowr:      LD    B,H
            PUSH  HL
            PUSH  AF
            LD    HL,_WSDPG
            CALL  PRINT
            LD    L,B
            LD    H,0
            CALL  WRITE_D
            CALL  NL
            POP   AF
            POP   HL

            EX    DE,HL           ; DE back to being the start address.
_nxblk:     PUSH  BC
            PUSH  HL
            LD    C,A_DSKDM
            RST   30h             ; Tell API where to get data.
            LD    DE,(SDMP_L)
            LD    HL,(SDMP_L+2)   ; HLDE is the logical address
            LD    A,(SDMP_MD)     ; The read command - turn into write command
            ADD   A_DSKWR-A_DSKRD
            LD    C,A
            RST   30h             ; Write sector

            ; Step forward to next sector
            LD    DE,(SDMP_L)
            INC   DE
            LD    (SDMP_L),DE
            ; Overflow to next page
            LD    A,D
            OR    E
            JR    NZ,_noov

            LD    HL,(SDMP_L+2)
            INC   HL
            LD    (SDMP_L+2),HL

_noov:      POP   HL              ; Move read address forward 512 bytes.
            LD    DE,512          ; sector size
            ADD   HL,DE
            POP   BC
            DJNZ  _nxblk          ; Write the next block

            JR    main


_WSDPG:      DEFB "SDCard sector count: ",NULL
