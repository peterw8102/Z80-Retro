import defs.asm
import config.asm
import pcb_def.asm

; Responsible for mapping available memory (RAM) pages to processes. There are
; currently 32 pages and a global map of used pages. Each process knows which
; pages have been allocated.

        extrn  ADD8T16

        public P_ALLOC,P_FREE,P_RES,P_MIN,_pages

        ; ----- DEBUG -----
        extrn  PRINT,WRITE_8,NL

; ------- P_RES
; Force reserve of a specific page.
; A: Contains the page number to reserve.
P_RES:    PUSH    HL
          PUSH    BC
          PUSH    AF
          CALL    _getpg
          JR      C,_badpg1
          ; Set the bit
          LD      C,A
          LD      A,(HL)
          OR      C           ; Set the bit
          LD      (HL),A      ; Store back
_badpg1:  POP     AF
          POP     BC
          POP     HL
          RET
_m1       DEFB   'Reserve: ',0


; ------ P_MIN
; Set the minimum usable page number for applications. All RAM pages lower
; than this limit are pre-reserved.
; INPUT  A  - Minimum page number that can be allocated. RAM pages so
;             20 is the first RAM page.
P_MIN:    DEC    A
          CP     1Fh
          RET    Z
          CALL   P_RES
          JR     NZ,P_MIN
          RET

_m2       DEFB   'SET MIN: ',0


; ------- P_ALLOC
; Search the available page map for the first available page and
; return the page number.
;
; The allocated page number is returned in A
;
; If there are no free pages then return 00 and set the C flag.
P_ALLOC:  PUSH    BC
          PUSH    HL
          LD      HL,_pages
          LD      B,04        ; B: count, C: base page in the current byte
          LD      C,RAM_PG_0  ; C: First page in the current byte
_lp:      LD      A,(HL)
          INC     A           ; If this is zero then all pages have been allocated.
          JR      NZ,_fndblk

          ; All pages in this byte allocated so move on.
          LD      A,8
          ADD     C
          LD      C,A
          INC     HL
          DJNZ    _lp

          ; If we get here then there are no available pages.
_err:     POP    HL
          POP    BC
          XOR    A
          CCF                 ; Set the C to indicate error
          RET

          ; There's a page available in this byte. Which one? Find the first zero
          ; bit, set it and return the page number this represents.
          ;   C: contains the lowest page number in this byte
_fndblk:  PUSH    HL             ; Make HL available as working registers
          LD      L,(HL)         ; Flag byte
          LD      A,1            ; Bit mask
          LD      B,8            ; Number of bits to test
_nxtbt:   AND     L              ; If zero then this bit represents a free page
          JR      Z,_fndpg

          ; Not this page.
          RLCA                   ; Move test bit mask one position to the left
          INC     C              ; Page number
          DJNZ    _nxtbt         ; Test bit
          POP     HL
          JR      _err           ; Shouldn't get here but treat as no available memory

          ; Found a free page. Change the bit mask and save.
_fndpg:   OR      L              ; Set bit
          POP     HL
          LD      (HL),A         ; Save modified mask
          LD      A,C            ; Get the allocated page number
          OR      A              ; Clear carry flag
          POP     HL
          POP     BC
          RET

; ------------ P_FREE
; Free a page number. Page to free is in A. Need to find the right bit. Use page number to
; find the correct byte.
P_FREE:   PUSH    HL
          PUSH    BC
          CALL    _getpg
          JR      C,_badpg2
          ; Set the bit
          CPL                 ; Complement the mask
          LD      C,A
          LD      A,(HL)
          AND     C           ; Clear the bit
          LD      (HL),A      ; Store back
_badpg2:  POP     BC
          POP     HL
          RET


; ------------ _getpg
; Calculate the byte location and bit mask for a specific page number.
; INPUT:  A   - the page number
; OUTPUT: A   - Bitmask - '1' in the bit that represents the requested page
;         HL  - Points into _pages to the byte containing the relevant bit
;
; No registers saved.
_getpg:   SUB     RAM_PG_0
          RET     C               ; Invalid page. Return with carry
          CP      20h             ; Number of pages supported
          CCF                     ; Complement carry flag
          RET     C               ; Out of range
          LD      C,A
          ; Divide by 8 to find byte
          AND     $F8             ; Clear lower three bits
          RRCA
          RRCA
          RRCA
          LD      HL,_pages
          CALL    ADD8T16         ; HL now points to the correct byte.

          ; Now the lower three bits of the page number map to the bit.
          LD      A,0111b
          AND     C               ; A=bit number within the byte references by HL
          LD      B,A
          LD      A,1
_shft:    RLCA
          DJNZ    _shft

          ; Now A contains a single '1' identifying the bit representing the requested page.
          OR      A               ; Clear carry flag
          RET

; ------------ _PGCALC
; Take a 16 bit address in application space and translate that into a block number (0-3)
; and a 16 offset. Arrange for the 16 bit offset to point into a block one address. This
; then allows us to map the application space page into block one regardless of where it
; is in the application space.
; HL - load address. Translate into an offset  and return the offset in HL
; Return:
; A  - The block number (0-3) in application space
; HL - The adjusted offset into that page, mapped as though in block 1.
; DE - WORKING SPACE, NOT SAVED!!
_PGCALC:  LD    A,H
          RLCA
          RLCA
          AND   03h        ; A now contains the block number.
          LD    D,A        ; Save to return

          ; And map the address in HL so it's in BLOCK 1.
          LD    A,3Fh
          AND   H

          ; Set bit 6 so it's in block 1
          OR    A,40h

          ; And move back to the address
          LD    H,A
          LD    A,D        ; Return the page we selected in A and D
          RET

; ------------ PGADJ
; Take a 16 bit address in application space and translate that into a page number and an offset.
; HL - load address. Translate in a page and offset then load that page into board page 1 and
;      return the offset in HL
; Return:
; A  - The actual physical page number to be mapped
; HL - The adjusted offset into that page
; DE - WORKING SPACE, NOT SAVED!!
P_ADJ::   CALL  _PGCALC

          ; A is the logical block number in application space. Translate that in a RAM
          ; page number.
          LD    DE,PAGE_MP
          ADD   E
          LD    E,A      ; Monitor memory is linked from 2000h so won't cross a 256 byte boundary.
          LD    A,(DE)   ; DE is now the address of the PAGE_MP for the addressed page in application space
          RET


; ------------- PGRESTX
; Restore page 1 and 2 to the application space. Used after extended instructions that have to map
; two pages to deal with page boundarys.
P_RESTX:: BANK  2,(PAGE_MP+2)

; ------------- PGREST
; Restore page 1 to the application space.
P_REST::  BANK  1,(PAGE_MP+1)
          RET

; ------------- PGMAP
; HL - load address. Translate in a page and offset then load that page into board page 1 and 2
;      return the offset in HL
; A returns the page mapped.
P_MAP::   PUSH  DE
          CALL  P_ADJ    ; Returns the target page number in A
          BANK  1        ; which we map to bank 1
          POP   DE
          RET

; ---- PGMAPX
; Same as PGMAP but also places the *next* application space page into bank 2. Use this if
; operations could cross a 16K page boundary.
; INPUT:   HL - address in application space
; OUTPUT:  HL - mapped address to use within supervisor map
P_MAPX::  PUSH  DE
          CALL  _PGCALC  ; HL: Adjusted address, A the 16K application block number to map (0-3)
          PUSH  AF
          LD    DE,PAGE_MP
          ADD   E
          LD    E,A      ; Monitor memory is linked from 2000h so won't cross a 256 byte boundary.

          LD    A,(DE)   ; DE is now the address of the PAGE_MP for the addressed page in application space
          BANK  1

          INC   DE
          LD    A,(DE)
          BANK  2

          POP   AF        ; AF includes the logical bock number (0-3)
          POP   DE
          RET






         DSEG

; Map of pages that have been allocated and are in use by the current application. Each
; page is represented by a bit in this map, currently supporting 32 pages (512KB), the
; base memory. If a bit is zero then the page is available, 1: the page has already
; been allocated. The bit number must have 20h added (first RAM page is 20h)
_pages:  DB   0,0,0,0


        END
