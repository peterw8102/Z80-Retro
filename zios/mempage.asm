import ../zlib/defs.asm
import config.asm

; Responsible for mapping available memory (RAM) pages to processes. There are
; currently 32 pages and a global map of used pages. Each process knows which
; pages have been allocated.

        extrn  ADD8T16

        public P_ALLOC, P_FREE, P_RES, _pages

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
          LD      C,A            ; Get the allocated page number
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






         DSEG

; Map of pages that have been allocated and are in use by the current application. Each
; page is represented by a bit in this map, currently supporting 32 pages (512KB), the
; base memory. If a bit is zero then the page is available, 1: the page has already
; been allocated.
_pages:  DB   0,0,0,0


        END
