import defs.asm
import config.asm
import zapi.asm

import pcb_def.asm
import zlib.asm
import zios.asm
import zload.asm

  extrn  SETDMA
  extrn  BADPS

  public SADDR,SDMP_MD,SDMP_L


; -------- SADDR
; Parse the SDCard address:
;   NNNNNNNN - 32 bit sector address into the whole SDCard
;   NNNNNNN: - 32 bit sector offset into the currently selected default drive (last mapped or listed)
;   NNNNNN:L - 32 bit offset into the specified logical drive (L becomes the default)
; Results stored in SDMP_MD and SDMP_L
; Returns Carry: True if error, false if ok
;         C:     The *read* command code
;         HLDE:  The offset (either raw or relative to logical drive letter)
SADDR:   CALL  SETDMA

         CALL  WASTESPC
         JR    NZ,_ginp

         ; No address. Get stored value.
         LD    A,(SDMP_MD)
         LD    C,A
         INC   A
         JR    NZ,_inpok

         ; Have no preset data so error
         OR    A
         SCF       ; Set carry flag
         RET

_inpok:  LD    DE,(SDMP_L)
         LD    HL,(SDMP_L+2)
         RET

         ; Expect a 32 bit number
_ginp:   CALL  INHEX

         RET   C      ; Bad value entered

         ; HLDE is the 512 sector address. This is either absolute OR relative to a specific
         ; mapped logical drive.
         CALL  BUFCHR
         LD    C,A_DSKRD

         JR    Z,_sabs    ; Absolute address

         ; Relative to a specific logical drive.
         CP    ':'
         JR    NZ,_sabs

         ; relative to a specific drive (or default)
         CALL  BUFCHUP
         JR    Z,BADPS    ; Missing drive letter

         ; This needs to be a logical drive number (0-15)
         SUB   'A'
         JR    C,BADPS
         CP    16
         JR    NC,BADPS

_defdrv: ; Get the drive offset which is 13 bits in DE
         LD    H,A       ; Save drive letter
         LD    A,$1F
         AND   D
         LD    D,A       ; H: drive number, L:0, DE: truncated to 13 bits (8192 sectors = 4MB)
         LD    L,0
         JR    _sderes

_sabs:   LD    C,A_DSKRW      ; Use command 8 (raw sector read)
_sderes: LD    A,C            ; Store command and address for writebacks
         LD    (SDMP_MD),A    ; RST 30h command code
         LD    (SDMP_L),DE    ; And the 32bit address
         LD    (SDMP_L+2),HL
         OR    A
         RET


         DSEG

; Record the LAST SD sector to be dumped. These need to persist over commands. The
; address (SDML_L) is either a virtual drive address OR a raw address. The content
; of SDMP_MD indicates which. If the address is raw/absolute hen it's simply a
; 32 bit sector (512 byte block) address. If it's virtual then it's in the standard
; virtual format which includes a logical drive number.
SDMP_L     DEFW    $FFFF, $FFFF
SDMP_MD    DEFB    $FF   ; Read mode. This is the API command either A_DSKRD (virtual) or A_DSKRW(raw)
