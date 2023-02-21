; ---------- MORE --------------
; A simple 'continue' processor for commands. There's a single exported
; function that takes two pointers to two strings. Each string is a
; ZLoader command string.
;
; A third string is provided which is the prompt to use.
;
; The first string is the command to run if the user presses the space bar.
; The second string is the command to run if the user presses return.
;
; Both string can be the same.
;
; If the user presses any other key then the 'more' prompt is removed,
; the key pressed is moved to the command buffer and the user is
; moved back to the standard input mode.
;
; INPUTS: HL    - Pointer to three 16 bit numbers (pointers)
;        (HL)   - The prompt to display to the user
;        (HL+2) - Command string to execute if the user presses SPACE
;        (HL+4) - Command string to execute if the user presses RETURN
;
import zlib.asm

  extrn  main,EXEC_LN
  extrn  INBUF,UNGET

  public MORE

MORE:    LD     E,(HL)    ; Display the prompt
         INC    HL
         LD     D,(HL)
         INC    HL
         PUSH   HL
         EX     DE,HL

         CALL   PRINT

         ; Wait for any key to be pressed
         RST    10h
         POP    HL

         CP     ' '
         JR     Z,_spc
         CP     0Dh        ; Carriage return
         JR     Z,_ret

         ; Key we don't want pressed. 'unget' this and return to the
         LD     (_prompt+2),A
         LD     HL,_clrln
         CALL   PRINT
         LD     HL,_prompt
         CALL   PRINT
         LD     HL,_prompt+2
         CALL   SET_LINE
         CALL   EDT_LINE
         CALL   NL
         JR     EXEC_LN

_ret:    INC    HL
         INC    HL         ; Use the second command string

_spc:    LD     E,(HL)
         INC    HL
         LD     D,(HL)
         EX     DE,HL
         CALL   SET_LINE
         LD     HL,_clrln
         CALL   PRINT
         JR     EXEC_LN

_clrln   DEFB   13,ESC,'[2K',NULL
_prompt  DEFB ">  ",0
