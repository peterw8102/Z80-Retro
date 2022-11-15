import defs.asm

; Command based block protocol to communicate over SIO Port B. I use this with a matching
; application running on a Raspberry Pi that accepts commands from the Z80 and repplies
; with requested data.
;
; See the CMD_B documentation below for the structure of the protocol messages. This is
; a fairly simple protocol and there's no error detection/correction and the two ends
; (Z80 and Pi) can get out of sync in which case just restart both.
;
; CMD_B is the only exported symbol.

                public CMD_B

                CSEG

; -- WRITEB
; Write the character in A to port B.
; NO REGISTERS CHANGED EXCEPT FLAGS
_TXB:           PUSH     AF              ; Store character
conout2:        XOR      A
                OUT      (SIOB_C),A
                IN       A,(SIOB_C)
                BIT      2,A             ; Set Zero flag if still transmitting character
                JR       Z,conout2       ; Loop until flag signals TX empty
                POP      AF              ; Retrieve character
                OUT      (SIOB_D),A      ; Output the character
                RET

; ------------- _SEND_CMD
; A:  Command ID
; BC: Length
; HL: Pointer to buffer with body, if C!=0
; DE: Used as workspace and NOT restored
_SEND_CMD:      LD       D,A             ; Save the command for later
                LD       A,$55           ; Write start sequence
                CALL     _TXB
                LD       A,$AA
                CALL     _TXB            ; Write out the command
                LD       A,D
                CALL     _TXB
                ; Next the 16 bit length byte - this is the value in 'BC'. 'C' sent first (LSB)
                LD       A,C
                CALL     _TXB            ; Send length LSB
                LD       A,B
                CALL     _TXB            ; Send length MSB
                ; And now the number of bytes from the TX buffer, if the length>0
                OR       C
                LD       A,D             ; Restore the command register
                RET      Z

                ; -- Write the body - number of bytes in BC
                PUSH     BC
                PUSH     HL

                ; Use 'E' as the 1 byte simple checksum
                XOR      A
                LD       E,A

                ; BC is the byte count
                OUT      (SIOB_C),A      ; A=0 - select control regiser 0
_nextbyte:      IN       A,(SIOB_C)      ; Check to see if the TX buffer is empty
                BIT      2,A             ; Set Zero flag if still transmitting character
                JR       Z,_nextbyte     ; No - not empty so check again
                LD       A,(HL)
                INC      HL
                OUT      (SIOB_D),A
                ADD      A,E
                LD       E,A
                DEC      BC
                LD       A,B
                OR       C
                JR       NZ,_nextbyte
                ; WHOLE BLOCK SENT - send the checksum
                LD       A,E
                CALL     _TXB
                LD       A,D             ; Restore command code
                POP      HL
                POP      BC
                RET

; ------------- _WAIT_RES
; Wait for a response from the server. Checksum must match the data.
; Parameters used:
;   - A the command that was sent and must patch the command in the response
;   - DE points to the response buffer
; All registers preserved EXCEPT:
;   - BC which returns the number of bytes in the body of the response
;   - C  set if error, otherwise clear
_WAIT_RES:        PUSH     HL
                  LD       L,A             ; Save the command in L
                  XOR      A               ; Poll for received character
                  OUT      (SIOB_C),A
_checkrx1:        IN       A, (SIOB_C)
                  RRCA
                  JR       NC, _checkrx1
                  IN       A,(SIOB_D)      ; FIRST CHAR

                  CP       55h             ; Check start of message
                  JR       NZ,_checkrx1
_checkrx2:        IN       A,(SIOB_C)      ; Next character should be CC
                  RRCA
                  JR       NC,_checkrx2
                  IN       A,(SIOB_D)
                  CP       $CC             ; Check start of message
                  JR       NZ,_checkrx1    ; Out of sync....

_checkrx3:        IN       A,(SIOB_C)     ; Next character should match the command sent
                  RRCA
                  JR       NC,_checkrx3
                  IN       A,(SIOB_D)
                  CP       L
                  JR       NZ,_checkrx1

_checkrx4:        IN       A,(SIOB_C)     ; Next character should result status
                  RRCA
                  JR       NC,_checkrx4
                  IN       A,(SIOB_D)
                  LD       H,A            ; Status stored in H

_checkrx5:        IN       A,(SIOB_C)    ; Next two bytes should match the length of the body if there is one.
                  RRCA
                  JR       NC, _checkrx5
                  IN       A,(SIOB_D)     ; LSB
                  LD       C,A
_checkrx6:        IN       A,(SIOB_C)    ; Next two bytes should match the length of the body if there is one.
                  RRCA
                  JR       NC,_checkrx6
                  IN       A,(SIOB_D)     ; MSB
                  LD       B,A
                  OR       C              ; If B & C are zero then no body so skip (including checksum)
                  JR       Z,_nores

                  PUSH     BC             ; Save response length to return

                  ; At this point H contains the status bytes - save that for later.

                  ; Read in the body (number of bytes in BC)
                  EX       DE,HL          ; Status now in D, HL is where to put data

                  ; Checksum in E
                  XOR      A              ; Checksum maintained in E
                  LD       E,A

                  ; Wait for character...
_checkrx7:        IN       A, (SIOB_C)
                  RRCA
                  JR       NC, _checkrx7
                  IN       A, (SIOB_D)
                  LD       (HL),A
                  INC      HL
                  ADD      A,E              ; Checksum
                  LD       E,A
                  DEC      BC
                  LD       A,C
                  OR       B
                  JR       NZ,_checkrx7

                  ; But the write address back into DE
                  EX       DE,HL            ; which leaves status back in H, csum in L

                  ; All data block characters received. The next byte should be the checksuum
_checkrx8:        IN       A, (SIOB_C)      ; Next byte SHOULD be the checksum
                  RRCA
                  JR       NC, _checkrx8
                  IN       A,(SIOB_D)       ; MSB
                  CP       L
                  ; JR       Z,_badcrc

                  LD       A,H              ; Move status back to A
                  POP      BC               ; BC now contains the body length
                  POP      HL               ; Restore SRC buffer pointer
                  OR       A
                  RET

_badcrc:          XOR      A                ; Return 0xFF for bad checksum
                  DEC      A
                  POP      BC
                  POP      HL               ; Restore SRC buffer pointer
                  RET

_nores:           LD       BC,0
                  LD       A,H              ; Status from H
                  POP      HL
                  OR       A                ; Set Z flag according to status
                  RET

; ------------- CMD_B
; Send a command to Port B and wait for a response. The protocol is synchronous and is pully
; polled for best performance at 230400baud. The maximum message size is 255 bytes. Both
; requests and responses have a header:
; -- REQUEST FORMAT
; 55   - Start Message
; AA   - Request marker
; cc   - Command identifier
; llll - 16 bit body length - which excludes the header. LSB first
; xx   - The body - ll number of bytes
; chks - 8 bit very simple checksum
;
; -- RESPONSE FORMAT
; 55   - Start Message
; CC   - Response marker
; cc   - Command identifier - MUST Match the request command identifier
; llll - 16 bit Body length - LSB first
; xx   - The response body - ll number of bytes
; chks - 8 bit very simple checksum
;
; The checksum is simple addition of all bytes in the body (only)
;
; This function takes the following parameters
;   A:  The command to send
;   HL: Pointer to data to SEND
;   DE: Pointer to a buffer to receive the response. Must be at least 256 bytes
;   BC: Number of bytes in the request
; and the response will have all registers unchanged EXCEPT:
;    A: Status byte from response
;   BC: Number of bytes in the response

; Start the send command sequence.
CMD_B:          PUSH     AF
                LD       A, $05
                OUT      (SIOA_C),A
                LD       A,RTS_HIGH
                OUT      (SIOA_C),A
                POP      AF
                PUSH     DE         ; Destination bufer not used to send the request so save
                CALL     _SEND_CMD  ; Send the command
                POP      DE
                CALL       _WAIT_RES  ; Wait for the response
                PUSH     AF
                LD       A, $05
                OUT      (SIOA_C),A
                LD       A,RTS_LOW
                OUT      (SIOA_C),A
                POP      AF
                RET
