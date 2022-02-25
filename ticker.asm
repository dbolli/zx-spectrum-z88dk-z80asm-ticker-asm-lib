
SECTION ticker_lib
PUBLIC INITTICKER
PUBLIC UPDATETICKER
PUBLIC HANDLETICKER

PUBLIC MAXNUMTICKERS

PUBLIC NUMTICKERS

EXTERN GETPRADDR
EXTERN PRSTRNG
EXTERN PRSTRNG2
EXTERN GET2

INCLUDE "../../z88dk-zxspectrum/z88dk-zxspectrum-equates.asm"

;
; **********
; * Ticker *
; **********
;
; (C) Derek Bolli 2002
;
; ***************
; Version History:
;
;   DB    2002      0.0d  Initial version
;

;
; . . . . . . . . . . .
; Constants
;
defc MAXNUMTICKERS = 32				; DB020927 19:35 Maximum of 32 tickers
;
defc TFLAGDBLHEIGHT = 1				; DB020921 16:29 Ticker is double height text
defc TFLAGFLPAPER = 2				; DB020921 16:29 Rotate colour of paper
defc TFLAGFLINK = 4					; DB020921 16:29 Rotate colour of ink
;
; . . . . . . . . . . .
; Globals
;
.NUMTICKERS	DEFB	$00				; DB020927 19:35 Current number of active tickers
.TICKERBUF	DEFS	MAXNUMTICKERS * 2	; DB020927 19:38 Addresses of current tickers
;
;.BORDPARAM1	DEFB	0				; DB020921 16:20 (IX+0) Start x coord
;			DEFB	0				; DB020921 16:20 (IX+1) Start y coord
;			DEFB	31				; DB020921 16:20 (IX+2) Width
;			DEFB	21				; DB020921 16:21 (IX+3) Height
;			DEFW	BORDEF1			; DB020921 16:31 (IX+4) Address of border char defs
;			DEFB	( $00 * $80 ) + ( $00 * $40 ) + ( $01 * $08 ) + $07	; DB020921 19:30 (IX+6) Border attribute
;;
;.BORDPARAM6	DEFB	1				; DB020921 16:20 (IX+0) Start x coord
;			DEFB	1				; DB020921 16:20 (IX+1) Start y coord
;			DEFB	29				; DB020921 16:20 (IX+2) Width
;			DEFB	2				; DB020921 16:21 (IX+3) Height
;			DEFW	BORDEF2			; DB020921 16:31 (IX+4) Address of border char defs
;			DEFB	( $00 * $80 ) + ( $00 * $40 ) + ( $01 * $08 ) + $06	; DB020921 19:30 (IX+6) Border attribute
;;
;.TICKPARAM5	DEFB	2				; DB020921 16:20 (IX+0) Start x coord
;			DEFB	2				; DB020921 16:20 (IX+1) Start y coord
;			DEFB	28				; DB020921 16:20 (IX+2) Width (can be longer or shorter than actual message)
;			DEFW	TICKMESS5		; DB020921 16:24 (IX+3) Pointer to message attr and text
;			DEFW	$0000			; DB020921 16:24 (IX+5) Pointer to current letter in message string (internal)
;			DEFB	$00				; DB020921 16:25 (IX+7) Current letter bit rotation (internal)
;			DEFB	TFLAGFLINK		; DB020921 16:27 (IX+8) Flags to control ticker behaviour
;			DEFS	$08				; DB020926 15:47 (IX+9) 8 byte buffer for next letter (internal)
;;
;.TICKMESS5	DEFB	( $00 * $80 ) + ( $00 * $40 ) + ( $02 * $08 ) + $07		; DB020921 16:03 Colour white ink on red paper
;			DEFM	"* * *  T I C K E R  T A P E  S A M P L E  C O D E "
;			defb	' ' + $80
;;
;.BORDPARAM2	DEFB	2				; DB020921 16:20 (IX+0) Start x coord
;			DEFB	9				; DB020921 16:20 (IX+1) Start y coord
;			DEFB	18				; DB020921 16:20 (IX+2) Width
;			DEFB	2				; DB020921 16:21 (IX+3) Height
;			DEFW	BORDEF2			; DB020921 16:31 (IX+4) Address of border char defs
;			DEFB	( $00 * $80 ) + ( $00 * $40 ) + ( $03 * $08 ) + $06	; DB020921 19:30 (IX+6) Border attribute
;;
;.TICKPARAM1	DEFB	3				; DB020921 16:20 (IX+0) Start x coord
;			DEFB	10				; DB020921 16:20 (IX+1) Start y coord
;			DEFB	17				; DB020921 16:20 (IX+2) Width (can be longer or shorter than actual message)
;			DEFW	TICKMESS1		; DB020921 16:24 (IX+3) Pointer to message attr and text
;			DEFW	$0000			; DB020921 16:24 (IX+5) Pointer to current letter in message string (internal)
;			DEFB	$00				; DB020921 16:25 (IX+7) Current letter bit rotation (internal)
;			DEFB	TFLAGFLPAPER	; DB020921 16:27 (IX+8) Flags to control ticker behaviour
;			DEFS	$08,$00			; DB020926 15:47 (IX+9) 8 byte buffer for next letter (internal)
;;
;.TICKMESS1	DEFB	( $00 * $80 ) + ( $00 * $40 ) + ( $05 * $08 ) + $01		; DB020921 16:03 Colour blue ink on black paper
;			DEFM	"This is a test ticker message."
;			defb	' ' + $80
;;
;.BORDPARAM3	DEFB	15				; DB020921 16:20 (IX+0) Start x coord
;			DEFB	5				; DB020921 16:20 (IX+1) Start y coord
;			DEFB	14				; DB020921 16:20 (IX+2) Width
;			DEFB	2				; DB020921 16:21 (IX+3) Height
;			DEFW	BORDEF3			; DB020921 16:31 (IX+4) Address of border char defs
;			DEFB	( $00 * $80 ) + ( $00 * $40 ) + ( $06 * $08 ) + $01	; DB020921 19:30 (IX+6) Border attribute
;;
;.TICKPARAM2	DEFB	16				; DB020921 16:20 (IX+0) Start x coord
;			DEFB	6				; DB020921 16:20 (IX+1) Start y coord
;			DEFB	13				; DB020921 16:20 (IX+2) Width (can be longer or shorter than actual message)
;			DEFW	TICKMESS2		; DB020921 16:24 (IX+3) Pointer to message attr and text
;			DEFW	$0000			; DB020921 16:24 (IX+5) Pointer to current letter in message string (internal)
;			DEFB	$00				; DB020921 16:25 (IX+7) Current letter bit rotation (internal)
;			DEFB	TFLAGFLINK		; DB020921 16:27 (IX+8) Flags to control ticker behaviour
;			DEFS	$08,$00			; DB020926 15:47 (IX+9) 8 byte buffer for next letter (internal)
;;
;.TICKMESS2	DEFB	( $00 * $80 ) + ( $00 * $40 ) + ( $03 * $08 ) + $01		; DB020921 16:03 Colour magenta ink on blue paper
;			DEFM	"This is a second test ticker message."
;			defb	' ' + $80
;;
;.BORDPARAM4	DEFB	15				; DB020921 16:20 (IX+0) Start x coord
;			DEFB	14				; DB020921 16:20 (IX+1) Start y coord
;			DEFB	14				; DB020921 16:20 (IX+2) Width
;			DEFB	2				; DB020921 16:21 (IX+3) Height
;			DEFW	BORDEF4			; DB020921 16:31 (IX+4) Address of border char defs
;			DEFB	( $00 * $80 ) + ( $00 * $40 ) + ( $07 * $08 ) + $04	; DB020921 19:30 (IX+6) Border attribute
;;
;.TICKPARAM3	DEFB	16				; DB020921 16:20 (IX+0) Start x coord
;			DEFB	15				; DB020921 16:20 (IX+1) Start y coord
;			DEFB	13				; DB020921 16:20 (IX+2) Width (can be longer or shorter than actual message)
;			DEFW	TICKMESS3		; DB020921 16:24 (IX+3) Pointer to message attr and text
;			DEFW	$0000			; DB020921 16:24 (IX+5) Pointer to current letter in message string (internal)
;			DEFB	$00				; DB020921 16:25 (IX+7) Current letter bit rotation (internal)
;			DEFB	TFLAGFLINK		; DB020921 16:27 (IX+8) Flags to control ticker behaviour
;			DEFS	$08,$00			; DB020926 15:47 (IX+9) 8 byte buffer for next letter (internal)
;;
;.TICKMESS3	DEFB	( $00 * $80 ) + ( $00 * $40 ) + ( $07 * $08 ) + $02		; DB020921 16:03 Colour red ink on white paper
;			DEFM	"This is a third test ticker message."
;			defb	' ' + $80
;;
;.BORDPARAM5	DEFB	1				; DB020921 16:20 (IX+0) Start x coord
;			DEFB	18				; DB020921 16:20 (IX+1) Start y coord
;			DEFB	29				; DB020921 16:20 (IX+2) Width
;			DEFB	2				; DB020921 16:21 (IX+3) Height
;			DEFW	BORDEF3			; DB020921 16:31 (IX+4) Address of border char defs
;			DEFB	( $00 * $80 ) + ( $00 * $40 ) + ( $01 * $08 ) + $06	; DB020921 19:30 (IX+6) Border attribute
;;
;.TICKPARAM4	DEFB	2				; DB020921 16:20 (IX+0) Start x coord
;			DEFB	19				; DB020921 16:20 (IX+1) Start y coord
;			DEFB	28				; DB020921 16:20 (IX+2) Width (can be longer or shorter than actual message)
;			DEFW	TICKMESS4		; DB020921 16:24 (IX+3) Pointer to message attr and text
;			DEFW	$0000			; DB020921 16:24 (IX+5) Pointer to current letter in message string (internal)
;			DEFB	$00				; DB020921 16:25 (IX+7) Current letter bit rotation (internal)
;			DEFB	TFLAGFLINK		; DB020921 16:27 (IX+8) Flags to control ticker behaviour
;			DEFS	$08,$00			; DB020926 15:47 (IX+9) 8 byte buffer for next letter (internal)
;;
;.TICKMESS4	DEFB	( $00 * $80 ) + ( $00 * $40 ) + ( $07 * $08 ) + $02		; DB020921 16:03 Colour red ink on white paper
;			DEFM	"Welcome to a bit of Ticker Tape Sample Code "
;			DEFB	$7F				; DB020927 18:25 © Char
;			DEFM	"1988 - 2002 Derek Bolli. The fun and enjoyment of Z80 assembly language never goes away. "
;			DEFM	"This is the end of the announcement. Thank you. That is all. You may go...               "
;			defb	' ' + $80
;;
;.TICKPARAM6	DEFB	0				; DB020921 16:20 (IX+0) Start x coord
;			DEFB	23				; DB020921 16:20 (IX+1) Start y coord
;			DEFB	32				; DB020921 16:20 (IX+2) Width (can be longer or shorter than actual message)
;			DEFW	TICKMESS6		; DB020921 16:24 (IX+3) Pointer to message attr and text
;			DEFW	$0000			; DB020921 16:24 (IX+5) Pointer to current letter in message string (internal)
;			DEFB	$00				; DB020921 16:25 (IX+7) Current letter bit rotation (internal)
;			DEFB	TFLAGFLINK		; DB020921 16:27 (IX+8) Flags to control ticker behaviour
;			DEFS	$08,$00			; DB020926 15:47 (IX+9) 8 byte buffer for next letter (internal)
;;
;.TICKMESS6	DEFB	( $00 * $80 ) + ( $00 * $40 ) + ( $01 * $08 ) + $07		; DB020921 16:03 Colour white ink on blue paper
;			DEFM	"Prime Minister tells truth * Real Estate Salesman gives true valuation of home * Press any key to continue * Bank waives fees for battling family * "
;			DEFM	"A farmer from Ohio just repaid a loan * It's a miracle * Above lyrics "
;			DEFB	$7F				; DB020928 16:20 © char
;			DEFM	"1988 - 1992 Roger Waters * Press any key to continue * Res ipsa loquitur *"
;			defb	' ' + $80
;
; . . . . . . . . . . .
;
defc TICKRSTRTX		= 0					; DB020921 16:37 Parameter table offsets
defc TICKRSTRTY		= 1	
defc TICKRWIDTH		= 2	
defc TICKRMESSG		= 3	
defc TICKRNEXTL		= 5	
defc TICKRCURRB		= 7	
defc TICKRFLAGS		= 8	
defc TICKRNLBUF		= 9	
;
.INITTICKER							; DB020921 16:07 Draw a ticker specified by the param block pointed to by IX
;
			LD		E,(IX+TICKRMESSG)
			LD		D,(IX+TICKRMESSG+1)
			LD		A,(IX+TICKRWIDTH)
			INC		A
			ADD		A,E
			LD		E,A
			LD		A,$00
			ADC		A,D
			LD		D,A
			LD		(IX+TICKRNEXTL),E
			LD		(IX+TICKRNEXTL+1),D
			LD		(IX+TICKRCURRB),$00	; DB020924 15:36 Initialise
			CALL	TICKINITNLBUF	; DB020927 16:01 Initialise next letter buffer
;			
			LD		C,(IX+TICKRSTRTX)
			LD		B,(IX+TICKRSTRTY)
			CALL	GETPRADDR		; DB020921 16:40 Get print address
;
			LD		E,(IX+TICKRMESSG)
			LD		D,(IX+TICKRMESSG+1)
			LD		A,(IX+TICKRWIDTH)
			ADD		A,E
			LD		E,A
			LD		A,$00
			ADC		A,D
			LD		D,A
			LD		A,(DE)
			OR		$80				; DB020927 13:04 Set hi bit of ASCII byte. Assume message longer than TICKRWIDTH
			LD		(DE),A
			PUSH	DE
			LD		E,(IX+TICKRMESSG)
			LD		D,(IX+TICKRMESSG+1)			
			LD		A,(IX+TICKRFLAGS)	; :dbolli:20130309 11:59:17 Get flags value
			AND 	TFLAGDBLHEIGHT		; :dbolli:20130309 11:59:17 Is double height flag set?
			JR		Z,ITSNGLHGHT
			CALL	PRSTRNG2
			JR		ITDBLHGHT2
.ITSNGLHGHT	CALL	PRSTRNG			; DB020921 16:46 Print the message string
.ITDBLHGHT2	POP		DE
			LD		A,(DE)
			AND		$7F				; DB020927 13:04 Reset hi bit of ASCII byte. Assume message longer than TICKRWIDTH
			LD		(DE),A
;
			LD		HL,NUMTICKERS
			LD		A,(HL)
			CP		MAXNUMTICKERS
			RET		Z				; DB020927 19:52 Don''t add ticker if maximum already active
;
			AND		A				; DB020927 19:45 Reset carry flag
			RLA						; DB020927 19:39 Get offset to next address in TICKERBUF
			INC		(HL)			; DB020927 19:40 Increment number of active tickers
			LD		E,A
			LD		D,$00
			LD		HL,TICKERBUF
			ADD		HL,DE
			PUSH	IX
			POP		DE
			LD		(HL),E
			INC		HL
			LD		(HL),D			; DB020927 19:50 Store IX in next TICKERBUF location
;
			RET
;
.UPDATETICKER
			LD		C,(IX+TICKRSTRTX)
			LD		B,(IX+TICKRSTRTY)
;			CALL	PRINTREG		; DB020927 17:41 DEBUG
			CALL	GETPRADDR		; DB020921 16:40 Get print address
;
			LD		A,(IX+TICKRWIDTH)
			DEC		A
			ADD		A,L
			LD		L,A				; DB020924 15:49 Adjust print address
;
			PUSH	IX
			POP		DE
			LD		A,TICKRNLBUF	; DB020927 15:51 Get offset to next letter buffer
			ADD		A,E
			LD		E,A
			LD		A,$00
			ADC		A,D
			LD		D,A
;
			LD		A,(IX+TICKRFLAGS)	; :dbolli:20130309 11:59:17 Get flags value
			AND 	TFLAGDBLHEIGHT		; :dbolli:20130309 11:59:17 Is double height flag set?
			JR		NZ,UTDBLHGHT
;
			LD		C,$08
.TICKLP1		LD		B,(IX+TICKRWIDTH)
			AND		A				; DB020924 16:05 Clear carry flag
			LD		A,(DE)
			RLA
			LD		(DE),A
			INC		DE
			PUSH	HL				; DB020927 13:38 Save print address
.TICKLP2		LD		A,(HL)
			RLA
			LD		(HL),A
			DEC		HL
			DJNZ	TICKLP2
			POP		HL				; DB020927 13:38 Restore print address
			INC		H				; DB020924 16:06 Next scan line
			DEC		C
;			CALL	PRINTREG		; DB020927 16:07 DEBUG
			JR		NZ,TICKLP1
;
			JR		UTUPDBPOS1
;
.UTDBLHGHT	PUSH	HL				; DB020927 13:38 Save print address
			PUSH	DE
;
			LD		C,$04
.TICKLP3		LD		B,(IX+TICKRWIDTH)
			AND		A				; DB020924 16:05 Clear carry flag
			LD		A,(DE)
			RLA
			LD		(DE),A
			INC		DE
			PUSH	HL				; DB020927 13:38 Save print address
.TICKLP4		LD		A,(HL)
			RLA
			LD		(HL),A
			INC		H				; :dbolli:20130812 17:53:17 Next scan line for double height
			LD		(HL),A
			DEC		H				; :dbolli:20130812 17:53:17 
			DEC		HL
			DJNZ	TICKLP4
			POP		HL				; DB020927 13:38 Restore print address
			INC		H				; DB020924 16:06 Next scan line
			INC		H				; :dbolli:20130812 17:53:17 Next scan line for double height
			DEC		C
;			CALL	PRINTREG		; DB020927 16:07 DEBUG
			JR		NZ,TICKLP3

			POP		DE
			LD		HL,$0004
			ADD		HL,DE
			EX		DE,HL
			POP		HL				; DB020927 13:38 Restore print address
			PUSH	DE
			LD		DE,$20
			ADD		HL,DE			; :dbolli:20130812 17:53:17 Next char down for double height
			POP		DE

			LD		C,$04
.TICKLP5		LD		B,(IX+TICKRWIDTH)
			AND		A				; DB020924 16:05 Clear carry flag
			LD		A,(DE)
			RLA
			LD		(DE),A
			INC		DE
			PUSH	HL				; DB020927 13:38 Save print address
.TICKLP6		LD		A,(HL)
			RLA
			LD		(HL),A
			INC		H				; :dbolli:20130812 17:53:17 Next scan line for double height
			LD		(HL),A
			DEC		H				; :dbolli:20130812 17:53:17 
			DEC		HL
			DJNZ	TICKLP6
			POP		HL				; DB020927 13:38 Restore print address
			INC		H				; DB020924 16:06 Next scan line
			INC		H				; :dbolli:20130812 17:53:17 Next scan line for double height
			DEC		C
;			CALL	PRINTREG		; DB020927 16:07 DEBUG
			JR		NZ,TICKLP5

.UTUPDBPOS1	LD		A,(IX+TICKRCURRB)
			INC		A
			AND		$07				; DB020927 13:21 Update current bit position within byte
			LD		(IX+TICKRCURRB),A
			JR		NZ,TICKEXIT
;
			LD		E,(IX+TICKRNEXTL)	; DB020927 13:21 If we''ve done a whole byte, we need to go to the next char in the message
			LD		D,(IX+TICKRNEXTL+1)
			INC		DE
			LD		(IX+TICKRNEXTL),E
			LD		(IX+TICKRNEXTL+1),D			
			CALL	TICKINITNLBUF	; DB020927 15:59 Init the next letter buffer
;
.TICKEXIT	RET
;
.TICKINITNLBUF
;
			LD		E,(IX+TICKRNEXTL)
			LD		D,(IX+TICKRNEXTL+1)
;
			LD		A,(DE)			; DB020924 16:25 Get ASCII char
			CP		$80
			JR		C,TICKILB1		; DB020927 17:47 Last char in message?
;
			LD		E,(IX+TICKRMESSG)
			LD		D,(IX+TICKRMESSG+1)
			LD		(IX+TICKRNEXTL),E
			LD		(IX+TICKRNEXTL+1),D	; DB020927 17:52 Set next letter pointer to first letter in message
			AND		$7F				; DB020927 17:50 Reset hi bit of ASCII char
;
.TICKILB1	LD		L,A
			LD		H,0
			ADD		HL,HL
			ADD		HL,HL
			ADD		HL,HL
			LD		DE,(CHARS)
			ADD		HL,DE			; DB020927 15:50 Address of char bitmap now in HL
;
			PUSH	IX
			POP		DE
			LD		A,TICKRNLBUF	; DB020927 15:51 Get offset to next letter buffer
			ADD		A,E
			LD		E,A
			LD		A,$00
			ADC		A,D
			LD		D,A
			LD		BC,$0008		; DB020927 15:53 8 bytes to copy
			LDIR
;
			RET
;
.HANDLETICKER LD		HL,NUMTICKERS
			LD		A,(HL)
			AND		A
			RET		Z				; DB020927 20:01 Abort if no tickers active
;
			LD		B,A
			LD		DE,$0002		; DB020927 20:02 Get offset for next ticker address 
;
			LD		HL,TICKERBUF
.HTICKLP1	PUSH	BC
			PUSH	DE
			PUSH	HL
			
			LD		A,(HL)
			LD		E,A
			INC		HL
			LD		A,(HL)
			LD		D,A
			PUSH	DE
			POP		IX
			CALL	UPDATETICKER
			
			POP		HL
			POP		DE
			ADD		HL,DE			; DB020927 20:08 Get next active ticker
			POP		BC
			DJNZ	HTICKLP1
;			
			RET
;
; . . . . . . . . . . .
;
defc DRBRDSTRTX		= 0					; DB020921 16:37 Parameter table offsets
defc DRBRDSTRTY		= 1	
defc DRBRDWIDTH		= 2	
defc DRBRDHEIGHT		= 3	
defc DRBRDBRDDEF		= 4	
defc DRBRDBRDATT		= 6	
;
.DRAWBORDER							; DB020921 16:07 Draw a border specified by the param block pointed to by IX
;
			LD		C,(IX+DRBRDSTRTX)
			LD		B,(IX+DRBRDSTRTY)
			CALL	GETPRADDR		; DB020921 16:40 Get print address
;
			LD		E,(IX+DRBRDBRDDEF)
			LD		D,(IX+DRBRDBRDDEF+1)
			LD		A,(IX+DRBRDBRDATT)
			CALL	DRBRDCHAR		; DB020921 16:49 Print the top left corner
;
			LD		C,(IX+DRBRDSTRTX)
			LD		B,(IX+DRBRDSTRTY)
			CALL	GETPRADDR		; DB020921 16:40 Get print address
;
			LD		B,(IX+DRBRDWIDTH)
			DEC		B
.DRBRDLP1	PUSH	BC
;
			LD		E,(IX+DRBRDBRDDEF)
			LD		D,(IX+DRBRDBRDDEF+1)
			LD		A,56
			ADD		A,E
			LD		E,A
			LD		A,$00
			ADC		A,D
			LD		D,A				; DB020921 17:50 Get address of top side
;
			PUSH	DE
;
			LD		A,B
			LD		C,(IX+DRBRDSTRTX)
			ADD		A,C
			LD		C,A
			LD		B,(IX+DRBRDSTRTY)
			CALL	GETPRADDR		; DB020921 16:40 Get print address
;
			POP		DE
			POP		BC
;
			PUSH	BC
			PUSH	DE
;
			LD		A,(IX+DRBRDBRDATT)
			CALL	DRBRDCHAR		; DB020921 16:49 Print the top side
;
			POP		DE
			POP		BC
;
			PUSH	BC
			PUSH	DE
;
			LD		A,B
			LD		C,(IX+DRBRDSTRTX)
			ADD		A,C
			LD		C,A
			LD		B,(IX+DRBRDSTRTY)
			LD		A,(IX+DRBRDHEIGHT)
			ADD		A,B
			LD		B,A
			CALL	GETPRADDR		; DB020921 16:40 Get print address
;
			POP		DE
			POP		BC
;
			PUSH	BC
			PUSH	DE
;
			LD		E,(IX+DRBRDBRDDEF)
			LD		D,(IX+DRBRDBRDDEF+1)
			LD		A,24
			ADD		A,E
			LD		E,A
			LD		A,$00
			ADC		A,D
			LD		D,A				; DB020921 17:50 Get address of bottom side
;
			LD		A,(IX+DRBRDBRDATT)
			CALL	DRBRDCHAR		; DB020921 16:49 Print the top side
;
			POP		DE
			POP		BC
;
			DJNZ	DRBRDLP1
;
			LD		C,(IX+DRBRDSTRTX)
			LD		B,(IX+DRBRDSTRTY)
			LD		A,(IX+DRBRDWIDTH)
			ADD		A,C
			LD		C,A
			CALL	GETPRADDR		; DB020921 16:40 Get print address
;
			LD		E,(IX+DRBRDBRDDEF)
			LD		D,(IX+DRBRDBRDDEF+1)
			LD		A,48
			ADD		A,E
			LD		E,A
			LD		A,$00
			ADC		A,D
			LD		D,A
			LD		A,(IX+DRBRDBRDATT)
			CALL	DRBRDCHAR		; DB020921 16:49 Print the top right corner
;
			LD		C,(IX+DRBRDSTRTX)
			LD		B,(IX+DRBRDSTRTY)
			CALL	GETPRADDR		; DB020921 16:40 Get print address
;
			LD		B,(IX+DRBRDHEIGHT)
			DEC		B
.DRBRDLP2	PUSH	BC
;
			LD		E,(IX+DRBRDBRDDEF)
			LD		D,(IX+DRBRDBRDDEF+1)
			LD		A,8
			ADD		A,E
			LD		E,A
			LD		A,$00
			ADC		A,D
			LD		D,A				; DB020921 17:50 Get address of left side
;
			PUSH	DE
;
			LD		C,(IX+DRBRDSTRTX)
			LD		A,B
			LD		B,(IX+DRBRDSTRTY)
			ADD		A,B
			LD		B,A
			CALL	GETPRADDR		; DB020921 16:40 Get print address
;
			POP		DE
			POP		BC
;
			PUSH	BC
			PUSH	DE
;
			LD		A,(IX+DRBRDBRDATT)
			CALL	DRBRDCHAR		; DB020921 16:49 Print the left side
;
			POP		DE
			POP		BC
;
			PUSH	BC
			PUSH	DE
;
			LD		A,B
			LD		B,(IX+DRBRDSTRTY)
			ADD		A,B
			LD		B,A
			LD		C,(IX+DRBRDSTRTX)
			LD		A,(IX+DRBRDWIDTH)
			ADD		A,C
			LD		C,A
			CALL	GETPRADDR		; DB020921 16:40 Get print address
;
			POP		DE
			POP		BC
;
			PUSH	BC
			PUSH	DE
;
			LD		E,(IX+DRBRDBRDDEF)
			LD		D,(IX+DRBRDBRDDEF+1)
			LD		A,40
			ADD		A,E
			LD		E,A
			LD		A,$00
			ADC		A,D
			LD		D,A				; DB020921 17:50 Get address of right side
;
			LD		A,(IX+DRBRDBRDATT)
			CALL	DRBRDCHAR		; DB020921 16:49 Print the right side
;
			POP		DE
			POP		BC
;
			DJNZ	DRBRDLP2
;
			LD		C,(IX+DRBRDSTRTX)
			LD		B,(IX+DRBRDSTRTY)
			LD		A,(IX+DRBRDHEIGHT)
			ADD		A,B
			LD		B,A
			CALL	GETPRADDR		; DB020921 16:40 Get print address
;
			LD		E,(IX+DRBRDBRDDEF)
			LD		D,(IX+DRBRDBRDDEF+1)
			LD		A,16
			ADD		A,E
			LD		E,A
			LD		A,$00
			ADC		A,D
			LD		D,A
			LD		A,(IX+DRBRDBRDATT)
			CALL	DRBRDCHAR		; DB020921 16:49 Print the bottom left corner
;
			LD		C,(IX+DRBRDSTRTX)
			LD		A,(IX+DRBRDWIDTH)
			ADD		A,C
			LD		C,A
			LD		B,(IX+DRBRDSTRTY)
			LD		A,(IX+DRBRDHEIGHT)
			ADD		A,B
			LD		B,A
			CALL	GETPRADDR		; DB020921 16:40 Get print address
;
			LD		E,(IX+DRBRDBRDDEF)
			LD		D,(IX+DRBRDBRDDEF+1)
			LD		A,32
			ADD		A,E
			LD		E,A
			LD		A,$00
			ADC		A,D
			LD		D,A
			LD		A,(IX+DRBRDBRDATT)
			CALL	DRBRDCHAR		; DB020921 16:49 Print the bottom right corner
;
			RET
;
.DRBRDCHAR	PUSH	HL
			PUSH	AF
			CALL	GET2
			POP		AF
			LD		(HL),A
			POP		HL
			LD		B,$08
.DRBRDCHLP1	LD		A,(DE)
			LD		(HL),A
			INC		DE
			INC		H
			DJNZ	DRBRDCHLP1
			LD		A,H
			SUB		$08
			LD		H,A
			INC		L
			RET
;
.BORDEF1	DEFB	@00000000		; DB020921 16:10 BORDEF1+0 Top left corner
			DEFB	@00000000
			DEFB	@00111111
			DEFB	@00100000
			DEFB	@00100000
			DEFB	@00100111
			DEFB	@00100100
			DEFB	@00100100
;
			DEFB	@00100100		; DB020921 16:13 BORDEF1+8 Left side
			DEFB	@00100100
			DEFB	@00100100
			DEFB	@00100100
			DEFB	@00100100
			DEFB	@00100100
			DEFB	@00100100
			DEFB	@00100100
;
			DEFB	@00100100		; DB020921 16:13 BORDEF1+16 Bottom left corner
			DEFB	@00100100
			DEFB	@00100111
			DEFB	@00100000
			DEFB	@00100000
			DEFB	@00111111
			DEFB	@00000000
			DEFB	@00000000
;
			DEFB	@00000000		; DB020921 16:14 BORDEF1+24 Bottom side
			DEFB	@00000000
			DEFB	@11111111
			DEFB	@00000000
			DEFB	@00000000
			DEFB	@11111111
			DEFB	@00000000
			DEFB	@00000000
;
			DEFB	@00100100		; DB020921 16:14 BORDEF1+32 Bottom right corner
			DEFB	@00100100
			DEFB	@11100100
			DEFB	@00000100
			DEFB	@00000100
			DEFB	@11111100
			DEFB	@00000000
			DEFB	@00000000
;
			DEFB	@00100100		; DB020921 16:13 BORDEF1+40 Right side
			DEFB	@00100100
			DEFB	@00100100
			DEFB	@00100100
			DEFB	@00100100
			DEFB	@00100100
			DEFB	@00100100
			DEFB	@00100100
;
			DEFB	@00000000		; DB020921 16:13 BORDEF1+48 Top right corner
			DEFB	@00000000
			DEFB	@11111100
			DEFB	@00000100
			DEFB	@00000100
			DEFB	@11100100
			DEFB	@00100100
			DEFB	@00100100
;
			DEFB	@00000000		; DB020921 16:14 BORDEF1+56 Top side
			DEFB	@00000000
			DEFB	@11111111
			DEFB	@00000000
			DEFB	@00000000
			DEFB	@11111111
			DEFB	@00000000
			DEFB	@00000000
;
.BORDEF2	DEFB	@00000000		; DB020921 16:10 BORDEF2+0 Top left corner
			DEFB	@00000000
			DEFB	@00011111
			DEFB	@00100000
			DEFB	@00100000
			DEFB	@00100011
			DEFB	@00100100
			DEFB	@00100100
;
			DEFB	@00100100		; DB020921 16:13 BORDEF2+8 Left side
			DEFB	@00100100
			DEFB	@00100100
			DEFB	@00100100
			DEFB	@00100100
			DEFB	@00100100
			DEFB	@00100100
			DEFB	@00100100
;
			DEFB	@00100100		; DB020921 16:13 BORDEF2+16 Bottom left corner
			DEFB	@00100100
			DEFB	@00100011
			DEFB	@00100000
			DEFB	@00100000
			DEFB	@00011111
			DEFB	@00000000
			DEFB	@00000000
;
			DEFB	@00000000		; DB020921 16:14 BORDEF2+24 Bottom side
			DEFB	@00000000
			DEFB	@11111111
			DEFB	@00000000
			DEFB	@00000000
			DEFB	@11111111
			DEFB	@00000000
			DEFB	@00000000
;
			DEFB	@00100100		; DB020921 16:14 BORDEF2+32 Bottom right corner
			DEFB	@00100100
			DEFB	@11000100
			DEFB	@00000100
			DEFB	@00000100
			DEFB	@11111000
			DEFB	@00000000
			DEFB	@00000000
;
			DEFB	@00100100		; DB020921 16:13 BORDEF2+40 Right side
			DEFB	@00100100
			DEFB	@00100100
			DEFB	@00100100
			DEFB	@00100100
			DEFB	@00100100
			DEFB	@00100100
			DEFB	@00100100
;
			DEFB	@00000000		; DB020921 16:13 BORDEF2+48 Top right corner
			DEFB	@00000000
			DEFB	@11111000
			DEFB	@00000100
			DEFB	@00000100
			DEFB	@11000100
			DEFB	@00100100
			DEFB	@00100100
;
			DEFB	@00000000		; DB020921 16:14 BORDEF2+56 Top side
			DEFB	@00000000
			DEFB	@11111111
			DEFB	@00000000
			DEFB	@00000000
			DEFB	@11111111
			DEFB	@00000000
			DEFB	@00000000
;
.BORDEF3	DEFB	@00000000		; DB020921 16:10 BORDEF3+0 Top left corner
			DEFB	@00000000
			DEFB	@00011111
			DEFB	@00110101
			DEFB	@00101010
			DEFB	@00110111
			DEFB	@00101100
			DEFB	@00110100
;
			DEFB	@00101100		; DB020921 16:13 BORDEF3+8 Left side
			DEFB	@00110100
			DEFB	@00101100
			DEFB	@00110100
			DEFB	@00101100
			DEFB	@00110100
			DEFB	@00101100
			DEFB	@00110100
;
			DEFB	@00101100		; DB020921 16:13 BORDEF3+16 Bottom left corner
			DEFB	@00110100
			DEFB	@00101011
			DEFB	@00110101
			DEFB	@00101010
			DEFB	@00011111
			DEFB	@00000000
			DEFB	@00000000
;
			DEFB	@00000000		; DB020921 16:14 BORDEF3+24 Bottom side
			DEFB	@00000000
			DEFB	@11111111
			DEFB	@01010101
			DEFB	@10101010
			DEFB	@11111111
			DEFB	@00000000
			DEFB	@00000000
;
			DEFB	@00101100		; DB020921 16:14 BORDEF3+32 Bottom right corner
			DEFB	@00110100
			DEFB	@11101100
			DEFB	@01010100
			DEFB	@10101100
			DEFB	@11111000
			DEFB	@00000000
			DEFB	@00000000
;
			DEFB	@00101100		; DB020921 16:13 BORDEF3+40 Right side
			DEFB	@00110100
			DEFB	@00101100
			DEFB	@00110100
			DEFB	@00101100
			DEFB	@00110100
			DEFB	@00101100
			DEFB	@00110100
;
			DEFB	@00000000		; DB020921 16:13 BORDEF3+48 Top right corner
			DEFB	@00000000
			DEFB	@11111000
			DEFB	@01010100
			DEFB	@10101100
			DEFB	@11010100
			DEFB	@00101100
			DEFB	@00110100
;
			DEFB	@00000000		; DB020921 16:14 BORDEF3+56 Top side
			DEFB	@00000000
			DEFB	@11111111
			DEFB	@01010101
			DEFB	@10101010
			DEFB	@11111111
			DEFB	@00000000
			DEFB	@00000000
;
.BORDEF4		DEFB	@00000000		; DB020921 16:10 BORDEF4+0 Top left corner
			DEFB	@00000000
			DEFB	@00011111
			DEFB	@00110101
			DEFB	@00101010
			DEFB	@00110111
			DEFB	@00101100
			DEFB	@00110100
;
			DEFB	@00001000		; DB020921 16:13 BORDEF4+8 Left side
			DEFB	@00010000
			DEFB	@00001000
			DEFB	@00010000
			DEFB	@00001000
			DEFB	@00010000
			DEFB	@00001000
			DEFB	@00010000
;
			DEFB	@00101100		; DB020921 16:13 BORDEF4+16 Bottom left corner
			DEFB	@00110100
			DEFB	@00101011
			DEFB	@00110101
			DEFB	@00101010
			DEFB	@00011111
			DEFB	@00000000
			DEFB	@00000000
;
			DEFB	@00000000		; DB020921 16:14 BORDEF4+24 Bottom side
			DEFB	@00000000
			DEFB	@00000000
			DEFB	@01010101
			DEFB	@10101010
			DEFB	@00000000
			DEFB	@00000000
			DEFB	@00000000
;
			DEFB	@00101100		; DB020921 16:14 BORDEF4+32 Bottom right corner
			DEFB	@00110100
			DEFB	@11101100
			DEFB	@01010100
			DEFB	@10101100
			DEFB	@11111000
			DEFB	@00000000
			DEFB	@00000000
;
			DEFB	@00001000		; DB020921 16:13 BORDEF4+40 Right side
			DEFB	@00010000
			DEFB	@00001000
			DEFB	@00010000
			DEFB	@00001000
			DEFB	@00010000
			DEFB	@00001000
			DEFB	@00010000
;
			DEFB	@00000000		; DB020921 16:13 BORDEF4+48 Top right corner
			DEFB	@00000000
			DEFB	@11111000
			DEFB	@01010100
			DEFB	@10101100
			DEFB	@11010100
			DEFB	@00101100
			DEFB	@00110100
;
			DEFB	@00000000		; DB020921 16:14 BORDEF4+56 Top side
			DEFB	@00000000
			DEFB	@00000000
			DEFB	@01010101
			DEFB	@10101010
			DEFB	@00000000
			DEFB	@00000000
			DEFB	@00000000
;
