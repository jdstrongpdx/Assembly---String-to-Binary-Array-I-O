TITLE Project 6     (Proj6_stronjoe.asm)

; Author: Joel Strong
; Last Modified: 6/8/2023
; OSU email address: xxx
; Course number/section:   CS271 Section 400
; Project Number: 6        Due Date:  6/11/23
; Description: Creation of a program that can:
;				1. read a series of ASCII input character values from the user and convert those values into a 
;				   binary value that fits in an SDWORD for storage in an array.  
;				2. convert a binary value back into an ASCII character string to write the value to the console
;				3. Collect 10 numbers using (1) and store the SDWORD values in an array
;				4. Calculate the sum and truncated average values of the array and using (2), display all the 
;					entered numbers, total sum, and truncated average to the console

INCLUDE Irvine32.inc

;--------------------------------------------------------------------------------------------------
;		Name: mGetString
;		Description: displays the count of valid numbers, displays the input prompt (either error
;					  or input prompt), reads input from user, stores the input, stores the bytes read.
;		Preconditions: registers saved before macro, values for receives/returns (below) entered in 
;						correct order during macro call
;		Postconditions: registers restored after macro, user input stored in inputAdd array, bytes read
;						 stored in outputLenAdd
;		Receives:
;			validCounter = address of validCounter		(input by reference)
;			promptAdd = address of message to display   (input by reference)
;			inputCount = number of bytes to read		(input by value)
;		Returns: 
;			inputAdd = address of input/output array	(output by reference)
;			outputLenAdd = address of user entry		(input by reference)
;---------------------------------------------------------------------------------------------------
mGetString MACRO promptAdd:REQ, inputAdd:REQ, inputCount: REQ, outputLenAdd:REQ, validCounter: REQ
		; SAVE REGISTERS
		pushad

		; EXTRA CREDIT ONE - DISPLAY THE COUNTER
		mov		EBX, [validCounter]
		mov		EAX, [EBX]
		push	EAX
		call	WriteVal

		; DISPLAY THE INPUT PROMPT
		mov		ESI, [promptAdd]
		mDisplayString	ESI

		; READ THE USER INPUT
		mov		EDX, [inputAdd]
		mov		ECX, inputCount
		call	ReadString
		mov		[outputLenAdd], EAX		; save number of entered characters

		; RESTORE REGISTERS AND EXIT (NO STACK CHANGES)
		popad
ENDM

;--------------------------------------------------------------------------------------------------
;		Name: mDisplayString
;		Description: writes a string to the console using WriteString
;		Preconditions: registers saved before the macro, address offset of string array entered during macro call
;		Postconditions: registers restored after the macro
;		Receives: stringAddr = address offset of input string		(input by reference)
;		Returns: writes character values to the console from the array at the input address
;---------------------------------------------------------------------------------------------------
mDisplayString MACRO stringAddr:REQ
		
		; SAVE REGISTERS
		pushad

		; WRITE STRING ARRAY TO CONSOLE
		mov		EDX, stringAddr
		call	WriteString

		; RESTORE REGISTERS AND EXIT (NO STACK CHANGES)
		popad

ENDM

; (insert constant definitions here)

.data

	introMsg			BYTE		"Welcome to Project 6: Low-level I/O design in Assembly          by Joel Strong", 10, 13, 10, 13,
									"Please enter 10 signed decimal integers.", 10, 13,
									"Each number needs to be small enough to fit into a 32 bit register.", 10, 13,
									"After you have finished inputting the raw numbers, I will display ", 10, 13,
									"a list of the integers, their sum, and their average value. ", 10, 13, 0
	ec1Msg				BYTE		10, 13, "EC1 - Will display a counted list of valid entries.", 10, 13, 10, 13, 0
	goodbyeMsg			BYTE		10, 13, "Well, I hope this was as fun for you as it was for me!  It has been a pleasure.  Cherrio!", 10, 13, 10, 13, 0

	; READVAL VARIABLES
	getPrompt			BYTE		" valid entered numbers. Please enter a signed number: ", 0
	getErrorMsg			BYTE		" valid entered numbers. ERROR: Not a signed number or your number was too big. Please try again: ", 0
	getSignFlag			DWORD		0
	getNumCalc			DWORD		0
	getString			DWORD		10  DUP(0)
	getStringLen		DWORD		0
	getErrorFlag		DWORD		0
	getStringOutput		SDWORD		0

	; MAIN VARIABLES
	validCount			DWORD		0
	sumVal				SDWORD		0
	avgVal				SDWORD		0
	intArray			SDWORD		10  DUP(?)
	intArrayLen			DWORD		LENGTHOF intArray
	displayPrompt		BYTE		"You entered the following numbers: ", 10, 13, 0
	resultMsg			BYTE		10, 13, "You have entered the following numbers: ", 10, 13, 0
	sumMsg				BYTE		10, 13, "The sum of these numbers is: ", 0
	avgMsg				BYTE		10, 13, "The truncated average is: ", 0
	comma				BYTE		", ", 0

.code
main PROC
;--------------------------------------------------------------------------------------------------
;		Name: main
;		Description: main procedure for calling procedures/macros
;		Preconditions: none
;		Postconditions: none
;		Receives: none - all inputs/outputs are handled by macros/procedures
;		Returns: none - all inputs/outputs are handled by macros/procedures
;---------------------------------------------------------------------------------------------------
		; CLEAR REGISTERS
		xor		EAX, EAX
		xor		EBX, EBX
		xor		ECX, ECX
		xor		EDX, EDX

		; INTRODUCE PROGRAM
		mov		ESI, OFFSET introMsg
		mDisplayString ESI
		mov		ESI, OFFSET ec1Msg
		mDisplayString ESI

		; GET 10 VALUES FROM THE USER, CONVERT ASCII TO DEC AND STORE IN AN ARRAY
		mov		EDI, OFFSET intArray
		mov		ECX, intArrayLen
		push	OFFSET validCount
		push	OFFSET getPrompt
		jmp		_continue

_error:
		push	OFFSET validCount
		push	OFFSET getErrorMsg		; if error, load error message instead of prompt message
		mov		getErrorFlag, 0			; reset error flag
		jmp		_continue

_continue:
		push	OFFSET getSignFlag
		push	OFFSET getNumCalc
		push	OFFSET getErrorFlag
		push	OFFSET getStringOutput
		push	OFFSET getString
		push	OFFSET getStringLen
		mov		getString, 0			; reset getString after each try
		mov		getSignFlag, 0			; reset signFlag after each try
		call	ReadVal	
		mov		EAX, getErrorFlag
		cmp		EAX, 1
		je		_error					; if there was an entry error
		inc		validCount
		mov		EAX, getStringOutput
		stosd							; if no error, store the value in the array
		push	OFFSET validCount
		push	OFFSET getPrompt
		loop	_continue
		
		; DISPLAY THE ENTERED NUMBERS WHILE CALCULATING THE SUM
		mov		ESI, OFFSET resultMsg
		mDisplayString ESI

		xor		EAX, EAX
		xor		EBX, EBX
		mov		ESI, OFFSET intArray
		mov		ECX, intArrayLen
		cld

_loopIn:
		lodsd
		push	EAX
		call	WriteVal				; print each value
		cmp		ECX, 1					; print a comma and space between each value except the last item
		jne		_printComma
_continuePrint:
		add		EBX, EAX				; accumulate sum
		loop	_loopIn
		mov		sumVal, EBX

		; CALCULATE THE AVERAGE
		mov		EAX, sumVal
		cdq								; sign extend befor idiv
		mov		ECX, intArrayLen
		idiv	ECX
		mov		avgVal, EAX
		call	CrLf

		; DISPLAY THE RESULTS
		mov		ESI, OFFSET sumMsg		; print the sum of the array
		mDisplayString ESI

		push	sumVal
		call	WriteVal
		call	CrLf

		mov		ESI, OFFSET avgMsg		; print the average of the array
		mDisplayString ESI

		push	avgVal
		call	WriteVal
		call	CrLf

		; GOODBYE
		mov		ESI, OFFSET goodbyeMsg	; print the goodbye message
		mDisplayString ESI

		; EXIT PROGRAM
		jmp		endofprogram			; explicit naming for end of program

_printComma:
		push	ESI
		mov		ESI, OFFSET comma		; print a comma
		mDisplayString ESI
		pop		ESI
		jmp		_continuePrint

endofprogram:
	Invoke ExitProcess,0				; exit to operating system

main ENDP


ReadVal PROC
;--------------------------------------------------------------------------------------------------
;		Name: ReadVal
;		Description: uses mGetString macro to read characters to an input array.  That array will then
;					  be converted from characters to a signed integer value that fits into an SDWORD.
;					  Uses error checking to only read +,-,0-9 characters and that the value will fit
;					  into a SDWORD without overflow.  If an error, will return an error value to main.
;		Preconditions: registers saved before the procedure, address offsets for all 'receives' values pushed to 
;						the stack before procedure call
;		Postconditions: registers restored and stack cleaned up after procedure 
;		Receives: 
;			getStringLen @ EBP+8 - address offset for the number of characters the user entered during the mGetString macro
;			getString @ EBP+12 - the address offset for the input character array
;			getStringOutput @ EBP+16 - the address offset for the SDWORD return value
;			getErrorFlag @ EBP+20 - the address offset for the error flag 
;			getNumCalc @ EBP+24 - the address offset for NumCalc - used for intermediate calculation storage
;			getSignFlag @ EBP+28 - the address offset for the sign flag - used by ReadVal to negate the total if negative
;			EITHER getPrompt OR getErrorMsg @ EBP+32 - the address offset for the entry prompt in the mGetString macro
;			validCount @ EBP+36 - the address offset for the number of valid entries printed during mGetString macro
;		Returns: converted SDWORD value in getStringOutput (output parameter, by reference) OR getErrorFlag (output parameter,
;				  by reference)
;---------------------------------------------------------------------------------------------------
		push	EBP
		mov		EBP, ESP
		pushad

		; CLEAR THE REGISTERS
		xor		EAX, EAX				
		xor		EBX, EBX				
		xor		ECX, ECX
		xor		EDX, EDX

		; GET USER INPUT
		mGetString EBP+32, EBP+12, 12, EBP+8, EBP+36  ; getPrompt, getString, bufferLen, getStringLen, validCount

		; VALIDATION - NO INPUT
		mov		EAX, [EBP+8]			; getStringLen
		cmp		EAX, 0
		je		_entryError

		; LOAD STRING TO READ
		mov		ESI, [EBP+12]			; input string
		mov		EDI, [EBP+16]			; output SDWORD
		mov		ECX, [EBP+8]			; loop counter
		CLD								

_convert:
		xor		EAX, EAX
		mov		AL, BYTE PTR [ESI]		; set byte register to read values
		lodsb							; convert using string primatives

		cmp		EBX, 0
		je		_first					; if the first number, check for sign

_continue:
		cmp		AL, 48
		jl		_entryError
		cmp		AL, 57
		jg		_entryError
		sub		AL, 48				
		mov		[EBP+24], EAX			; convert ASCII to dec
		mov		EAX, 10
		mul		EDX						; value of numInt * 10
		jo		_entryError				; check for overflow
		mov		EBX, [EBP+24]
		add		EAX, EBX			    ; value of 10 * numInt + (numChar - 48)
		jo		_entryError				; check for overflow
		mov		EDX, EAX				; store result in accumulator
		inc		EBX
		loop	_convert
		jmp		_negate

		; CHECK FIRST CHARACTER FOR +/- SIGN
_first:									
		cmp  AL, 45						; if - sign
		je	_setNegative
		cmp	AL, 43						; if + sign
		je	_setPositive
		jmp	_continue

		; IF + SIGN, MOVE TO NEXT CHARACTER
_setPositive:
		inc EBX
		dec	ECX
		jmp  _convert

		; IF - SIGN, SET NEGATIVE FLAG AND MOVE TO NEXT CHARACTER
_setNegative:
		inc	EBX
		dec	ECX
		mov	EAX, 1
		mov	[EBP+28], EAX
		jmp	_convert

		; IF NEGATIVE FLAG SET, NEGATE TOTAL
_negate:
		cmp		EDX, 7FFFFFFFh			; compare maximum positive value
		jg		_entryError
		xor		EAX, EAX
		mov		EAX, [EBP+28]
		cmp		EAX, 1
		jne		_exitReadVal
		neg		EDX
		jmp		_exitReadVal

		; IF ERROR, SET ERROR FLAG AND EXIT
_entryError:
		mov		EBX, [EBP+20]
		mov		EAX, 1
		mov		[EBX], EAX
		jmp		_exitReadVal

		; SAVE VALUE TO OUTPUT, RESTORE REGISTERS, CLEANUP STACK FRAME, AND EXIT
_exitReadVal:
		mov		EAX, [EBP+16]
		mov		[EAX], EDX

		popad
		pop		EBP
		ret		32

ReadVal ENDP


WriteVal PROC
;--------------------------------------------------------------------------------------------------
;		Name: WriteVal
;		Description: converts a SDWORD value into a string of characters and prints them to the console using mWriteString
;		Preconditions: registers saved before the procedure, value of the SDWORD pushed to the stack before call 
;		Postconditions: registers restored after the procedure
;		Receives: SDWORD value at EBP+8 (input parameter, by value)
;		Returns: prints SDWORD value to the console using mWriteString
;---------------------------------------------------------------------------------------------------
		local	localString[16]: BYTE
		; SAVE REGISTERS
		pushad

		; CLEAR THE REGISTERS
		xor		EAX, EAX
		xor		EBX, EBX
		xor		ECX, ECX
		xor		EDX, EDX

		lea		EDI, localString	; set EDI to localString array
		mov		EAX, [EBP+8]		; load input value
		cmp		EAX, 0
		jl		_negate
		jmp		_convertLoop

		; IF A NEGATIVE NUMBER, PRINT - SIGN
_negate:
		mov		EAX, [EBP+8]		; load input value and negate
		neg		EAX					
		jmp		_convertLoop

		; DIVIDE BY 10 AND PUSH REMAINDER TO STACK UNTIL QUOTIENT 0
_convertLoop:
		cdq
		mov		EBX, 10
		idiv	EBX
		cmp		EAX, 0
		je		_loadArrayInit
		push	EDX
		inc		ECX
		jmp		_convertLoop

		; PUSH LAST VALUE AND CHECK IF MINUS NEEDED
_loadArrayInit:
		push	EDX					; save last value from the loop
		inc		ECX
		mov		EAX, [EBP+8]		; load input value
		cmp		EAX, 0
		jl		_addMinus
		jmp		_loadArray

		; ADD MINUS TO START OF ARRAY IF NEGATIVE
_addMinus:
		mov		EAX, 45
		stosb

		; WRITE STRING ARRAY BY POPPING VALUES OFF THE STACK
_loadArray:
		pop		EAX	
		add		EAX, '0'
		stosb
		loop	_loadArray
		mov		EAX, 0				; save trailing zero to indicate end of string
		stosb

		; USE mDisplayString TO DISPLAY CONVERTED SDWORD TO ASCII STRING
		lea		ESI, localString
		mDisplayString ESI

		; RESTORE REGISTERS AND CLEANUP STACK FRAME BEFORE EXIT
_exitLoop:
		popad
		ret 4

WriteVal ENDP

END main
