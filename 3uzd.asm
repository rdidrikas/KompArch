;***************************************************************
; Programa, perrašanti 1 pertraukimo apdorojimo procedūrą; procedūra atpažįsta komandą MOV ir išveda į ekraną jos bitą w
;***************************************************************

.model small

.stack 256h

.data

	PranMUL	db "Zingsninio rezimo pertraukimas! $"
	Enteris db 13, 10, "$"
	PranNe	db "Komanda ne MUL", 13, 10, "$"
	mulKomanda db "mul $"
	address db '0000$'
	ses db 32, 61, 32
	w db 0
	poslinkis db 0
	num db 3 dup(0)
	multiple_mem db 0
	autorius db "Autorius Rokas D. Didrikas$"
	progApr db "Programa atpazista mul komanda ir isveda jos duomenis i komandine eilute$"
	operandas dw ?

	; Registrai
	r_ax db "ax$"
	r_bx db "bx$"
	r_cx db "cx$"
	r_dx db "dx$"

	r_al db "al$"
	r_bl db "bl$"
	r_cl db "cl$"
	r_dl db "dl$"
	
	r_si db "si$"
	r_di db "di$"
	r_bp db "bp$"
	r_sp db "sp$"
	
	r_ah db "ah$"
	r_bh db "bh$"
	r_ch db "ch$"
	r_dh db "dh$"

	; Registru reiksmes

	ax_value dw ?
	bx_value dw ?
	dx_value dw ?
	cx_value dw ?
	bp_value dw ?
	si_value dw ?
	di_value dw ?
	sp_value dw ?

	al_value db ?

	ds_value dw ?

.code
  Pradzia:
	MOV	ax, @data	;reikalinga kiekvienos programos pradžioj
	MOV	ds, ax		;reikalinga kiekvienos programos pradžioj
	
	mov ah, 9h
	mov dx, offset autorius
	int 21h

	mov ah, 9h
	mov dx, offset Enteris
	int 21h

	mov ah, 9h
	mov dx, offset progApr
	int 21h

	mov ah, 9h
	mov dx, offset Enteris
	int 21h

;****************************************************************************
; Nusistatom reikalingas registrų reikšmes
;****************************************************************************
	MOV	ax, 0		;nėra komandos MOV es, 0 - tai reikia daryti per darbinį registrą (ax)
	MOV	es, ax		;į es įsirašome 0, nes pertraukimų vektorių lentelė yra segmente, kurio pradžios adresas yra 00000
	
;****************************************************************************
; Iššisaugome tikrą pertraukimo apdorojimo procedūros adresą, kad programos gale galėtume jį atstatyti
;****************************************************************************
	PUSH	es:[4]
	PUSH	es:[6]
	
;****************************************************************************
; Pertraukimų vektorių lentelėje suformuojame pertraukimo apdorojimo procedūros adresą
;****************************************************************************
	MOV	word ptr es:[4], offset ApdorokPertr	;į pertraukimų vektorių lentelę įrašome pertraukimo apdorojimo procedūros poslinkį nuo kodo segmento pradžios
	MOV	es:[6], cs				;į pertraukimų vektorių lentelę įrašome pertraukimo apdorojimo procedūros segmentą


;****************************************************************************
; Testuojame pertraukimo apdorojimo procedūrą
;****************************************************************************
	PUSHF			;Išsisaugome SF reikšmę testavimo pradžioje
	PUSHF			;Išsisaugome SF kad galėtume ją išimti ir nustatyti TF
	POP ax			;Išimame SF reikšmę į TF
	OR ax, 0100h		;Nustatome TF=1
	PUSH ax			;Įdedame pakoreguotą reikšmę
	POPF			;Išimame pakoreguotą reikšmę į SF; Nuo čia TF=1
	NOP			;Pirmas pertraukimas kyla ne prieš šią komandą, o po jos; todėl tiesiog vieną komandą nieko nedarome
	


	;mov byte ptr [bx], 6h
	mov al, 07h
	imul bx
	mov byte ptr [bx+si+1234h], 11h
	MUL	byte ptr [bx+si+1234h]
	mov byte ptr [bx+si+12h], 08h
	MUL	byte ptr [bx+si+12h]
	mov byte ptr [si+12h], 5h
	mul byte ptr [si+12h]
	mov byte ptr [si], 31h
	mul byte ptr [si]
	mul bx
	mov bl, 14
	mul bl
	mov ax, 0
	mov byte ptr [bx+4144h], 8h
	mul byte ptr [bx+4144h]
	mov byte ptr [bp+di+4h], 1Eh
	mul byte ptr [bp+di+4h]
	db 0C6h, 006h, 034h, 012h, 06h ; mov byte ptr [1234h], 5h
	db 0F6h, 026h, 034h, 012h  ; mul byte ptr [1234h]	
	POPF			;Ištraukiame iš steko testavimo pradžioje buvusią SF reikšmę
				;Kadangi tada TF buvo lygi 0, tai tokiu būdu numušame TF
	
;**************************************************************************** 
; Atstatome tikrą pertraukimo apdorojimo programos adresą pertraukimų vektoriuje
;****************************************************************************
	POP	es:[6]
	POP	es:[4]

	MOV	ah, 4Ch		;reikalinga kiekvienos programos pabaigoj
	MOV	al, 0		;reikalinga kiekvienos programos pabaigoj
	INT	21h		;reikalinga kiekvienos programos pabaigoj

;****************************************************************************
; Pertraukimo apdorojimo procedūra
;****************************************************************************
PROC ApdorokPertr
	;Įdedame registrų reikšmes į steką
	mov ax_value, ax
	mov al_value, al
	mov bx_value, bx
	mov cx_value, cx
	mov dx_value, dx
	mov bp_value, bp
	mov si_value, si
	mov di_value, di

	mov ds_value, ds

	PUSH	ax
	PUSH	bx
	PUSH	dx
	PUSH	bp
	PUSH	es
	PUSH	ds

	;Nustatome DS reikšmę, jei pertraukimą iškviestų kita programa
	MOV	ax, @data
	MOV	ds, ax

	;Į registrą DL įsirašom komandos, prieš kurią buvo iškviestas INT, operacijos kodą
	MOV bp, sp		;Darbui su steku patogiausia naudoti registrq BP
	ADD bp, 12		;Suskaičiuojame kaip giliai steke yra įdėtas grįžimo adresas
	MOV bx, [bp]		;Į bx įdedame grįžimo adreso poslinkį nuo segmento pradžios (IP)
	MOV es, [bp+2]		;Į es įdedame grįžimo adreso segmentą (CS)
	MOV dl, [es:bx]		;Išimame pirmąjį baitą, esantį grįžimo adresu - komandos OPK
	
	MOV al, dl
	CMP al, 11110111b		;Ar tai Mul ir w = 1
	JE w1
	CMP al, 11110110b		;Ar tai mul ir w = 0
	JE Mul1

	JMP skip		;Jei tai ne MOV, tai išeiname iš pertraukimo apdorojimo procedūros

	;Jei INT buvo iškviestas prieš komandą MOV, tai tada dl registre suformuojame bito w reikšmę
w1:
	mov al, 32
	mov w, al

Mul1:
	push dx

	xor ax, ax
	mov al, [es:bx+1]
	and al, 00111000b
	cmp al, 00100000b
	je nextGood
	pop dx
	jmp skip
nextGood:
	xor ax, ax

	MOV ah, 9
	MOV dx, offset PranMUL
	INT 21h

	mov ax, es
	call toHex
	mov dx, ':'
	mov ah, 2
	int 21h
	mov ax, bx
	call toHex
	
	mov ah, 2
	mov dx, 32
	int 21h

	mov ah, [es:bx]
	inc bx
	mov al, [es:bx]

	call toHex

	mov ah, 2
	mov dx, 32
	int 21h

	mov ah, 9
	mov dx, offset mulKomanda
	int 21h

	mov ah, 9
	mov dx, 32
	int 21h
	
	mov al, [es:bx]
	and al, 11000000b ; paimam mod reiksme
	cmp al, 11000000b
	jne nextmod1
	jmp register
nextmod1:
	cmp al, 00000000b
	jne nextmod2
	jmp op_noP
nextmod2:
	cmp al, 01000000b
	jne nextmod3
	mov al, 1
	mov poslinkis, al
	jmp op_P1
nextmod3:
	mov al, 2
	mov poslinkis, al
	jmp op_P1

register:
	mov al, [es:bx]
	and al, 00000111b ; paimam r/m reiksme
	cmp al, 000b
	je print_ax
	cmp al, 011b
	je print_bx
	jmp register2
; *****************************************************
; Registru spausdinimas #1
; *****************************************************
print_ax:
	mov al, w
	cmp al, 0
	je print_al
	mov ax, offset r_ax
	call spausdink
	mov ax, ax_value
	call charToHex
	jmp pabaiga
print_al:
	xor ax, ax
	mov ax, offset r_al
	call spausdink
	mov ax, ax_value
	call charToHex
	jmp pabaiga
print_bx:
	mov al, w
	cmp al, 0
	je print_bl
	mov ax, offset r_bx
	call spausdink
	mov ax, bx_value
	call charToHex
	jmp pabaiga
print_bl:
	xor ax, ax
	mov ax, offset r_bl
	call spausdink
	mov ax, bx_value
	call charToHex
	jmp pabaiga
; *****************************************************
; Registru spausdinimas #2
; *****************************************************
register2:
	cmp al, 010b
	je print_dx
	cmp al, 001b
	je print_cx
	jmp register3
print_cx:
	mov al, w
	cmp al, 0
	je print_cl
	mov ax, offset r_cx
	call spausdink
	mov ax, cx_value
	call charToHex
	jmp pabaiga
print_cl:
	xor ax, ax
	mov ax, offset r_cl
	call spausdink
	mov ax, cx_value
	call charToHex
	jmp pabaiga
print_dx:
	mov al, w
	cmp al, 0
	je print_bl
	mov ax, offset r_dx
	call spausdink
	mov ax, dx_value
	call charToHex
	jmp pabaiga
print_dl:
	xor ax, ax
	mov ax, offset r_dl
	call spausdink
	mov ax, dx_value
	call charToHex
	jmp pabaiga
; *****************************************************
; Registru spausdinimas #3
; *****************************************************
register3:
	cmp al, 100b
	je print_sp
	cmp al, 101b
	je print_bp
	jmp register4
print_sp:
	mov al, w
	cmp al, 0
	je print_ah
	mov ax, offset r_sp
	call spausdink
	mov ax, sp_value
	call charToHex
	jmp pabaiga
print_ah:
	xor ax, ax
	mov ax, offset r_ah
	call spausdink
	mov ax, sp_value
	call charToHex
	jmp pabaiga
print_bp:
	mov al, w
	cmp al, 0
	je print_ch
	mov ax, offset r_bp
	call spausdink
	mov ax, bp_value
	call charToHex
	jmp pabaiga
print_ch:
	xor ax, ax
	mov ax, offset r_ch
	call spausdink
	mov ax, bp_value
	call charToHex
	jmp pabaiga
; *****************************************************
; Registru spausdinimas #4
; *****************************************************
register4:
	cmp al, 110b
	je print_si
	cmp al, 111b
	je print_di
	jmp pabaiga
print_si:
	mov al, w
	cmp al, 0
	je print_dh
	mov ax, offset r_si
	call spausdink
	mov ax, si_value
	call charToHex
	jmp pabaiga
print_dh:
	xor ax, ax
	mov ax, offset r_dh
	call spausdink
	mov ax, si_value
	call charToHex
	jmp pabaiga	
print_di:
	cmp w, 0
	je print_bh
	mov ax, offset r_di
	call spausdink
	mov ax, di_value
	call charToHex
	jmp pabaiga
print_bh:
	xor ax, ax
	mov ax, offset r_bh
	call spausdink
	mov ax, di_value
	call charToHex
	jmp pabaiga

op_noP:
	mov al, 1
	mov multiple_mem, al
	mov al, [es:bx]
	and al, 00000111b ; paimam r/m reiksme
	cmp al, 000b
	je print_bx_si
	cmp al, 001b
	je print_bx_di
	cmp al, 010b
	jne next_noP1
	jmp print_bp_si
next_noP1:
	cmp al, 011b
	jne next_noP2
	jmp print_bp_di
next_noP2:
	mov al, 0
	mov multiple_mem, al
	mov al, [es:bx]
	and al, 00000111b
	cmp al, 100b
	jne next_noP3
	jmp print_si_brackets
next_noP3:
	cmp al, 101b
	jne next_noP4
	jmp print_di_brackets
next_noP4:
	cmp al, 110b
	jne next_noP5
	jmp print_mem
next_noP5:
	jmp print_bx_brackets

print_bx_si:
	xor ax, ax
	mov ax, offset r_bx
	mov operandas, offset r_si
	call spausdinkMem
	call spausdinkKab
	mov ax, offset r_bx
	call spausdinkMem
	call spausdinkLygu
	mov bx, [bx_value]
	mov si, [si_value]
	mov ax, [bx+si]
	call charToHex
	jmp pabaiga

print_bx_di:
	xor ax, ax
	mov ax, offset r_bx
	mov operandas, offset r_di
	call spausdinkMem
	call spausdinkKab
	mov ax, offset r_bx
	call spausdinkMem
	call spausdinkLygu
	mov bx, [bx_value]
	mov di, [di_value]
	mov ax, [bx+di]
	call charToHex
	jmp pabaiga
print_bp_si:
	xor ax, ax
	mov ax, offset r_bp
	mov operandas, offset r_si
	call spausdinkMem
	call spausdinkKab
	mov ax, offset r_bp
	call spausdinkMem
	call spausdinkLygu
	mov bp, [bp_value]
	mov si, [si_value]
	mov ax, [bp+si]
	call charToHex
	jmp pabaiga
print_bp_di:
	xor ax, ax
	mov ax, offset r_bp
	mov operandas, offset r_di
	call spausdinkMem
	call spausdinkKab
	mov ax, offset r_bp
	call spausdinkMem
	call spausdinkLygu
	mov bp, [bp_value]
	mov di, [di_value]
	mov ax, [bp+di]
	call charToHex
	jmp pabaiga

print_si_brackets:
	xor ax, ax
	mov ax, offset r_si
	call spausdinkMem
	call spausdinkKab
	mov ax, offset r_si
	call spausdinkMem
	call spausdinkLygu
	mov si, [si_value]
	mov ax, [si]
	call charToHex
	jmp pabaiga

print_di_brackets:
	xor ax, ax
	mov ax, offset r_di
	call spausdinkMem
	call spausdinkKab
	mov ax, offset r_di
	call spausdinkMem
	call spausdinkLygu
	mov di, [di_value]
	mov ax, [di]
	call charToHex
	jmp pabaiga

print_mem:
	xor ax, ax
	mov ax, [es:bx+1]
	call memToHex
	mov ax, offset address
	call spausdinkMem
	call spausdinkKab
	mov ax, offset address
	call spausdinkMem
	call spausdinkLygu
	mov si, [es:bx+1]
	mov ax, [si]
	call charToHex
	jmp pabaiga

print_bx_brackets:
	xor ax, ax
	mov ax, offset r_bx
	call spausdinkMem
	call spausdinkKab
	mov ax, offset r_bx
	call spausdinkMem
	call spausdinkLygu
	mov bx, [bx_value]
	mov ax, [bx]
	call charToHex
	jmp pabaiga

op_P1:
	mov al, 1
	mov multiple_mem, al
	mov al, [es:bx]
	and al, 00000111b ; paimam r/m reiksme
	cmp al, 000b
	jne next_P11
	jmp print_bx_si_pos
next_P11:
	cmp al, 001b
	jne next_p12
	jmp print_bx_di_pos
next_p12:
	cmp al, 010b
	jne next_p13
	jmp print_bp_si_pos
next_p13:
	cmp al, 011b
	jne next_p14
	jmp print_bp_di_pos
next_p14:
	mov al, 0
	mov multiple_mem, al
	mov al, [es:bx]
	and al, 00000111b
	cmp al, 100b
	jne next_p15
	jmp print_si_pos
next_p15:

print_bx_si_pos:
	xor ax, ax
	mov ax, offset r_bx
	mov operandas, offset r_si
	call spausdinkMem
	call spausdinkKab
	mov ax, offset r_bx
	call spausdinkMem
	call spausdinkLygu
	mov dx, [es:bx+1]
	cmp poslinkis, 2
	je print_bx_si_pos2

	mov bx, [bx_value]
	mov si, [si_value]
	xor dh, dh
	add si, dx
	mov ax, [si+bx]
	call charToHex
	
	jmp pabaiga
print_bx_si_pos2:
	mov bx, [bx_value]
	mov si, [si_value]
	add si, dx
	mov ax, [si+bx]
	call charToHex	
	jmp pabaiga

print_bx_di_pos:
	xor ax, ax
	mov ax, offset r_bx
	mov operandas, offset r_di
	call spausdinkMem
	call spausdinkKab
	mov ax, offset r_bx
	call spausdinkMem
	call spausdinkLygu
	mov dx, [es:bx+1]
	cmp poslinkis, 2
	je print_bx_di_pos2

	mov bx, [bx_value]
	mov di, [di_value]
	xor dh, dh
	add di, dx
	mov ax, [di+bx]
	call charToHex
	
	jmp pabaiga
print_bx_di_pos2:
	mov bx, [bx_value]
	mov di, [di_value]
	add di, dx
	mov ax, [di+bx]
	call charToHex	
	jmp pabaiga

print_bp_si_pos:
	xor ax, ax
	mov ax, offset r_bp
	mov operandas, offset r_si
	call spausdinkMem
	call spausdinkKab
	mov ax, offset r_bp
	call spausdinkMem
	call spausdinkLygu
	mov dx, [es:bx+1]
	cmp poslinkis, 2
	je print_bp_si_pos2

	mov bp, [bp_value]
	mov si, [si_value]
	xor dh, dh
	add si, dx
	mov ax, [si+bp]
	call charToHex
	
	jmp pabaiga
print_bp_si_pos2:
	mov bp, [bp_value]
	mov si, [si_value]
	add si, dx
	mov ax, [si+bp]
	call charToHex	
	jmp pabaiga

print_bp_di_pos:
	xor ax, ax
	mov ax, offset r_bp
	mov operandas, offset r_di
	call spausdinkMem
	call spausdinkKab
	mov ax, offset r_bp
	call spausdinkMem
	call spausdinkLygu
	mov dx, [es:bx+1]
	cmp poslinkis, 2
	je print_bp_di_pos2

	mov bp, [bp_value]
	mov di, [di_value]
	xor dh, dh
	add di, dx
	mov ax, [di+bp]
	call charToHex
	
	jmp pabaiga
print_bp_di_pos2:
	mov bp, [bp_value]
	mov di, [di_value]
	add di, dx
	mov ax, [di+bp]
	call charToHex	
	jmp pabaiga
print_si_pos:
	xor ax, ax
	mov ax, offset r_si
	call spausdinkMem
	call spausdinkKab
	mov ax, offset r_si
	call spausdinkMem
	call spausdinkLygu
	mov dx, [es:bx+1]
	mov si, [si_value]
	cmp poslinkis, 2
	je print_si_pos2
	xor dh, dh
	add si, dx
	mov ax, [si]
	call charToHex
	
	jmp pabaiga
print_si_pos2:
	add si, dx
	mov ax, [si]
	call charToHex	
	jmp pabaiga
print_di_pos:
	xor ax, ax
	mov ax, offset r_di
	call spausdinkMem
	call spausdinkKab
	mov ax, offset r_di
	call spausdinkMem
	call spausdinkLygu
	mov dx, [es:bx+1]
	mov di, [di_value]
	cmp poslinkis, 2
	je print_di_pos2
	xor dh, dh
	add di, dx
	mov ax, [di]
	call charToHex
	
	jmp pabaiga
print_di_pos2:
	add di, dx
	mov ax, [di]
	call charToHex	
	jmp pabaiga
print_bp_pos:
	xor ax, ax
	mov ax, offset r_bp
	call spausdinkMem
	call spausdinkKab
	mov ax, offset r_bp
	call spausdinkMem
	call spausdinkLygu
	mov dx, [es:bx+1]
	mov bp, [bp_value]
	cmp poslinkis, 2
	je print_bp_pos2
	xor dh, dh
	add bp, dx
	mov ax, [bp]
	call charToHex
	
	jmp pabaiga
print_bp_pos2:
	add bp, dx
	mov ax, [bp]
	call charToHex	
	jmp pabaiga
print_bx_pos:
	xor ax, ax
	mov ax, offset r_bx
	call spausdinkMem
	call spausdinkKab
	mov ax, offset r_bx
	call spausdinkMem
	call spausdinkLygu
	mov dx, [es:bx+1]
	mov bx, [bx_value]
	cmp poslinkis, 2
	je print_bx_pos2
	xor dh, dh
	add bx, dx
	mov ax, [bx]
	call charToHex
	
	jmp pabaiga
print_bx_pos2:
	add bx, dx
	mov ax, [bx]
	call charToHex	
	jmp pabaiga

skip:
	mov al, 0
	mov w, al
	POP ds
	POP es
	POP bp
	POP	dx
	POP bx
	POP	ax
	IRET			;pabaigoje būtina naudoti grįžimo iš pertraukimo apdorojimo procedūros komandą IRET
				;paprastas RET netinka, nes per mažai informacijos išima iš steko

pabaiga:

	call spausdinkAX

	mov ah, 9
	mov dx, offset Enteris
	int 21h

	pop dx
	mov al, 0
	mov w, al ; reset w
	mov multiple_mem, al ; reset multiple_mem
	mov poslinkis, al ; reset poslinkis
	
	;MOV	ah, 4Ch		;reikalinga kiekvienos programos pabaigoj
	;MOV	al, 0		;reikalinga kiekvienos programos pabaigoj
	;INT	21h		;reikalinga kiekvienos programos pabaigoj

	POP ds
	POP es
	POP bp
	POP	dx
	POP bx
	POP	ax
	IRET

ApdorokPertr ENDP

PROC toHex

    push bx                 ; Save used registers
    push cx
    push dx

    mov cx, 4   
	mov bx, ax          

next_byte:

    ; Convert the high nibble
    mov dx, bx
    shr dx, 12     
	and dl, 0Fh            ; Mask to isolate the top 4 bits
    call toAscii  
	mov ah, 2
	int 21h
    shl bx, 4         		; Next 4 bits
    loop next_byte          ; Process the next byte until CX reaches 0

    pop dx                
    pop cx
    pop bx

    ret

toAscii:
    cmp dl, 9               ; Check if the nibble is <= 9
    jbe convert_digit       ; If yes, it's a decimal digit (0-9)
    add dl, 7               ; Adjust for letters A-F
convert_digit:
    add dl, '0'             ; Convert the nibble to its ASCII representation
    ret

ENDP toHex

PROC charToHex
	push dx
	push bx
	mov bx, ax
    mov bh, bl            
    shr bl, 4		; stumiam 4 bitus i desine kad paimt pirmus 4 is kaires ir juos konvertuot    
    call toAscii2
    mov dl, bl
	mov ah, 2
	int 21h      
    mov bl, bh           
    and bl, 0Fh		; istrina pirmus 4 bitus is kaires nes 00001111 and al        
    call toAscii2 
    mov dl, bl
	mov ah, 2
	int 21h   
	pop bx
	pop dx 
    ret
toAscii2:
    cmp bl, 9             
    jbe convert_digit2     ; If <= 9, skaicius
    add bl, 7             ; A-F (10-15), pridet 7
convert_digit2:
    add bl, '0'           ; Convert to ASCII by adding '0'
    ret
ENDP charToHex

PROC memToHex

    push bx                 ; Save used registers
    push cx
    push dx
	push si

    mov cx, 4   
	mov bx, ax  
	mov si, 0        

mem_next_byte:

    ; Convert the high nibble
	mov dx, bx
    shr dx, 12     
	and dl, 0Fh            ; Mask to isolate the top 4 bits
    call memToAscii  
	mov [address+si], dl
	;mov ah, 2
	;int 21h
	inc si
    shl bx, 4         		; Next 4 bits
    loop mem_next_byte          ; Process the next byte until CX reaches 0

	pop si
    pop dx                
    pop cx
    pop bx

    ret

memToAscii:
    cmp dl, 9               ; Check if the nibble is <= 9
    jbe mem_convert_digit       ; If yes, it's a decimal digit (0-9)
    add dl, 7               ; Adjust for letters A-F
mem_convert_digit:
    add dl, '0'             ; Convert the nibble to its ASCII representation
    ret

ENDP memToHex

proc spausdink
	push dx
	push bx

	mov bx, ax

	MOV ah, 9
	MOV dx, bx
	INT 21h
	
	MOV ah, 2
	mov dl, 59
	INT 21h
	
	mov ah, 2
	mov dl, 32
	INT 21h

	MOV ah, 9
	MOV dx, bx
	INT 21h

	mov ah, 2
	mov dl, 32
	int 21h

	mov ah, 2
	mov dl, 61
	int 21h	

	mov ah, 2
	mov dl, 32
	int 21h

	pop bx
	pop dx
	ret
	
endp spausdink

proc spausdinkAX

	push dx
	push bx

	mov ah, 2
	mov dl, 44
	int 21h

	cmp w, 0
	je sp_al

	mov ah, 9
	mov dx, offset r_ax
	int 21h


	mov ah, 2
	mov dl, 32 
	int 21h

	mov ah, 2
	mov dl, 61
	int 21h

	mov ah, 2
	mov dl, 32
	int 21h

	mov ax, ax_value
	call charToHex
	jmp pab

sp_al:
	mov ah, 9
	mov dx, offset r_al
	int 21h

	mov ah, 2
	mov dl, 32 
	int 21h

	mov ah, 2
	mov dl, 61
	int 21h

	mov ah, 2
	mov dl, 32
	int 21h

	mov al, al_value
	call charToHex
pab:
	pop bx
	pop dx
	ret
endp spausdinkAX

proc spausdinkMem

	push dx
	push bx
	

	mov bx, ax

	mov ah, 2
	mov dl, 91
	int 21h

	mov ah, 9
	mov dx, bx
	int 21h

	cmp multiple_mem, 0
	jne toliau

	cmp poslinkis, 0
	jne toliau_pos
	jmp s_pabaiga
toliau:
	mov ah, 2
	mov dl, 43
	int 21h

	mov bx, operandas
	mov ah, 9
	mov dx, bx
	int 21h

	cmp poslinkis, 0
	je s_pabaiga
toliau_pos:
	mov ah, 2
	mov dl, 43
	int 21h

	pop bx
	mov ax, [es:bx+1]
	push bx
	cmp poslinkis, 2
	je poslinkis2

	call chartoHex
	jmp s_pabaiga
	
poslinkis2:
	call toHex

s_pabaiga:
	mov ah, 2
	mov dl, 93
	int 21h

	pop bx
	pop dx
	ret

endp spausdinkMem

proc spausdinkKab
	push dx
	push ax

	mov ah, 2
	mov dl, 59
	int 21h

	mov ah, 2
	mov dl, 32
	int 21h

	pop ax
	pop dx
	ret

endp spausdinkKab

proc spausdinkLygu
	push dx
	push ax

	mov ah, 2
	mov dl, 32
	int 21h

	mov ah, 2
	mov dl, 61
	int 21h

	mov ah, 2
	mov dl, 32
	int 21h

	pop ax
	pop dx
	ret
endp spausdinkLygu
END Pradzia