[org 0x0100]
 jmp start
strmiss: db 'Missed:',0
strscore: db 'Score:',0
;keyboard----------
boxPosition:dw 3920
scoree: dw 0

;timer------------
tickcount: dw 0
tickcount2:dw 0
tickcount3:dw 0
tickcount4:dw 0
tickcount5:dw 0

oldkb:dd 0
oldtimer: dd 0
assci:dw 65,66,67,68,69
colour:dw 3,1,5,6,7
location:dw 160,300,240,260,220
delay:dw 3,3,3,3,3
rand: dw 0
randnum: dw 0
lives:dw 0
scrcloc:dw 0

;-----------------------------------------------------------
clrscr: 
		 push es
		 push ax
		 push cx
		 push di
		 mov ax, 0xb800
		 mov es, ax ; point es to video base
		 xor di, di ; point di to top left column
		 mov ax, 0x0720 ; space char in normal attribute
		 mov cx, 2000 ; number of screen locations
		 cld ; auto increment mode
		 rep stosw ; clear the whole screen
		 pop di 
		 pop cx
		 pop ax
		 pop es
		 ret 





randG:

   	push bp
   	mov bp, sp
   	push ax
	push dx
	push cx
   	cmp word [rand], 0
   	jne next
  	MOV  AH, 00h   
  	INT  1AH
  	inc word [rand]
  	mov  [randnum], dx
  	jmp next1

  next:
  	mov     ax, 25173          
  	mul     word  [randnum]     
  	add     ax, 13849          
  	mov     [randnum], ax         

  next1:	
	xor dx, dx
 	mov ax, [randnum]
 	mov cx, [bp+4]
 	inc cx
 	div cx
 	mov [bp+6], dx
	pop cx
 	pop dx
	pop ax
 	pop bp
 	ret 2

;---------------------------------------------------------


printmiss: 
push bp
mov bp, sp
push es
push ax
push cx
push si
push di
mov cx, 8 ; save length in cx
mov ax, 0xb800
mov es, ax ; point es to video base
mov ax,0
mov di,ax ; point di to required location
mov si, strmiss ; point si to string
mov ah, 0x07 ; load attribute in ah
cld ; auto increment mode
nextchar: lodsb ; load next char in al
stosw ; print char/attribute pair
loop nextchar ; repeat for the whole string
pop di
pop si
pop cx
pop ax
pop es
pop bp
ret
printScore: 
push bp
mov bp, sp
push es
push ax
push cx
push si
push di
mov cx, 8 ; save length in cx
mov ax, 0xb800
mov es, ax ; point es to video base
mov ax,140
mov di,ax ; point di to required location
mov si, strscore ; point si to string
mov ah, 0x07 ; load attribute in ah
cld ; auto increment mode
nextchar1: lodsb ; load next char in al
stosw ; print char/attribute pair
loop nextchar1 ; repeat for the whole string
pop di
pop si
pop cx
pop ax
pop es
pop bp
ret


printnum: 
 		push bp
 		mov bp, sp
 		push es
 		push ax
 		push bx
 		push cx
 		push dx
 		push di
 		mov ax, 0xb800
 		mov es, ax  
 		mov ax, [bp+6] ; load number in ax
 		mov bx, 10 ; use base 10 for division
 		mov cx, 0 ; initialize count of digits

 nextdigit: 
		 mov dx, 0 ; zero upper half of dividend
		 div bx ; divide by 10
		 add dl, 0x30 ; convert digit into ascii value
		 push dx ; save ascii value on stack
		 inc cx ; increment count of values
		 cmp ax, 0 ; is the quotient zero
		 jnz nextdigit ; if no divide it again
		 mov di, [bp+4] ; point di to 70th column

 nextpos: 
		 pop dx ; remove a digit from the stack
		 mov dh, 0x07 ; use normal attribute
		 mov [es:di], dx ; print char on screen
		 add di, 2 ; move to next screen location
		 loop nextpos ; repeat for all digits on stack

		 pop di
		 pop dx
		 pop cx
		 pop bx
		 pop ax
		 pop es
		 pop bp
		 ret 4

;------------------------------------------------------------------------------------------
timer: 
      		pusha
		push es
      		mov ax,0xb800
      		mov es,ax
		mov cx,5
		mov si,0
	        checker:
			cmp word [cs:location+si],4000
			jb ter;locations
			mov dx,[cs:boxPosition]
			add dx,160
			cmp word[cs:location+si],dx
			jne missed
					jmp scored
			missed:
					inc word [cs:lives]
            scored:
			sub sp,2
			push 79
			call randG
			pop dx
			shl dx, 1
			add dx,160
			mov word[cs:location+si],dx

			 sub sp,2
			 push 9
			 call randG
			 pop dx
			 inc dx
			 mov word[cs:colour+si],dx

			  sub sp,2
			  push 5		;delay
			  call randG
			  pop dx
			  add dx,5
			  mov word[cs:delay+si],dx

			  sub sp,2
			  push 25
			  mov dx,90
			  call randG 
			  pop ax
			  sub dx,ax
			  mov word[cs:assci+si],dx
          ter:
			add si,2
		  loop checker

	  		mov dx,[cs:delay]
      		cmp word [cs:tickcount],dx
      		jbe next2;
	  		mov si,[cs:location]
	  		mov word[cs:tickcount],0
	  		;mov[cs:delay+4],dx
      		mov ah,[cs:colour]
      		mov al,[cs:assci]
      		mov word [es:si],0x720
      		add si,160
	  		mov[cs:location],si
      		mov [es:si],ax


      next2:

	  		mov dx,[cs:delay+2]
	  		cmp word [cs:tickcount2],dx
      		jbe next3;
	  		mov si,[cs:location+2]
	  		mov word[cs:tickcount2],0
	  		;mov[cs:delay+2],dx
      		mov ah,[cs:colour+2]
      		mov al,[cs:assci+2]
      		mov word [es:si],0x720
      		add si,160
	  		mov[cs:location+2],si
      		mov [es:si],ax


	  next3:

	  		mov dx,[cs:delay+4]
	  		cmp word [cs:tickcount3],dx
      		jbe next4;
	  		mov si,[cs:location+4]
	  		mov word[cs:tickcount3],0
	  		;mov[cs:delay+2],dx
      		mov ah,[cs:colour+4]
      		mov al,[cs:assci+4]
      		mov word [es:si],0x0720
      		add si,160
	  		mov[cs:location+4],si
      		mov [es:si],ax


	  next4:

	  		mov dx,[cs:delay+6]
	  		cmp word [cs:tickcount4],dx
      		jbe next5;
	  		mov si,[cs:location+6]
	  		mov word[cs:tickcount4],0
	  		;mov[cs:delay+2],dx
      		mov ah,[cs:colour+6]
      		mov al,[cs:assci+6]
      		mov word [es:si],0x720
      		add si,160
	  		mov[cs:location+6],si
      		mov [es:si],ax

	  next5:

	  		mov dx,[cs:delay+8]
	  		cmp word [cs:tickcount5],dx
      		jbe exit;
	  		mov si,[cs:location+8]
	  		mov word[cs:tickcount5],0
	  		;mov[cs:delay+2],dx
      		mov ah,[cs:colour+8]
      		mov al,[cs:assci+8]
      		mov word [es:si],0x720
      		add si,160
	  		mov[cs:location+8],si
      		mov [es:si],ax

mov cx,5
	mov si,0
	scorechecker3:
		mov di,[cs:boxPosition]
		sub di,160
		cmp di,[cs:location+si]
		jne cont3
			inc word[cs:scoree]
		cont3:
		add si,2
		loop scorechecker3

exit:
			
     		 	 inc word[cs:tickcount]			; increment tick count
	 		 inc word[cs:tickcount2]
	 		 inc word[cs:tickcount3]
	 		 inc word[cs:tickcount4]
	 		 inc word[cs:tickcount5]
     		 push word [cs:scoree]
		 push 154
	         call printnum
		 push word [cs:lives]
		 push 14
	         call printnum
     		 mov di,[cs:boxPosition]
	         mov word [es:di],0x03DC
     		 mov al, 0x20
     		 out 0x20, al ; end of interrupt
     		 pop es
		 popa
     		 iret ; return from interrupt

;------------------------------------------------------------------------------------

kbisr: 
cld
			push ax
 			push es
 			mov ax, 0xb800
 			mov es, ax ; point es to video memory
			in al,0x60
			cmp al,0x4B
			jne nextcmp
			cmp word[cs:boxPosition],3840
			je exitisr
			mov word [es:di],0x0720
			sub di,2
			mov word [cs:boxPosition],di
			mov cx,5
			mov si,0
			scorechecker1:
		mov di,[cs:boxPosition]
		sub di,160
		cmp di,[cs:location+si]
		jne cont1
			inc word[cs:scoree]
		cont1:
		add si,2
		loop scorechecker1

nextcmp: 
			cmp al, 0x4D ; is the key right shift
 			jne nomatch ; no, leave interrupt routine
			cmp word[cs:boxPosition],3998
			je exitisr
			mov word [es:di],0x0720
			add di,2
			mov word [cs:boxPosition],di
			mov cx,5
		mov si,0
		scorechecker:
		mov di,[cs:boxPosition]
		sub di,160
		cmp di,[cs:location+si]
		jne cont
			inc word[cs:scoree]
		cont:
		add si,2
		loop scorechecker

nomatch:
	    		mov di,[cs:boxPosition]
			mov word [es:di],0x03DC
exitisr:
 			mov al, 0x20
 			out 0x20, al
 			pop es
 			pop ax
			cmp word [cs:lives],10
			jb r

 			jmp far [cs:oldkb] ; call the original ISR
		
			r:
			iret

start:
 		call clrscr
		mov cx,5
		mov si,0
		initialize:
			sub sp,2
			push 79
			call randG
			pop dx
			shl dx, 1
			add dx,160
			mov word[cs:location+si],dx

			 sub sp,2
			 push 9
			 call randG
			 pop dx
			 inc dx
			 mov word[cs:colour+si],dx

			  sub sp,2
			  push 10		;delay
			  call randG
			  pop dx
			  add dx,1
			  mov word[cs:delay+si],dx

			  sub sp,2
			  push 25
			  mov dx,90
			  call randG 
			  pop ax
			  sub dx,ax
			  mov word[cs:assci+si],dx
			add si,2

		loop initialize
 		xor ax, ax
 		mov es, ax ; point es to IVT base
 		mov ax, [es:9*4]
 		mov [oldkb], ax ; save offset of old routine
 		mov ax, [es:9*4+2]
 		mov [oldkb+2], ax ; save segment of old routine

		xor ax, ax
 		mov es, ax ; point es to IVT base
 		mov ax, [es:8*4]
 		mov [oldtimer], ax ; save offset of old routine
 		mov ax, [es:8*4+2]
 		mov [oldtimer+2], ax ; save segment of old routine

 		cli ; disable interrupts

 		mov word [es:9*4], kbisr ; store offset at n*4
 		mov [es:9*4+2], cs ; store segment at n*4+2
		cmp word [cs:lives],10
 		mov word [es:8*4], timer ; store offset at n*4
 		mov [es:8*4+2], cs ; store segment at n*4+
 		sti ; enable interrupts
		call printmiss
		call printScore

		gameloops:
			cmp word[lives],10
			jne gameloops

		call clrscr


		xor ax, ax
 		mov es, ax ; point es to IVT base
		mov ax,[oldtimer] ; save offset of old routine
		mov [es:8*4],ax
		mov ax,[oldtimer+2] ; save offset of old routine
		mov [es:8*4+2],ax


		xor ax, ax
 		mov es, ax ; point es to IVT base
		mov ax,[oldkb]; save offset of old routine
		mov [es:9*4],ax
		mov  ax,[oldkb+2] ; save offset of old routine
		mov [es:9*4+2],ax

 		mov dx, start ; end of resident portion
 		add dx, 15 ; round up to next para
 		mov cl, 4
 		shr dx, cl ; number of paras
 		mov ax, 0x4c00 ; terminate and stay resident


 int 0x21