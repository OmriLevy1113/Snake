IDEAL
MODEL small
STACK 100h
DATASEG
	snake_length dw 1							; the starter length of the snake.
	key dw 'a' 									; first key.
	line_position dw 40*2						; starter head position in the line.
	position_head dw (12*80+40)*2, 3999 dup(?)	; stars list.
CODESEG
; this procedure doesn't get anything.
; this procedure doesn't return anything.
; this procedure clear and change the screen to black.
proc clear
    push ax
	push bx
	push cx
    mov cx,25*80*2	;number of times the loop runs.
    xor bx,bx		; bx=0.
  	mov ah,0		;color.
	mov al,' '		;shape.
lop_clear:
	mov [es:bx],ax
	add bx, 2
	loop lop_clear
	pop cx
	pop bx
	pop ax
	ret 
endp clear
;this procedure gets:
;[bp+4] == the head location in a line.
;[bp+6] == the key that was last pressed.
;[bp+8] == offset of the snake head.
;this procedure returns:
;the new head location in a line.
;the new key that was pressed.
;the new head position.
;this procedure moves the snake left.
proc move_left
;;;;;;;;;;;;;;;;;;;;;;;
	push bp
	mov bp,sp
	push bx
	push ax
	push di
	push dx
	push si
	push cx
;;;;;;;;;;;;;;;;;;;;;;;
	mov di,[bp+4]
	cmp di,0					; checks if the snake got to the border if he got there exit move left.
	je exit_move_left
	mov dx,[bp+6]
	cmp dx,'d'					; checks if the the last key was d if  si = d exit move left.
	je exit_move_left
	mov dx,'a'
	push bx
	push offset snake_length
	call delete_last_star		; delete last star.
	mov ah,2 ; color.
	mov al,219 ; shape.
	mov bx,[bp+8]
	mov cx,[snake_length]
	mov si,cx
	add si,cx
loop_move_left:
	mov di,[bx+si-2]
	mov [bx+si],di
	sub si,2
	loop loop_move_left
	sub [word ptr bx],2
	mov di,[bx]
	mov [es:di],ax 
	mov di,[bp+4]
	sub di,2
exit_move_left:
	mov [bp+4],di				; move the new head location in line to the stack segment.
	mov [bp+6],dx				; move the new key to the stack segment.
	mov [bp+8],bx				; move the new head location to the stack segment.
;;;;;;;;;;;;;;;;;;;;;;;
	pop cx
	pop si
	pop dx
	pop di
	pop ax
	pop bx
	pop bp
;;;;;;;;;;;;;;;;;;;;;;;
	ret 
endp move_left
;this procedure gets:
;[bp+4] == the head location in a line.
;[bp+6] == the key that was last pressed.
;[bp+8] == offset of the snake head.
;this procedure returns:
;the new head location in a line.
;the new key that was pressed.
;the new head position.
;this procedure moves the snake right.
proc move_right
;;;;;;;;;;;;;;;;;;;;;;;	
	push bp
	mov bp,sp
	push bx
	push ax
	push di
	push dx
	push si
	push cx
;;;;;;;;;;;;;;;;;;;;;;;	
	mov di,[bp+4]
	cmp di,2*80-2				; checks if the snake got to the border if he got there exit move right.
	je exit_move_right
	mov dx,[bp+6]
	cmp dx,'a'					; checks if the the last key was a if  si = a exit move right.
	je exit_move_right
	mov dx,'d'
	push bx
	push offset snake_length
	call delete_last_star		; delete last star.
	mov bx,[bp+8]
	mov cx,[snake_length]
	mov si,cx
	add si,cx
loop_move_right:
	mov di,[bx+si-2]
	mov [bx+si],di
	sub si,2
	loop loop_move_right
	mov ah,2 ; color.
	mov al,219 ; shape.
	add [word ptr bx],2
	mov di,[bx]
	mov [es:di],ax 
	mov di,[bp+4]
	add di,2
exit_move_right:
	mov [bp+4],di				; move the new head location in line to the stack segment.
	mov [bp+6],dx				; move the new key to the stack segment.
	mov [bp+8],bx				; move the new head location to the stack segment.
;;;;;;;;;;;;;;;;;;;;;;;	
	pop cx
	pop si
	pop dx
	pop di
	pop ax
	pop bx
	pop bp
;;;;;;;;;;;;;;;;;;;;;;;	
	ret 
endp move_right
; this procedure gets:
;[bp+4] == the key that was last pressed.
;[bp+6] == offset of the snake head.
;this procedure returns:
;the new key that was pressed.
;the new head position.
;this procedure moves the snake down.
proc move_down
;;;;;;;;;;;;;;;;;;;;;;;	
	push bp
	mov bp,sp
	push bx
	push ax
	push di
	push dx
	push si
	push cx
;;;;;;;;;;;;;;;;;;;;;;;	
	mov dx,[bp+4]
	cmp dx,'w'						; checks if the the last key was w if  si = w exit move down.
	je exit_move_down
	mov dx,'s'
	mov bx,[bp+6]
	cmp [word ptr bx],2*25*80-162	; checks if the snake got to the border if he got there exit move down.
	ja exit_move_down
	push bx
	push offset snake_length
	call delete_last_star			; delete last star.
	mov bx,[bp+6]
	mov cx,[snake_length]
	mov si,cx
	add si,cx
loop_move_down:
	mov di,[bx+si-2]
	mov [bx+si],di
	sub si,2
	loop loop_move_down
	mov ah,2 ; color.
	mov al,219 ; shape.
	add [word ptr bx],80*2 
	mov di,[bx]
	mov [es:di],ax 
exit_move_down:
	mov [bp+4],dx					; move the new key to the stack segment.
	mov [bp+6],bx					; move the new head location to the stack segment.
	mov [bp+6],bx					; move the new head location to the stack segment.
;;;;;;;;;;;;;;;;;;;;;;;	
	pop cx
	pop si
	pop dx
	pop di
	pop ax
	pop bx
	pop bp
;;;;;;;;;;;;;;;;;;;;;;;	
	ret 
endp move_down
;this procedure gets:
;[bp+4] == the key that was last pressed.
;[bp+6] == offset of the snake head.
;this procedure returns: 
;the new key that was pressed.
;the new head position.
;this procedure moves the snake up.
proc move_up
;;;;;;;;;;;;;;;;;;;;;;;	
	push bp
	mov bp,sp
	push bx
	push ax
	push di
	push dx
	push si
	push cx
;;;;;;;;;;;;;;;;;;;;;;;	
	mov dx,[bp+4]
	cmp dx,'s'						; checks if the the last key was s if  si = s exit move up.
	je exit_move_up
	mov dx,'w'
	mov bx,[bp+6]
	cmp [word ptr bx],79*2			; checks if the snake got to the border if he got there exit move up.
	jna exit_move_up
	push bx
	push offset snake_length
	call delete_last_star			; delete last star.
	mov cx,[snake_length]
	mov si,cx
	add si,cx
loop_move_up:
	mov di,[bx+si-2]
	mov [bx+si],di
	sub si,2
	loop loop_move_up
	mov bx,[bp+6]
	mov ah,2 ; color.
	mov al,219 ; shape.
	sub [word ptr bx],80*2 
	mov di,[bx]
	mov [es:di],ax 
exit_move_up:
	mov [bp+4],dx					; move the new key to the stack segment.
	mov [bp+6],bx					; move the new head location to the stack segment.
;;;;;;;;;;;;;;;;;;;;;;;	
	pop cx
	pop si
	pop dx
	pop di
	pop ax
	pop bx
	pop bp
;;;;;;;;;;;;;;;;;;;;;;;	
	ret 
endp move_up
;this procedure doesn't get anything.
;this procedure doesn't return anything.
;this procedure draw the snake on the screen.
proc draw_snake
;;;;;;;;;;;;;;;;;;;;;;;	
	push ax
	push di
;;;;;;;;;;;;;;;;;;;;;;;	
	mov ah,2				; color = green.
	mov al,219				; shape = rectangle.
	mov di,(12*80+40)*2		; first star location.
	mov [es:di],ax
;;;;;;;;;;;;;;;;;;;;;;;	
	pop di
	pop ax
;;;;;;;;;;;;;;;;;;;;;;;	
	ret
endp draw_snake
;this procedure gets:
;[bp+4] == offset of the snake head.
;[bp+6] == previous apple location.	
;this procedure returns: 
;the new apple location.
;this procedure draw a random apple on the screen.
proc generate_random_apple
;;;;;;;;;;;;;;;;;;;;;;;
	push bp
	mov bp,sp
	push ax
	push es
	push bx
	push si
	push dx
;;;;;;;;;;;;;;;;;;;;;;;	
	mov si,[bp+4]			; the head location.
	mov dx,[bp+6]			; the random number.
	cmp [word ptr si],dx	; checks if the head is in the apple location if he is generate new apple.
	jnz exit_generate_random_apple
return_random_apple:
	mov ax, 40h
	mov es, ax
	mov ax, [es:6Ch]
	and ax,11111101000b		; 2024 numbers rate.
	mov bx,ax
	mov ax,0b800h
	mov es,ax
	mov ah,2				; color = green.
	mov al,149				; shape = apple.
	shl bx,1				; multiple the number by 2.
	cmp bx,dx				; if the apple is in the same location as last one generate new number.
	jz return_random_apple
	cmp bx,(12*80+40)*4		; if the number is above 4000 go back to return_random_apple.
	ja return_random_apple
	mov [es:bx],ax
	mov dx,bx
	inc [snake_length]
	mov [bp+6],dx			; move the random number to the stack segment.
exit_generate_random_apple:
;;;;;;;;;;;;;;;;;;;;;;;	
	pop dx
	pop si
	pop bx
	pop es
	pop ax
	pop bp
;;;;;;;;;;;;;;;;;;;;;;;
	ret 2
endp generate_random_apple
;this procedure doesn't get anything.
;this procedure doesn't return anything.
;this procedure draw the first apple.
proc generate_first_apple
;;;;;;;;;;;;;;;;;;;;;;;
	push ax
	push bx
;;;;;;;;;;;;;;;;;;;;;;;	
	mov ah,2					; color = green.
	mov al,149					; shape = apple.
	mov bx,((12*80+40)*2)-20	; first apple location.
	mov [es:bx],ax
;;;;;;;;;;;;;;;;;;;;;;;	
	pop bx
	pop ax
;;;;;;;;;;;;;;;;;;;;;;;
	ret
endp generate_first_apple
;this procedure doesn't get anything.
;this procedure doesn't return anything.
;this procedure create a delay.
proc delay
;;;;;;;;;;;;;;;;;;;;;;;   
	push cx
;;;;;;;;;;;;;;;;;;;;;;;	
delay_rep:
    push cx  
    mov cx, 06111H	; loop times.
delay_dec:
    dec cx 
    jnz delay_dec
    pop cx
    dec cx
    jnz delay_rep
;;;;;;;;;;;;;;;;;;;;;;;	
	pop cx
;;;;;;;;;;;;;;;;;;;;;;;
    ret
endp delay
;this procedure gets:
;[bp+4] == offset of snake length.
;[bp+6] == offset of snake head location.
;this procedure dosen't return anything.
;this procedure delete the last star of the snake.
proc delete_last_star
;;;;;;;;;;;;;;;;;;;;;;;
	push bp
	mov bp,sp
	push bx
	push cx
	push ax
	push di
	push bx
;;;;;;;;;;;;;;;;;;;;;;;	
	mov si,[bp+4]		; offset snake length.
	mov di,[bp+6]		; offset snake head.
	mov bx,[si]			; bx = snake length.
	add di, bx			
	add di, bx			; di = snake length *2.
	mov di, [di]		; di = the value of the last star (tail).
	mov ah,0
	mov al,' '
	mov [es:di],ax		; delete tail.
;;;;;;;;;;;;;;;;;;;;;;;
	pop bx
	pop di
	pop ax
	pop cx
	pop bx
	pop bp
;;;;;;;;;;;;;;;;;;;;;;;
	ret 4
	endp delete_last_star
start:
	mov ax, @data
	mov ds, ax
;-----------------------------------main-----------------------------------;
	mov ax,0b800h
	mov es,ax
	call clear 					; clear the screen.
	call draw_snake 			; draw the snake.
	call generate_first_apple	; draw the first apple.
	mov di,[line_position]		; di = 80.
	mov si,[key] 				; si = a.
	mov dx,((12*80+40)*2)-20	; first apple location.
	mov bx,offset position_head
main_lop:
	 mov ah,1
	 int 16h
	 jz main_lop_continue
	mov ah,0
	int 16h
main_lop_continue:
	cmp al,'q'
	je exit
	call delay
	push dx						; pass by value.
	push bx						; pass by value.
	call generate_random_apple
	pop dx 						; the random number.
up: 
	cmp al,'w'
	jnz down
	push bx						; pass by value.
	push si						; pass by value.
	call move_up				; calls the procedure move up if al = w.
	pop si
	pop bx
down:
	cmp al,'s'
	jnz right
	push bx						; pass by value.
	push si 					; pass by value.
	call move_down				; calls the procedure move down if al = s.
	pop si
	pop bx
right:
	cmp al,'d'
	jnz left
	push bx						; pass by value.
	push si						; pass by value.
	push di						; pass by value.
	call move_right				; calls the procedure move right if al = d.
	pop di
	pop si
	pop bx
left:
	cmp al,'a'
	jnz main_lop
	push bx						; pass by value.
	push si						; pass by value.
	push di						; pass by value.
	call move_left				; calls the procedure move left if al = a.
	pop di
	pop si
	pop bx
	jmp main_lop
; --------------------------------end main------------------------------------------;
exit:
	call clear
	mov ax, 4c00h
	int 21h
END start