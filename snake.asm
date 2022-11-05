IDEAL
MODEL small
STACK 100h
DATASEG
	snake_length dw 1							; the starter length of the snake.
	key dw 'a' 									; first key.
	position_head dw (12*80+40)*2, 3999 dup(?)	; stars list.
CODESEG
;this procedure doesn't get anything.
;this procedure doesn't return anything.
;this procedure clear and change the screen to black.
proc clear
;;;;;;;;;;;;;;;;;;;;;;;
    push ax
	push bx
	push cx
;;;;;;;;;;;;;;;;;;;;;;;
    mov cx,25*80*2	;number of times the loop runs.
    xor bx,bx		; bx=0.
  	mov ah,0		;color.
	mov al,' '		;shape.
lop_clear:
	mov [es:bx],ax
	add bx, 2
	loop lop_clear
;;;;;;;;;;;;;;;;;;;;;;;
	pop cx
	pop bx
	pop ax
;;;;;;;;;;;;;;;;;;;;;;;
	ret 
endp clear
;this procedure gets:
;[bp+4] == offset of the snake head.
;this procedure doesn't return anything.
;this procedure moves the snake left.
proc move_left
;;;;;;;;;;;;;;;;;;;;;;;
	push bp
	mov bp,sp
	push bx
	push ax
	push si
	push cx
;;;;;;;;;;;;;;;;;;;;;;;
	push bx
	push offset snake_length
	call delete_last_star		; delete last star.
	mov ah,2 ; color.
	mov al,219 ; shape.
	mov bx,[bp+4]
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
;;;;;;;;;;;;;;;;;;;;;;;
	pop cx
	pop si
	pop ax
	pop bx
	pop bp
;;;;;;;;;;;;;;;;;;;;;;;
	ret 2
endp move_left
;this procedure gets:
;[bp+4] == offset of the snake head.
;this procedure doesn't return anything.
;this procedure moves the snake right.
proc move_right
;;;;;;;;;;;;;;;;;;;;;;;	
	push bp
	mov bp,sp
	push bx
	push ax
	push si
	push cx
;;;;;;;;;;;;;;;;;;;;;;;	
	push bx
	push offset snake_length
	call delete_last_star		; delete last star.
	mov bx,[bp+4]
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
;;;;;;;;;;;;;;;;;;;;;;;	
	pop cx
	pop si
	pop ax
	pop bx
	pop bp
;;;;;;;;;;;;;;;;;;;;;;;	
	ret 2
endp move_right
; this procedure gets:
;[bp+4] == offset of the snake head.
;this procedure doesn't return anything.
;this procedure moves the snake down.
proc move_down
;;;;;;;;;;;;;;;;;;;;;;;	
	push bp
	mov bp,sp
	push bx
	push ax
	push di
	push si
	push cx
;;;;;;;;;;;;;;;;;;;;;;;	
	mov bx,[bp+4]
	push bx
	push offset snake_length
	call delete_last_star			; delete last star.
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
;;;;;;;;;;;;;;;;;;;;;;;	
	pop cx
	pop si
	pop di
	pop ax
	pop bx
	pop bp
;;;;;;;;;;;;;;;;;;;;;;;	
	ret 2
endp move_down
;this procedure gets:
;[bp+4] == offset of the snake head.
;this procedure doesn't return anything.
;this procedure moves the snake up.
proc move_up
;;;;;;;;;;;;;;;;;;;;;;;	
	push bp
	mov bp,sp
	push bx
	push ax
	push di
	push si
	push cx
;;;;;;;;;;;;;;;;;;;;;;;	
	mov bx,[bp+4]
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
	mov bx,[bp+4]
	mov ah,2 ; color.
	mov al,219 ; shape.
	sub [word ptr bx],80*2 
	mov di,[bx]
	mov [es:di],ax 
;;;;;;;;;;;;;;;;;;;;;;;	
	pop cx
	pop si
	pop di
	pop ax
	pop bx
	pop bp
;;;;;;;;;;;;;;;;;;;;;;;	
	ret 2
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
    mov cx, 0F888H	; loop times.
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
;this procedure gets:
;[bp+4] == the key that was just pressed.
;[bp+6] == the last key that was pressed.
;this procedure returns:
;ax = the key that was pressed if its legal.
;this procedure checks if the key is legal.
proc check_if_the_key_is_legal
;;;;;;;;;;;;;;;;;;;;;;;
	push bp
	mov bp,sp
	push bx
	push ax
;;;;;;;;;;;;;;;;;;;;;;;
	mov bx,[bp+4]	; the key that was just pressed.
	mov ax,[bp+6]	; the last key that was pressed.
up_key:
	cmp bl,'w'		; checks if the key that was pressed is 'w'.
	jne down_key
	cmp al,'s'		; checks if the last key that was pressed == 's'.
	je down_key
	mov ax,'w'
down_key:
	cmp bl,'s'		; checks if the key that was pressed is 's'. 
	jne right_key
	cmp al,'w'		; checks if the last key that was pressed == 'w'.
	je right_key
	mov ax,'s'
right_key:
	cmp bl,'d'		; checks if the key that was pressed is 'd'. 
	jne left_key
	cmp al,'a'		; checks if the last key that was pressed == 'a'.
	je left_key
	mov ax,'d'
left_key:
	cmp bl,'a'		; checks if the key that was pressed is 'a'. 
	jne exit_key
	cmp al,'d'		; checks if the last key that was pressed == 'd'.
	je exit_key
	mov ax,'a'
exit_key:
	mov [bp+6],ax 	; enter the new key to the stack segment.
;;;;;;;;;;;;;;;;;;;;;;;
	pop ax
	pop bx
	pop bp
;;;;;;;;;;;;;;;;;;;;;;;
	ret 2
endp check_if_the_key_is_legal
;this procedure gets:
;[bp+4] ==  the key that was just pressed.
;[bp+6] == offset of the head location.
;this procedure returns:
; the key that was pressed and if the head reached the border it returns 'q'.
;this procedure checks the borders of the screen.
proc check_borders
;;;;;;;;;;;;;;;;;;;;;;;
	push bp
	mov bp,sp
	push bx
	push ax
	push dx
	push si
;;;;;;;;;;;;;;;;;;;;;;;
	mov bx,[bp+4]			; the key that was pressed.
	mov si,[bp+6]			; offset of the head location.
	xor dx,dx				; dx = 0.
border_up:
	cmp bl,'w'				; checks if the key that was pressed is 'w'.
	jne border_down
	cmp [word ptr si],158	; if the head reached the up border bx = q.
	ja border_down
	mov bx,'q'
border_down:
	cmp bl,'s'				; checks if the key that was pressed is 's'.
	jne border_right
	cmp [word ptr si],3840	; if the head reached the down border bx = q.
	jb border_right
	mov bx,'q'
border_right:
	cmp bl,'d'				; checks if the key that was pressed is 'd'.
	jne border_left
	mov ax,[si]
	mov si,160
	div si
	cmp dx,158				; if the head reached the right border bx = q.
	jne border_left
	mov bx,'q'	
border_left:	
	cmp bl,'a'				; checks if the key that was pressed is 'a'.
	jne exit_borders
	mov ax,[si]
	mov si,160
	div si
	cmp dx,0				; if the head reached the left border bx = q.
	jne exit_borders
	mov bx,'q'
exit_borders:
	mov [bp+6],bx			; moves bx into the stack segment.
;;;;;;;;;;;;;;;;;;;;;;;
	pop si
	pop dx
	pop ax
	pop bx
	pop bp
;;;;;;;;;;;;;;;;;;;;;;;
	ret 2
endp check_borders
start:
	mov ax, @data
	mov ds, ax
;-----------------------------------main-----------------------------------;
	mov ax,0b800h
	mov es,ax
	call clear 					; clear the screen.
	call draw_snake 			; draw the snake.
	call generate_first_apple	; draw the first apple.
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
	mov cx,ax
	push si
	push cx
	call check_if_the_key_is_legal
	pop cx
	push bx
	push ax
	call check_borders
	pop ax
up:
	cmp cl,'w'
	jnz down
	cmp al,'w'
	jnz down
	push offset position_head	; pass by reference.
	call move_up				; calls the procedure move up if al = w.
	mov si,'w'					; the new key.
down:
	cmp cl,'s'
	jne right
	cmp al,'s'
	jnz right
	push offset position_head	; pass by reference.
	call move_down				; calls the procedure move down if al = s.
	mov si,'s'					; the new key.
right:
	cmp cl,'d'
	jne left
	cmp al,'d'
	jnz left
	push offset position_head	; pass by reference.
	call move_right				; calls the procedure move right if al = d.
	mov si,'d'					; the new key.
left:
	cmp cl,'a'
	jne main_lop
	cmp al,'a'
	jnz main_lop
	push offset position_head	; pass by reference.
	call move_left				; calls the procedure move left if al = a.
	mov si,'a'					; the new key.
	jmp main_lop
; --------------------------------end main------------------------------------------;
exit:
	call clear					; clear the screen at the end.
	mov ax, 4c00h
	int 21h
END start