IDEAL
MODEL small
STACK 100h
DATASEG
	snake_length dw 1							; the starter length of the snake.
	key dw 'a' 									; first key.
	position_head dw (12*80+40)*2, 3999 dup(?)	; snake parts list.
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
    mov cx,25*80*2	; number of times the loop runs.
    xor bx,bx		; bx=0.
  	mov ah,0		; color = black.
	mov al,' '		; shape = space.
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
;[bp+6] == offset of the snake length.
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
	push di
;;;;;;;;;;;;;;;;;;;;;;;
	mov bx,[bp+4]
	mov di,[bp+6]
	push bx
	push di
	call delete_last_star		; delete last star.
	mov ah,2					; color = green.
	mov al,219					; shape.
	mov cx,[di]
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
	pop di
	pop cx
	pop si
	pop ax
	pop bx
	pop bp
;;;;;;;;;;;;;;;;;;;;;;;
	ret 4
endp move_left
;this procedure gets:
;[bp+4] == offset of the snake head.
;[bp+6] == offset of the snake length.
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
	push di
;;;;;;;;;;;;;;;;;;;;;;;	
	mov bx,[bp+4]
	mov di,[bp+6]
	push bx
	push di
	call delete_last_star		; delete last star.
	mov cx,[di]
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
	pop di
	pop cx
	pop si
	pop ax
	pop bx
	pop bp
;;;;;;;;;;;;;;;;;;;;;;;	
	ret 4
endp move_right
; this procedure gets:
;[bp+4] == offset of the snake head.
;[bp+6] == offset of the snake length.
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
	mov di,[bp+6]
	push bx
	push di
	call delete_last_star			; delete last star.
	mov cx,[di]
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
	ret 4
endp move_down
;this procedure gets:
;[bp+4] == offset of the snake head.
;[bp+6] == offset of the snake length.
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
	mov di,[bp+6]
	push bx
	push di
	call delete_last_star			; delete last star.
	mov cx,[di]
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
	ret 4
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
;[bp+8] == offset of snake length.
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
	push di
;;;;;;;;;;;;;;;;;;;;;;;	
	mov si,[bp+4]			; the head location.
	mov dx,[bp+6]			; the old random number.
	mov di,[bp+8]			; offset of snake length.
	cmp [word ptr si],dx	; checks if the head is in the apple location if he is generate new apple.
	jnz exit_generate_random_apple
	mov ax, 40h
	mov es, ax
	mov ax, [es:6Ch]
    mov bx, 42h     
    mul bx          ; mult the timer counter by 42h.
    mov dx, 0
    mov bx, 4000    ; the random is between 0 to 4000.
    div bx          ; div ax:bx.
    mov ax, dx      ; ax = the remainder.
    mov bx, 42h
    mul bx          ; mult the timer counter by 42h.
    mov dx, 0
    mov bx, 4000    ; the random is between 0 to 4000.
    div bx          ; div ax:bx.
    mov ax, dx      ; ax = the remainder.
    and ax, 0FFFEh  ; making the number a even number.
	mov bx,ax
	mov ax,0b800h
	mov es,ax
	mov ah,4				; color = red.
	mov al,149				; shape = apple.
	mov [es:bx],ax
	mov dx,bx
	inc [di]
exit_generate_random_apple:
	mov [bp+8],dx			; move the random number to the stack segment.
;;;;;;;;;;;;;;;;;;;;;;;	
	pop di
	pop dx
	pop si
	pop bx
	pop es
	pop ax
	pop bp
;;;;;;;;;;;;;;;;;;;;;;;
	ret 4
endp generate_random_apple
;this procedure doesn't get anything.
;this procedure doesn't return anything.
;this procedure draw the first apple.
proc generate_first_apple
;;;;;;;;;;;;;;;;;;;;;;;
	push ax
	push bx
;;;;;;;;;;;;;;;;;;;;;;;	
	mov ah,4					; color = red.
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
    mov cx, 06888H	; loop times.
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
;this procedure recieves:
;[bp+4] == the key that was just pressed.
;[bp+6] == the offset of the head location.
;this procedure returns:
;the key that was pressed and if the head reached the border it returns 'q'.
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
;this procedure gets:
;[bp+4] == offset of snake head.
;[bp+6] == offset of snake length.
;this procedure returns:
;ax = 'q' if the snake hit himself.
;this procedure checks if the snake it himself.
proc check_hit
;;;;;;;;;;;;;;;;;;;;;;;
	push bp
	mov bp,sp
	push bx
	push ax
	push cx
	push si
;;;;;;;;;;;;;;;;;;;;;;;
	mov bx,[bp+4]	; offset of snake head.
	mov si,[bp+6]	; offset of snake length.
	xor cx,cx
	add cx,[si]
	mov si,cx
	add si,cx
loop_check_hit:
	mov di,[bx+si]
	cmp [bx],di
	je continue_check_hit
	sub si,2
	loop loop_check_hit
	jmp end_check_hit
continue_check_hit:
	mov ax,'q'
end_check_hit:
	mov [bp+6],ax
;;;;;;;;;;;;;;;;;;;;;;;
	pop si
	pop cx
	pop ax
	pop bx
	pop bp
;;;;;;;;;;;;;;;;;;;;;;;
	ret 2
endp check_hit
;this procedure doesn't get anything.
;this procedure doesn't return anything.
;this procedure calls the starter procedures.
proc main_lop_start
	call clear 					; clear the screen.
	call draw_snake 			; draw the snake.
	call generate_first_apple	; draw the first apple.
	ret
endp main_lop_start
;this procedure gets:
;[bp+4] == offset of snake head.
;[bp+6] == last random number.
;[bp+8] == offset of snake length.
;[bp+10] == last key pressed.
;[bp+12] == the key that was just pressed.
;this procedure returns:
;the key that was just pressed.
;the key that was last pressed.
;the new random number.
;this procedure runs the procedures that needs to be done before moving the snake.
proc main_loop_continue
;;;;;;;;;;;;;;;;;;;;;;;
	push bp
	mov bp,sp
	push bx
	push dx
	push si
	push ax
	push di
;;;;;;;;;;;;;;;;;;;;;;;
	mov bx,[bp+4]
	mov dx,[bp+6]
	mov di,[bp+8]
	mov si,[bp+10]
	mov ax,[bp+12]
	call delay
	push di
	push dx						
	push bx	
	call generate_random_apple
	pop dx 						
	push si	
	push ax					
	call check_if_the_key_is_legal
	pop ax
	push bx
	push ax						
	call check_borders
	pop ax
	push di
	push bx
	call check_hit
	pop ax
	mov [bp+10],ax
	mov [bp+12],dx
;;;;;;;;;;;;;;;;;;;;;;;
	pop di
	pop ax
	pop si
	pop dx
	pop bx
	pop bp
;;;;;;;;;;;;;;;;;;;;;;;
	ret 6
endp main_loop_continue
start:
	mov ax, @data
	mov ds, ax
;-----------------------------------main-----------------------------------;
	mov ax,0b800h
	mov es,ax
	call main_lop_start
	mov si,[key] 				; si = a.
	mov dx,((12*80+40)*2)-20	; first apple location.
main_lop:
	mov ah,1
	int 16h
	jz main_lop_continue
	mov ah,0
	int 16h
main_lop_continue:
	cmp al,'q'
	je exit
	push ax
	push si
	push offset snake_length	; pass by reference.
	push dx						; pass by value.
	push offset position_head	; pass by reference.
	call main_loop_continue
	pop ax											
	pop dx						; the random number.
up:
	push offset snake_length	; pass by reference.
	push offset position_head	; pass by reference.
	cmp al,'w'
	jnz down
	call move_up				; calls the procedure move up if al = w.
	mov si,'w'					; the new key.
down:
	cmp al,'s'
	jnz right
	call move_down				; calls the procedure move down if al = s.
	mov si,'s'					; the new key.
right:
	cmp al,'d'
	jnz left
	call move_right				; calls the procedure move right if al = d.
	mov si,'d'					; the new key.
left:	
	cmp al,'a'
	jnz main_lop
	call move_left				; calls the procedure move left if al = a.
	mov si,'a'					; the new key.
	jmp main_lop
;--------------------------------end main------------------------------------------;
exit:
	call clear					; clear the screen at the end.
	mov ax,4c00h
	int 21h
END start