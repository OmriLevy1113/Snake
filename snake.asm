IDEAL
MODEL small
STACK 100h
DATASEG
snake_length dw 1 ; the starter length of the snake.
key dw 'a' ; first key.
line_position dw 40*2 ; starter head position in the line.
position_head dw (12*80+40)*2, 3999 dup(?); stars list.
CODESEG
; this procedure doesn't get anything.
; this procedure doesn't return anything.
; this procedure clear and change the screen to black.
proc clear
    push ax
	push bx
	push cx
    mov cx,25*80*2; number of times the loop runs.
    xor bx, bx
  	mov ah,0; color.
	mov al,' '; shape.
lop_clear:
	mov [es:bx],ax
	add bx, 2
	loop lop_clear
	pop cx
	pop bx
	pop ax
	ret 
endp clear
; this procedure gets: key [bp+4], head postion in line [bp+6], head position [bp+8].
; this procedure returns: the new head postion in line, the new key, the new head position.
; this procedure moves the snake left.
proc move_left
	push bp
	mov bp,sp
	push bx
	push ax
	push di
	push dx
	push si
	push cx
	mov di,[bp+4]
	cmp di,0 ; checks if the snake got to the border if he got there exit move left.
	je exit_move_left
	mov si,[bp+6]
	cmp si,'d'  ; checks if the the last key was d if  si = d exit move left.
	je exit_move_left
	mov si,'a'
	call delete_last_star ; delete last star.
	mov bx,[bp+10]
	mov cx,[bx]
	mov bx,[bp+8]
	mov ah,2 ; color.
	mov al,219 ; shape.
lop_left:
	add bx,2
	mov di,[bx]
	mov [es:di],ax
	cmp cx,0
	je exit_move_left
	loop lop_left
	mov bx,[bp+10]
	sub [word ptr bx],2
	mov di,[bx]
	mov [es:di],ax 
	mov di,[bp+4]
	sub di,2
exit_move_left:
	mov [bp+4],di ; move the new head location in line to the stack segment.
	mov [bp+6],si ; move the new key to the stack segment.
	mov [bp+8],bx ; move the new head location to the stack segment.
	pop cx
	pop si
	pop dx
	pop di
	pop ax
	pop bx
	pop bp
	ret 2
endp move_left
; this procedure gets: key [bp+4], head postion in line, [bp+6] head position [bp+8].
; this procedure returns: the new head postion in line, the new key, the new head position.
; this procedure moves the snake right.
proc move_right
	push bp
	mov bp,sp
	push bx
	push ax
	push di
	push dx
	push si
	push cx
	mov di,[bp+4]
	cmp di,2*80-2 ; checks if the snake got to the border if he got there exit move right.
	je exit_move_right
	mov si,[bp+6]
	cmp si,'a' ; checks if the the last key was a if  si = a exit move right.
	je exit_move_right
	mov si,'d'
call delete_last_star ; delete last star.
	mov bx,[bp+10]
	mov cx,[bx]
	mov bx,[bp+8]
	mov ah,2 ; color.
	mov al,219 ; shape.
lop_right:
	add bx,2
	mov di,[bx]
	mov [es:di],ax
	cmp cx,0
	je exit_move_right
	loop lop_right
	mov bx,[bp+10]
	add [word ptr bx],2
	mov di,[bx]
	mov [es:di],ax 
	mov di,[bp+4]
	add di,2
exit_move_right:
	mov [bp+4],di ; move the new head location in line to the stack segment.
	mov [bp+6],si ; move the new key to the stack segment.
	mov [bp+8],bx ; move the new head location to the stack segment.
	pop cx
	pop si
	pop dx
	pop di
	pop ax
	pop bx
	pop bp
	ret 2
endp move_right
; this procedure gets: key [bp+4], head position [bp+6].
; this procedure returns:the new key, the new head position.
; this procedure moves the snake down.
proc move_down
	push bp
	mov bp,sp
	push bx
	push ax
	push di
	push dx
	push si
	push cx
	mov si,[bp+4]
	cmp si,'w' ; checks if the the last key was w if  si = w exit move down.
	je exit_move_down
	mov si,'s'
	mov bx,[bp+6]
	cmp [word ptr bx],2*25*80-162 ; checks if the snake got to the border if he got there exit move down.
	ja exit_move_down
	call delete_last_star ; delete last star.
	mov bx,[bp+8]
	mov cx,[bx]
	mov bx,[bp+6]
	mov ah,2 ; color.
	mov al,219 ; shape.
lop_down:
	add bx,2
	mov di,[bx]
	mov [es:di],ax
	cmp cx,0
	je exit_move_down
	loop lop_down	
	mov bx,[bp+6]
	add [word ptr bx],80*2 
	mov di,[bx]
	mov [es:di],ax 
exit_move_down:
	mov [bp+4],si ; move the new key to the stack segment.
	mov [bp+6],bx ; move the new head location to the stack segment.
	pop cx
	pop si
	pop dx
	pop di
	pop ax
	pop bx
	pop bp
	ret 2
endp move_down
; this procedure gets: key [bp+4], head position [bp+6].
; this procedure returns: the new key, the new head position.
; this procedure moves the snake up.
proc move_up
	push bp
	mov bp,sp
	push bx
	push ax
	push di
	push dx
	push si
	push cx
	mov si,[bp+4]
	cmp si,'s' ; checks if the the last key was s if  si = s exit move up.
	je exit_move_up
	mov si,'w'
	mov bx,[bp+6]
	cmp [word ptr bx],79*2 ; checks if the snake got to the border if he got there exit move up.
	jna exit_move_up
	call delete_last_star ; delete last star.
	mov bx,[bp+8]
	mov cx,[bx]
	mov bx,[bp+6]
	mov ah,2 ; color.
	mov al,219 ; shape.
lop_up:
	add bx,2
	mov di,[bx]
	mov [es:di],ax
	cmp cx,0
	je exit_move_up
	loop lop_up	
	mov bx,[bp+6]
	sub [word ptr bx],80*2 
	mov di,[bx]
	mov [es:di],ax 
exit_move_up:
	mov [bp+4],si ; move the new key to the stack segment.
	mov [bp+6],bx ; move the new head location to the stack segment.
	pop cx
	pop si
	pop dx
	pop di
	pop ax
	pop bx
	pop bp
	ret 2
endp move_up
;	this procedure doesn't get anything.
;	this procedure doesn't return anything.
;	this procedure draw the snake on the screen.
proc draw_snake
	push ax
	push di
	mov ah,2 ; color.
	mov al,219 ; shape.
	mov di,(12*80+40)*2; first star location.
	mov [es:di],ax
	pop di
	pop ax
	ret
endp draw_snake
;	this procedure gets: the snake head [bp+4], and the previous apple location [bp+6].	
;	this procedure returns: the new apple location.
;	this procedure draw a random apple on the screen.
proc generate_random_apple
	push bp
	mov bp,sp
	push ax
	push es
	push bx
	push si
	push dx
	mov si,[bp+4] ; the head location.
	mov dx,[bp+6] ; the random number.
	cmp [word ptr si],dx ; checks if the head is in the apple location if he is generate new apple.
	jnz exit_generate_random_apple
return_random_apple:
	mov ax, 40h
	mov es, ax
	mov ax, [es:6Ch]
	and ax,11111101000b ; 2024 numbers rate.
	mov bx,ax
	mov ax,0b800h
	mov es,ax
	mov ah,2 ; color.
	mov al,149 ; shape.
	shl bx,1 ; multiple the number by 2.
	cmp bx,dx ; if the apple is in the same location as last one generate new number.
	jz return_random_apple
	cmp bx,(12*80+40)*4; if the number is above 4000 go back to return_random_apple.
	ja return_random_apple
	mov [es:bx],ax
	mov dx,bx
	inc [snake_length]
	mov [bp+6],dx ; move the random number to the stack segment.
exit_generate_random_apple:
	pop dx
	pop si
	pop bx
	pop es
	pop ax
	pop bp
	ret 2
endp generate_random_apple
;	this procedure doesn't get anything.
;	this procedure doesn't return anything.
;   this procedure draw the first apple.
proc generate_first_apple
	push ax
	push bx
	mov ah,2 ; color.
	mov al,149 ; shape.
	mov bx,((12*80+40)*2)-20; first apple location.
	mov [es:bx],ax
	pop bx
	pop ax
	ret
endp generate_first_apple
;	this procedure doesn't get anything.
;	this procedure doesn't return anything.
;	this procedure create a delay.
proc delay   
	push cx
delay_rep:
    push cx  
    mov cx, 06111H ; loop times
delay_dec:
    dec cx 
    jnz delay_dec
    pop cx
    dec cx
    jnz delay_rep
	pop cx
    ret
endp delay
proc delete_last_star
	push bp
	mov bp,sp
	push bx
	push cx
	push ax
	push di
	mov si,[snake_length]
	mov bx,[position_head]
	shl si,1
	add di, bx
	add di, si
	mov ah,0
	mov al, ' '
	mov [es:di],ax
	pop di
	pop ax
	pop cx
	pop bx
	pop bp
	ret
	endp delete_last_star
start:
	mov ax, @data
	mov ds, ax
;-----------------------------------main-----------------------------
	mov ax,0b800h
	mov es,ax
	call clear ; clear the screen.
	call draw_snake ; draw the snake.
	call generate_first_apple ; draw the first apple.
	mov di,[line_position] ; di = 80.
	mov si,[key] ; si = a.
	mov dx,((12*80+40)*2)-20; first apple location.
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
	push dx ; pass by value.
	push bx ; pass by reference.
	call generate_random_apple
	pop dx ; the random number.
up: 
	cmp al,'w'
	jnz down
	push offset snake_length
	push bx ; pass by reference.
	push si ; pass by value.
	call move_up ; calls the procedure move up if al = w.
	pop si
	pop bx
down:
	cmp al,'s'
	jnz right
	push offset snake_length
	push bx ; pass by reference.
	push si ; pass by value.
	call move_down; calls the procedure move down if al = s.
	pop si
	pop bx
right:
	cmp al,'d'
	jnz left
	push offset snake_length
	push bx ; pass by reference.
	push si ; pass by value.
	push di ; pass by value.
	call move_right ;  calls the procedure move right if al = d.
	pop di
	pop si
	pop bx
left:
	cmp al,'a'
	jnz main_lop
	push offset snake_length
	push bx ; pass by reference.
	push si ; pass by value.
	push di ; pass by value.
	call move_left ; calls the procedure move left if al = a.
	pop di
	pop si
	pop bx
	jmp main_lop
; --------------------------------------------------------------------
exit:
	call clear
	mov ax, 4c00h
	int 21h
END start