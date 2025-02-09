[org 0x0100]
jmp start

menu:
    mov ax, 30 
    push ax ; push x position 
    mov ax, 10 
    push ax ; push y position 
    mov ax, 2 ; green on black attribute 
    push ax ; push attribute 
    mov ax, start_message 
    push ax ; push address of message 
    call printstr
    mov ax,  110
    push ax ; push x position 
    mov ax, 11
    push ax ; push y position 
    mov ax, 4 ; red on black attribute 
    push ax ; push attribute 
    mov ax, end_message 
    push ax ; push address of message 
    call printstr
    ret

background:
mov cx,0
mov al,[sky_color]
mov ah,0x0C
mov dx,0

back_b:
int 10h
inc cx
cmp cx,320
jne back_b
mov cx,0
inc dx
cmp dx,150
jne back_b
mov cx,0
mov al,[grass_color]
mov ah,0x0C

back_g:
int 10h
inc cx
cmp cx,320
jne back_g
mov cx,0
inc dx
cmp dx,200
jne back_g
ret

pillar_up:
mov si,10
mov di,0
push bp
mov cx,[pipe_pos+di]
mov bp,sp
mov al,[bp+4]
mov ah,0x0C
mov dx,0

line_h:
int 10h
inc cx
dec si
jnz line_h
inc dx
mov si,10
mov cx,[pipe_pos+di]
cmp dx,[pipe_p1+di]
jne line_h
mov dx,0
add di,2
cmp di,4
jne line_h
pop bp
ret 2

pillar_down:
mov si,10
mov di,0
push bp
mov cx,[pipe_pos+di]
mov bp,sp
mov al,[bp+4]
mov ah,0x0C
mov dx,[pipe_p2+di]

line_h_d:
int 10h
inc cx
dec si
jnz line_h_d
inc dx
mov si,10
mov cx,[pipe_pos+di]
cmp dx,150
jne line_h_d
add di,2
mov dx,[pipe_p2+di]
cmp di,4
jne line_h_d
pop bp
ret 2

chota_don:
            xor ax, ax
            mov al, [score]
            add al, 0x30
            mov [text_score], al
            ret

elvish_bhai:    

            pusha
            mov ah, 2
            mov bh, 0
       
            mov dh, 23
            mov dl, 19
            int 0x10 
            mov ah, 9
            mov dx, text_score
            int 0x21

            popa
            ret

marks:
mov di,0

wow:
mov ax,50
mov bx,[pipe_pos+di]
cmp bx,45
jne just_looking
inc word[score]

just_looking:
add di ,2
cmp di , 4
jne wow
ret

end: 
popa
ret

game_over:
mov ah,2
mov bh,0
mov dh,10 ;row number
mov dl,15 ;column number
int 0x10

mov ah,9
mov dx,game_ovr_var
int 0x21

mov ah,2
mov bh,0
mov dh,12 ;row number
mov dl,18 ;column number
int 0x10

mov ah,9
mov dx,score
int 0x21

ret

make_bird:
;Bird code
push bp
mov bp, sp

mov al, [bp+4]  ; color of bird
mov ah,0x0C

mov di,[bird_row]
add di, 5

mov si,10
mov cx,50;column number
mov dx,[bird_row] ;row number
for_bird:
int 10h
inc cx
dec si
jnz for_bird
mov si,10
mov cx, 50
int 0x10
inc dx
cmp dx, di
jne for_bird
pop bp
ret 2

rand:
mov word [game_active],0
jmp bird_move_exit

bird_move:
pusha

cmp word [bird_row],4   ;top touch
jle rand

cmp word [bird_row],145 ;bottom touch
jge rand
    
;mov si, [bird_row]; save old pos of bird
mov ah, 1
int 16h
jz moveUp
mov cx,3
mov ah,0
int 16h
cmp al,0x1B
cmp al,'j'
jne moveDown

moveUp:
push word [sky_color] ; erase the old bird
call make_bird
mov ax, [gravity]
add word [bird_row], ax     ;down jump
push word [bird_color]
call make_bird
jmp bird_move_exit

moveDown:
push word [sky_color] ; erase the old bird
call make_bird
sub word [bird_row], 4  ;up jump
push word [bird_color]
call make_bird

bird_move_exit:
popa
ret

rand2:
ret

game:
call waj_gia
call baigan
mov ah, 0x2C
int 0x21
cmp dl, [time_var]
je game

mov byte [time_var], dl
            
push word[sky_color]
call pillar_up
push word[sky_color]
call pillar_down
mov di,0

call nan_sense
array_sub:
dec word[pipe_pos+di]
add di,2
cmp di,4
jne array_sub
push word[pipe_col]
call pillar_up
push word[pipe_col]
call pillar_down
call marks
call elvish_bhai
call chota_don

pusha
call bird_move
popa
cmp word [game_active],0
je rand2
jmp game

ret

baigan:  pusha
            xor cx, cx
            xor dx, dx
            xor ax, ax
            mov di, 0
            mov ah, 0x0D
            ayien:        mov cl, [x1+di]
                        mov dl, [x1+di+1]
                        int 0x10
                       
                        cmp al, 6
                        je khatam
                        add di, 2
                        cmp di, 8
                        jne ayien
                        jmp tata
            khatam:  push ax
                        mov ax, 0
                        mov word [game_active], ax
                        pop ax
            tata: popa
            ret
            
            waj_gia:     pusha
            mov al, 59
            mov ah, 0x0C
            mov cx,50
            dec cx
            push cx
            mov dx, [bird_row]
            dec dx
            mov byte [x1], cl ; mov x1
            mov byte [y1], dl ; mov y1
            ;int 0x10
            ; x2 y2
            mov cx, 50
            add cx, 10
            mov di, cx
            ; mov dx, [bird_row]
            ; dec dx
            mov byte [x2], cl ; mov x2
            mov byte [y2], dl ; mov y2
            ;int 0x10
            ; x3 y3
            pop cx
            mov dx, [bird_row]
            add dx, 4
            mov byte [x3], cl ; mov x3
            mov byte [y3], dl ; mov y3
            ;int 0x10
            mov cx, di
            mov byte [x4], cl ; mov x4
            mov byte [y4], dl ; mov y4
            ;int 0x10
            popa
            ret


clrscr: 
 push es 
 push ax 
 push cx 
 push di 
 mov ax, 0xb800 
 mov es, ax
 xor di, di
 mov ax, 0x0720
 mov cx, 2000 
 cld 
 rep stosw 
 pop di
 pop cx 
 pop ax 
 pop es 
 ret 


 printstr:
 push bp 
 mov bp, sp 
 push es 
 push ax 
 push cx 
 push si 
 push di 
 push ds 
 pop es
 mov di, [bp+4] 
 mov cx, 0xffff
 xor al, al 
 repne scasb 
 mov ax, 0xffff 
 sub ax, cx 
 dec ax 
 jz exit 
 mov cx, ax 
 mov ax, 0xb800 
 mov es, ax 
 mov al, 80 
 mul byte [bp+8] 
 add ax, [bp+10] 
 shl ax, 1 
 mov di,ax 
 mov si, [bp+4]
 mov ah, [bp+6]
 cld

nextchar: 
lodsb 
 stosw 
 loop nextchar 

exit: 
pop di 
 pop si 
 pop cx 
 pop ax 
 pop es 
 pop bp 
 ret 8 

 nan_sense:
 mov di,0
 mov ax,10
 cmp [pipe_pos],ax
 jne return
 call pipe_tur_ja
 mov ax,300
 mov [pipe_pos], ax
 return:
 mov ax,10
 cmp [pipe_pos+2],ax
 jne tur_ja
 mov ax,300
 mov [pipe_pos+2], ax
 call pipe_tur_ja
 tur_ja:
 ret

 pipe_tur_ja:

mov si,10
mov di,0
mov cx,[pipe_pos]
mov al,[sky_color]
mov ah,0x0C
mov dx,0

line_h_del:
int 10h
inc cx
dec si
jnz line_h_del
inc dx
mov si,10
mov cx,[pipe_pos]
cmp dx,150
jne line_h_del

 ret

start:
    call clrscr
    call menu
    mov ah,0
    int 0x16

    cmp al,0x1B
    je skip

    mov ah, 0x00
    mov al, 0x13
    int 0x10
    
    call background
    mov ax,[pipe_col]
    push ax

    call pillar_up
    mov ax,[pipe_col]
    push ax

    call pillar_down
    
    push word [bird_color]
    call make_bird
    call bird_move
    call game
    call game_over
        
    ; Your program's termination code goes here
termination: 
   mov eax, 0x01  ; system call number for exit
    xor ebx, ebx    ; exit code 0
    int 0x80        ; call kerne

    skip:
    call clrscr
    mov ax,0x4c00
    int 0x21

score: dw 0
text_score: dw '0','$' 
bird_row: dw 100
bird_color: dw 14
sky_color: dw 3
grass_color: dw 2
game_active: dw 1
game_ovr_var: db 'GAME OVER!!','$'
start_message: db 'START-ANY KEY', 0 
end_message: db 'QUIT-ESC', 0 
pipe_col: dw 6
pipe_pos: dw 150,300
pipe_p1: dw 50,80
pipe_p2: dw 100,130
time_var: dw 0
gravity: dw 1
x1: db 0
y1: db 0
x2: db 0
y2: db 0
x3: db 0
y3: db 0
x4: db 0
y4: db 0
