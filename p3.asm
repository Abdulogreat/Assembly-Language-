[org 0x0100]

jmp mainchoice 

pre: dw 0
upper : db "-----"
lower : db "-----"
space : db"     "
previous1 : dw 376
previous2 : dw 3694
previous : dw 3534
lrboundary : db "#"
udboundary : db "*"
ball : db "o"
danda : db "-"
interval : dw 0
status: dw 1
scmsj1: db 'p1 score is:'
scmsj2: db 'p2 score is:'
score1: dw 0
score2 : dw 0
msj1 : db 'p1 win'
msj2 : db 'p2 win'
u1 : dw 1
u2 : dw 0
d1 : dw 0
d2 : dw 0





maze db "***************************************************************************", 0
     db "#                                                                         #", 0
     db "#                                                                         #", 0
     db "#                                                                         #", 0
     db "#                                                                         #", 0
     db "#                                                                         #", 0
     db "#                                                                         #", 0
     db "#                                                                         #", 0
     db "#                                                                         #", 0
     db "#                                                                         #", 0
     db "#                                                                         #", 0
     db "#                                                                         #", 0
     db "#                                                                         #", 0
     db "#                                                                         #", 0
     db "#                                                                         #", 0
     db "#                                                                         #", 0
     db "#                                                                         #", 0
     db "#                                                                         #", 0
     db "#                                                                         #", 0
     db "#                                                                         #", 0
     db "#                                                                         #", 0
     db "#                                                                         #", 0
     db "#                                                                         #", 0 
     db "***************************************************************************", 0

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
    mov ax, [bp+4]
    mov bx, 10
    mov cx, 0

    ; Special case for 0
    cmp ax, 0
    je handle_zero

nextdigit:
    mov dx, 0
    div bx
    add dl, 0x30
    push dx
    inc cx
    cmp ax, 0
    jnz nextdigit

    jmp print_digits

handle_zero:
    push 0x30  ; ASCII '0'
    inc cx

print_digits:
    mov di, [bp+6]
nextpos:
    pop dx
    mov dh, 0x07
    mov [es:di], dx
    add di, 2
    loop nextpos

    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    pop es
    pop bp
    ret 4

clrscr:
    push es
    push ax
    push di
    mov ax,0xb800
    mov es,ax
    mov di,0

nextlocation:
    mov word[es:di],0x0720
    add di,2
    cmp di,4000
    jne nextlocation

    pop di
    pop ax
    pop es
    ret
draw_maze:
    push bp
    mov bp, sp
    push ax
    push cx
    push si
    push di
    push bx
    mov ax, 0xb800
    mov es, ax
    mov cx, 0
    mov si, [bp+4]
    mov di, 166
    mov word [pre], di
lop1:
    lodsb                 ; Load next byte from maze string
    cmp al, 0
    je nextl              ; End of string, move to the next line
    cmp al, ' '           ; Check for space character
    je transparent_space  ; Handle space as transparent
    mov ah, 0x07          ; Normal attribute (white on black)
    jmp draw_char
transparent_space:
    mov ah, 0x00          ; Transparent attribute (black on black)
draw_char:
    mov [es:di], ax       ; Write character with attribute
    add di, 2             ; Move to the next position
    jmp lop1
nextl:
    mov di, [pre]
    add di, 160           ; Move to the next line
    mov word [pre], di
    add cx, 1
    cmp cx, 25
    jne lop1
    pop bx
    pop di
    pop si
    pop cx
    pop ax
    pop bp
    ret


printscr:
    push bp
    mov bp,sp
    push ax
    push cx
    push si
    push di
    push bx
    mov di,[bp+4]
    mov ax,0xb800
    mov es,ax
    mov si,[bp+8]
    mov cx,[bp+6]
    mov ah,0x07
l1:
    mov al,[si]
    mov [es:di],ax
    add di,2
    inc si
    dec cx
    jnz l1
    pop bx
    pop di
    pop si
    pop cx
    pop ax
    pop bp
    ret 6

updatepos:
    push bp
    mov bp,sp
    push ax
    push cx
    push si
    push di
    push bx

    mov ax,0xb800
    mov es,ax
    mov di,[bp+6]
    mov bx,0x0723 
    cmp word[es:di],bx
    je e1
    mov si,di
    add si,10
    cmp word[es:si],bx
    je e1

    mov ax,space
    push ax
    mov ax,5
    push ax
    mov bx,[previous1]
    push bx
    call printscr

    mov ax,upper
    push ax
    mov ax,5
    push ax
    mov bx,di
    push bx
    call printscr

    mov word[previous1],di
e1:
    pop bx
    pop di
    pop si
    pop cx
    pop ax
    pop bp
    ret 4

updatepos2:
    push bp
    mov bp,sp
    push ax
    push cx
    push si
    push di
    push bx

    mov ax,0xb800
    mov es,ax
    mov di,[bp+6]
    mov bx,0x0723 
    cmp word[es:di],bx
    je e2
    mov si,di
    add si,10
    cmp word[es:si],bx
    je e2

    mov ax,space
    push ax
    mov ax,5
    push ax
    mov bx,[previous2]
    push bx
    call printscr

    mov ax,upper
    push ax
    mov ax,5
    push ax
    mov bx,di
    push bx
    call printscr

    mov word[previous2],di
e2:
    pop bx
    pop di
    pop si
    pop cx
    pop ax
    pop bp
    ret 4








timer:
    push ax
    push bx
    push cx
    push dx
    push es
    push di

    in1:
    inc word[interval]
    cmp word[interval],2
    jne eeee
    mov word[interval],0


    mov ax ,0xb800
    mov es,ax
    cmp word[u1],1
    je up1
    cmp word[u2],1
    je j1
    cmp word [d1],1
    je j2
    cmp word[d2],1
    je j3

     eeee :
     jmp end1

    up1:
    mov di,[previous]
    sub di, 158
    cmp word[es:di],0x0723 
    jne a1
    mov word[u1],0
    mov word[u2],1
    jmp in1
    a1:
    cmp word[es:di],0x072A
    jne  a2
    mov bl,[ball]
    mov bh,0x07
    mov di,[previous]
    mov word[es:di],0x0720
    mov di,[previous1]
    add di,160
    mov word [previous],di
    mov word[es:di],bx
    mov word[u1],0
    mov word [d2],1
    mov word[status],0
    inc word[score1]
    mov ax ,3522
    push ax
    mov ax,[score1]
    push ax
    call printnum

    jmp in1   
    a2:
    jmp a4
     
     j1:
     jmp up2
     j2:
     jmp down1
     j3 : 
     jmp j4


    a4:
    mov bl,[danda]
    mov bh,0x07
    cmp word[es:di],bx
    jne  a3
    mov word[u1],0
    mov word [d1],1
    mov word[status],0
    jmp in1

    a3:
     mov si,[previous]
    mov word[es:si],0x0720
     mov bl,[ball]
    mov bh,0x07
    mov word[es:di],bx
    mov word [previous],di
    jmp end1


    end1:
    jmp end2

    up2:
    mov di,[previous]
    sub di, 162
    cmp word[es:di],0x0723 
    jne b1
    mov word[u1],1
    mov word[u2],0
    jmp in1


    b1:
    cmp word[es:di],0x072A
    jne  b2
    mov bl,[ball]
    mov bh,0x07
    mov di,[previous]
    mov word[es:di],0x0720
    mov di,[previous1]
    add di,160
    mov word [previous],di
    mov word[es:di],bx
    mov word[u2],0
    mov word[d1],1
    mov word[status],0
    inc word[score1]
    mov ax ,3522
    push ax
    mov ax,[score1]
    push ax
    call printnum
    jmp in1
     
    b2:
    mov bl,[danda]
    mov bh,0x07
    cmp word[es:di],bx
    jne  b3
    mov word[u2],0
    mov word[d2],1
    mov word[status],0
    jmp in1

    b3:
    mov si,[previous]
    mov word[es:si],0x0720
    mov bl,[ball]
    mov bh,0x07
    mov word[es:di],bx
     mov word [previous],di
    jmp end1



    down1:
    mov di,[previous]
    add di, 162
    cmp word[es:di],0x0723 
    jne c1
    mov word[d1],0
    mov word[d2],1
    jmp in1


    c1:
    cmp word[es:di],0x072A
    jne  c2
    mov bl,[ball]
    mov bh,0x07
    mov di,[previous]
    mov word[es:di],0x0720
    mov di,[previous2]
    sub di,160
    mov word [previous],di
    mov word[es:di],bx
    mov word[u1],1
    mov word[d1],0
    mov word[status],1
    inc word[score2]
    mov ax ,162
    push ax
    mov ax,[score2]
    push ax
    call printnum
    jmp in1
     
    c2:
    jmp c4
    j4:
    jmp down2

    c4:
    mov bl,[danda]
    mov bh,0x07
    cmp word[es:di],bx
    jne  c3
    mov word[u1],1
    mov word[d1],0
    mov word[status],1
    jmp in1

    c3:
    mov si,[previous]
    mov word[es:si],0x0720
    mov bl,[ball]
    mov bh,0x07
    mov word[es:di],bx
     mov word [previous],di
    jmp end1






    down2:
    mov di,[previous]
    add di, 158
    cmp word[es:di],0x0723 
    jne k1
    mov word[d1],1
    mov word[d2],0
    jmp in1


    k1:
    cmp word[es:di],0x072A
    jne  k2
    mov bl,[ball]
    mov bh,0x07
    mov di,[previous]
    mov word[es:di],0x0720
    mov di,[previous2]
    sub di,160
    mov word [previous],di
    mov word[es:di],bx
    mov word[u1],1
    mov word[d2],0
    mov word[status],1
     inc word[score2]
    mov ax ,162
    push ax
    mov ax,[score2]
    push ax
    call printnum
    jmp in1
     
    k2:
    mov bl,[danda]
    mov bh,0x07
    cmp word[es:di],bx
    jne  k3
    mov word[u2],1
    mov word[d2],0
   mov word[status],1
    jmp in1

    k3:
    mov si,[previous]
    mov word[es:si],0x0720
    mov bl,[ball]
    mov bh,0x07
    mov word[es:di],bx
     mov word [previous],di
    jmp end2


    end2:
    pop di
    pop es
    pop dx
    pop cx
    pop bx
    pop ax

    mov al, 0x20             ; End-Of-Interrupt (EOI) signal
    out 0x20, al
    iret                     ; Return from interrupt

mainchoice:
    call clrscr
    mov ax,maze
    push ax
    call draw_maze

    mov ax,upper
    push ax
    mov ax,5
    push ax
    mov bx,376
    push bx
    call printscr

    mov ax,lower
    push ax
    mov ax,5
    push ax
    mov bx,3694
    push bx
    call printscr

    mov ax,0xb800
    mov es,ax
    mov di,3534
    mov bl, [ball]
    mov bh,0x07

mov word[es:di],bx
mov ax ,162
    push ax
    mov ax,[score1]
    push ax
    call printnum


mov ax ,3522
    push ax
    mov ax,[score2]
    push ax
    call printnum

    xor ax, ax
    mov es, ax   
    cli                       
    mov word [es:8*4], timer 
    mov [es:8*4+2], cs
    sti 



loopp:
    mov ah, 0
    cmp word[score1],5
    je player1
    cmp word[score2],5
    je player2

    int 0x16
    cmp ah, 0x4B ; Left arrow key
    je move_left
    cmp ah, 0x4D ; Right arrow key
    je move_right

move_right:
    cmp word[status],0
    je p1

    mov cx, [previous1]
    add cx,2
    push cx
    mov ax, lower
    push ax
    call updatepos 

    jmp loopp
p1:
    mov cx, [previous2]
    add cx,2
    push cx
    mov ax, upper
    push ax
    call updatepos2 

    jmp loopp

move_left:
    cmp word[status],0
    je p2
    mov cx, [previous1]
    sub cx,2
    push cx
    mov ax, lower
    push ax
    call updatepos
    jmp loopp
p2:
    mov cx, [previous2]
    sub cx,2
    push cx
    mov ax, upper
    push ax
    call updatepos2

    jmp loopp

player1:
call clrscr
mov ax ,msj1
push ax
mov ax,6
push ax
mov bx , 1880
push bx
call printscr
jmp ending



player2:
call clrscr
mov ax ,msj2
push ax
mov ax,6
push ax
mov bx , 1880
push bx
call printscr

ending:
mov ax,0x4c00
int 0x21

