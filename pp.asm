[org 0x0100]

jmp start
previous : dw 160
current : dw 140
connect : dw 0
d1 : dw 0
d2 : dw 0	
delay:
    pusha              
    mov cx, 0xFFFF      
.loop1:
    dec cx              
    jnz .loop1          
    popa                
    ret                

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

timer:
    push ax
    push es
    push di
	mov ax,0xb800
	mov es,ax

inc word[connect]
cmp word[connect],4
je moveball


end1:
    pop di
    pop es
    pop ax
    mov al, 0x20
    out 0x20, al
    iret 	


moveball:
mov di,[current]
mov word[es:di],0x072A
add di,162
mov word[current],di
mov word[connect],0
jmp end1


	
	
start:

   call clrscr

    xor ax, ax 
    mov es, ax ; point es to IVT base 
    cli ; disable interrupts 
    mov word [es:8*4] , timer ; store offset at n*4 
    mov word [es:8*4+2], cs ; store segment at n*4+2 
    sti ; enable interrupts 
 
    mov ax, 0x4c00 ; terminate and stay resident
int 0x21