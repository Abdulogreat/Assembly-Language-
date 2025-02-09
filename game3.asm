[org 0x100]
jmp start

u1L : dw 80
u1R : dw 0
d1L : dw 3920
d1R : dw 0
previous : dw 1998
current : dw 2000
connect : dw 0
d1 : dw 0
d2 : dw 0	
sc : dw 0


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
 mov di,[u1L]
 mov cx,10
 sub di,2
 l1:
 add di,2
 mov word[es:di],0x772b
 loop l1
 mov [u1R],di
mov di,[d1L]
 mov cx,10
 sub di,2
 l2:
add di,2
 mov word[es:di],0x772b
 loop l2
 mov [d1R],di
 mov cx,25
mov di,0
l5:
mov ax,0x0023
stosw
add di,158
loop l5
mov cx,25
mov di,158
l6:
mov ax,0x0023
stosw
add di,158
loop l6
pop di
pop ax
pop es
ret



slides:
   push ax
   push es
   mov ax,0xb800
   mov es,ax
   in al,0x60
   cmp al,0x2a
   je fast1
   cmp al,0x36
   je slow1
jmp end2


fast1:
mov word[sc],0
jmp end2

slow1:
mov word[sc],1
jmp end2
   
   
   end2:
   mov al,0x20
   out 0x20,al
   pop es 
   pop ax
   iret
   
 
 
 






timer:
    push ax
    push es
    push di
	mov ax,0xb800
	mov es,ax

inc word[connect]
cmp word[sc],0
je fast
jne slow


fast:
cmp word[connect],2
je moveball


slow:
cmp word[connect],10
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
add di,160
cmp di,3840
jg moveup1
mov di,[current]
sub di,160
cmp di,160
jl movedown1
mov ax,[d1]
cmp ax,1
je moveup1
jmp movedown1


moveup1:
mov ax,[d2]
cmp ax,0
je moveupR1
mov ax,[d2]
cmp ax,1
je moveupL1


moveupL1:
mov di,[current]
sub di,2
cmp word[es:di],0x0023
je moveupR
jmp moveupL


moveupR1:
mov di,[current]
add di,2
cmp word[es:di],0x0023
je moveupL
jmp moveupR


movedown1:
mov ax,[d2]
cmp ax,0
je movedownR1
mov ax,[d2]
cmp ax,1
je movedownL1 


movedownL1:
mov di,[current]
sub di,2
cmp word[es:di],0x0023
je movedownR
jmp movedownL


movedownR1:
mov di,[current]
add di,2
cmp word[es:di],0x0023
je movedownL
jmp movedownR


moveupL:
mov word[d1],1
mov word[d2],1
mov ax,cs
mov ds,ax
mov di,[current]
mov ax,0x072A
stosw
mov di,[previous]
mov ax,0x0720
stosw
mov di,[current]
mov [previous],di
sub word[current],162

mov word[connect],0
jmp end1


movedownL:
mov word[d1],0
mov word[d2],1
mov ax,cs
mov ds,ax
mov di,[current]
mov ax,0x072A
stosw
mov di,[previous]
mov ax,0x0720
stosw
mov di,[current]
mov [previous],di
add word[current],158

mov word[connect],0
jmp end1


moveupR:
mov word[d1],1
mov word[d2],0
mov ax,cs
mov ds,ax
mov di,[current]
mov ax,0x072A
stosw
mov di,[previous]
mov ax,0x0720
stosw
mov di,[current]
mov [previous],di
sub word[current],158

mov word[connect],0
jmp end1


movedownR:
mov word[d1],0
mov word[d2],0
mov ax,cs
mov ds,ax
mov di,[current]
mov ax,0x072A
stosw
mov di,[previous]
mov ax,0x0720
stosw
mov di,[current]
mov [previous],di
add word[current],162

mov word[connect],0
jmp end1



start:
    call clrscr
	xor ax, ax 
    mov es, ax ; point es to IVT base 
    cli ; disable interrupts 
    mov word [es:9*4] , slides ; store offset at n*4 
    mov word [es:9*4+2], cs ; store segment at n*4+2 
    sti ; enable interrupts 
  xor ax, ax 
    mov es, ax ; point es to IVT base 
    cli ; disable interrupts 
    mov word [es:8*4] , timer ; store offset at n*4 
    mov word [es:8*4+2], cs ; store segment at n*4+2 
    sti ; enable interrupts 



mov ax,0x4c00
int 0x21