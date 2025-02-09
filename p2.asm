[org 0x100]


jmp start

;--------------------VARIABLES---------------------

; WINNER PRINTING ARRAY

p1win : dw "PlayerB wins$"

p2win : dw "PlayerA wins$"

; STORE DEFAULT TIMER

timerOffset: dw 0x0

timerSegment: dw 0x0

;TICK COUNT

tickCnt: dw 0x0


; STORE DEFAULT KEYPAD

keypadOffset: dw 0x0

keypadSegment: dw 0x0


; OFFSET VARIABLES
  
readupOff: dw 0x0

  
; HOOKER

ThookOff: dw 0x0
KhookOff: dw 0x0


; DIFF ARGUMENTS

timeSwitch: dw 2
paddleWidth: dw 16
ono: dw 0

; CHARS

paddlech: dw 0xdb

ballch: dw 0x09

; POSITION VARIABLES

paddleOne: dw 0x0

paddleTwo: dw 0x0


ball_x: dw 39
ball_y: dw 23

; BALL DIRECTION

ball_dy: dw 0x0
ball_dx: dw 0x0

; GAME SCORE

p1Score: dw 0
p2Score: dw 0

;  Hookersaver

oldkb : dd 0

oldtb : dd 0


;-------------------- CLEAR SCREEN --------------------------

clrscr:
    pusha                   ; Save registers
    mov ax, 0xB800          ; Load video memory segment
    mov es, ax              ; Set extra segment to video memory
    xor di, di              ; Start at offset 0 in video memory
    mov cx, 2000            ; 80x25 = 2000 characters to clear
    mov al, 0x20            ; Space character (ASCII 32)
    mov ah, 0x07            ; Attribute byte (white text on black background)

clear_loop:
    stosw                   ; Store AX (character + attribute) and increment DI
    loop clear_loop         ; Repeat 2000 times

    popa                    ; Restore registers
    ret                     ; Return to caller

;---------------------DELAY---------------------------

delay :
push bp
mov bp,sp
push cx
mov cx,[bp+4]
delay_loop1 :
push cx
mov cx, 0xFFFF
delay_loop2 :
	loop delay_loop2
	pop cx
	loop delay_loop1
	pop cx
    pop bp
	ret 2

;---------------------------- DISPLAY PRINT------------------------------------

print:

push bp
mov bp,sp

pusha

push cs
pop ds

mov ax,0xb800
mov es,ax

mov bx,[bp+6]
mov si,0x8

mov ax,[bp+4]

pl1:


mov cx,ax
and cx,0xf

shr ax,4

add cx,0x30

mov ch,0x57

sub si,2
mov [es:bx+si],cx

cmp si,0
jnz pl1

popa

pop bp


ret 4

;-------------------------------ABSOLUTE DELAY ----------------------------------


absDelay :
push cx
mov cx, 3 ; change the values to increase delay time
absdelay_loop1 :
push cx
mov cx, 0xFFFF
absdelay_loop2 :
	loop absdelay_loop2
	pop cx
	loop absdelay_loop1
	pop cx
	ret









;--------------------------------  CLEAR GAME PAD     -----------------------------------

clrGame:

pusha

xor di,di

mov ax,0xb800
mov es,ax

mov cx,2000

clg1:

cmp word [es:di],0xc611
jz clgskip1

cmp word [es:di],0xc610
jz clgskip1

cmp word [es:di],0x0711
jz clgskip1

cmp word [es:di],0x0710
jz clgskip1


and word [es:di],0xff00
or word [es:di],0x0020


clgskip1:

add di,2

loop clg1


popa

ret


;-------------------------- CLEAR BOARD PAD------------------------------------

clrBoard:

pusha

mov di,0
mov si,158

mov ax,0xb800
mov es,ax

mov cx,25

clb0:

mov word [es:di],0x0711

mov word [es:si],0x0710

add di,160

add si,160

loop clb0


xor di,di

mov cx,2000

clb1:

cmp word [es:di],0x0711
jz clbskip1

cmp word [es:di],0x0710
jz clbskip1


mov word[es:di],0x0720 


clbskip1:

add di,2

loop clb1


popa

ret

;-----------------------------  DRAW GAME PAD  -------------------------------

draw:


pusha
cli

;; //0X0FDB
;; //0x09

mov ax,0xb800
mov es,ax

xor di,di
xor si,si

mov di,[paddleOne]
shl di,1
add di,3840

mov si,[paddleTwo]
shl si,1
add di,0

mov cx,[paddleWidth]

drwp:

mov ax,[es:di]
and ax,0x7f00
or ax,[paddlech]

mov word [es:di],ax


mov ax,[es:si]
and ax,0x7f00
or ax,[paddlech]

mov word [es:si],ax

add di,2
add si,2

loop drwp

mov ax,[ball_y]
mov dx,0x0
mov bx,0xa0
mul bx
mov di,ax
mov si,[ball_x]
shl si,1
add di,si


mov ax,[es:di]
and ax,0x7f00
or ax,[ballch]

mov word [es:di],ax



sti
popa


ret
;*********************spawn************************
spawn:
    pusha

    ; Initialize paddle positions directly without animation
    mov bx, 80               ; Screen width divided by 2 (assuming 80 columns total)
    sub bx, [paddleWidth]       ; Center the paddle horizontally
    shr bx, 1                   ; Divide by 2 to account for the paddle width

    mov word [paddleOne], bx    ; Centered on the first row
    mov word [paddleTwo], bx    ; Centered on the last row

    xor di, di

    mov ax, 0xb800
    mov es, ax

    ; Clear the game screen
    call clrGame

    ; Draw paddleOne (First row)
    mov di, [paddleOne]
    shl di, 1                   ; Convert to word offset
    add di, 0                   ; First row offset (row 0)

    mov cx, [paddleWidth]

drawPaddleOne:
    cmp word [es:di], 0xc611
    jz skipPaddleOneDraw

    cmp word [es:di], 0xc610
    jz skipPaddleOneDraw

    push ax

    mov ax, [es:di]
    and ax, 0x7f00
    or ax, 0x8000
    or ax, [paddlech]

    mov word [es:di], ax

    pop ax

skipPaddleOneDraw:
    add di, 2
    loop drawPaddleOne

    ; Draw paddleTwo (Last row)
    mov di, [paddleTwo]
    shl di, 1                   ; Convert to word offset
    add di, 3840                ; Last row offset (row 24, assuming 25 rows total)

    mov cx, [paddleWidth]

drawPaddleTwo:
    cmp word [es:di], 0xc611
    jz skipPaddleTwoDraw

    cmp word [es:di], 0xc610
    jz skipPaddleTwoDraw

    push ax

    mov ax, [es:di]
    and ax, 0x7f00
    or ax, 0x8000
    or ax, [paddlech]

    mov word [es:di], ax

    pop ax

skipPaddleTwoDraw:
    add di, 2
    loop drawPaddleTwo

    ; Initialize the ball on the last row paddle
    mov di, [paddleTwo]         ; Ball starts on paddleTwo (last row paddle)
    shl di, 1
    add di, 3840                ; Offset for last row

    ; Center the ball on the paddle
    mov bx, [paddleWidth]
    shr bx, 1
    add di, bx                  ; Move to the middle of the paddle

    ; Draw the ball
    mov ax, 0x0F00              ; Ball color/character (example: white color)
    mov word [es:di], ax

    popa

    ret

;--------------------------------------SHUTTERON ----------------------------------

shutterOn:

pusha


    push cs
    pop ds

    mov ax, 0xb800
    mov es, ax


    xor di,di
    mov cx,2000

    son1:

    mov ax,[es:di]
    or ax,0x8000
    mov word[es:di],ax

    add di,2

    loop son1


popa

ret

;--------------------------------------SHUTTER OFF ----------------------------------

shutterOff:

pusha


    push cs
    pop ds

    mov ax, 0xb800
    mov es, ax


    xor di,di
    mov cx,2000

    sof1:

    mov ax,[es:di]
    and ax,0x7fff
    mov word[es:di],ax

    add di,2

    loop sof1


popa

ret





;--------------------------------------TH ----------------------------------

THooker:

pusha

push cs
pop ds

xor ax,ax
mov es,ax
mov ax,[es:8*4]
mov [oldtb],ax
mov ax,[es:8*4+2]
mov [oldtb+2],ax
cli

mov bx,[ThookOff]
mov word [es:8*4],bx

mov word [es:8*4+2],cs

sti




popa
ret


;--------------------------------------KH ----------------------------------

KHooker:

pusha

push cs
pop ds

xor ax,ax
mov es,ax
mov ax,[es:9*4]
mov [oldkb],ax
mov ax,[es:9*4+2]
mov [oldkb+2],ax
cli

mov bx,[KhookOff]
mov word [es:9*4],bx

mov word [es:9*4+2],cs

sti


popa
ret


;--------------------------------------PADDLE MOVEMENT----------------------------------


paddleMove:

pusha


push cs
pop ds

mov bx,79
sub bx,[paddleWidth]

mov cx,1

cmp word[ball_dy],0x0
jnz othrp


in al,0x60

cmp al,0x4d
jnz pmp1nc1

cmp word [paddleOne],bx
jz pmp1nc1

inc word [paddleOne]

pmp1nc1:

cmp al,0x4b
jnz pmp1ce

cmp word [paddleOne],cx
jz pmp1ce


dec word [paddleOne]

pmp1ce:

jmp pmfin

othrp:

in al,0x60

cmp al,0x4d
jnz pmp2nc1

cmp word [paddleTwo],bx
jz pmp2nc1

inc word [paddleTwo]

pmp2nc1:

cmp al,0x4b
jnz pmp2ce

cmp word [paddleTwo],cx
jz pmp2ce

dec word [paddleTwo]

pmp2ce:

pmfin:

mov al,0x20
out 0x20,al

popa

iret


;--------------------------------------BALL MOVE----------------------------------


ballMove:

pusha


    call clrGame
   

push cs
pop ds

mov cx,[timeSwitch]

cmp word[tickCnt],cx
jz bmst

inc word[tickCnt]

mov al,0x20
out 0x20,al

popa

call draw
iret

bmst:

mov cx,[paddleWidth]
shr cx,1
mov ax,40
sub ax,cx


cmp word[ball_y],-4
jnz bmfonc

mov word[paddleOne],ax
mov word[paddleTwo],ax

mov word[ball_x],39
mov word[ball_y],23

mov word[ball_dx],1
mov word[ball_dy],0


call clrBoard
inc word[p1Score]
call draw

call shutterOn



call shutterOff

jmp bmbgn

bmfonc:
cmp word[ball_y],28
jnz bmbgn

mov word[paddleOne],ax
mov word[paddleTwo],ax

mov word[ball_x],39
mov word[ball_y],1

mov word[ball_dx],0
mov word[ball_dy],1

call clrBoard
inc word[p2Score]
call draw

call shutterOn



call shutterOff

bmbgn:


and word[tickCnt],0x0


;; // BORDER COLLISION

cmp word[ball_x],78
jnz bcnc1

mov word[ball_dx],0

jmp bmbcen

bcnc1:
cmp word[ball_x],1
jnz bmbcen

mov word[ball_dx],1

bmbcen:


;; // PAD COLLISON

mov bx,[ball_x]

cmp word[ball_y],23
jnz pcnc1

mov dx,[paddleOne]

sub bx,dx

cmp bx,[paddleWidth]
jge bmpcen

cmp bx,0
jl bmpcen


;call clrBoard
mov word[ono],1

mov word[ball_dy],1


jmp bmpcen

pcnc1:
cmp word[ball_y],1
jnz bmpcen

mov dx,[paddleTwo]

sub bx,dx

cmp bx,[paddleWidth]
jge bmpcen

cmp bx,0
jl bmpcen

;call clrBoard
mov word[ono],1

mov word[ball_dy],0

bmpcen:



bmchn:

cmp word[ball_dx],0x0
jnz bmright


cmp word[ball_dy],0x1
jnz bmldown

dec word[ball_x]
dec word[ball_y]

jmp bmfin

bmldown:

dec word[ball_x]
inc word[ball_y]

jmp bmfin

bmright:

cmp word[ball_dy],0x1
jnz bmrdown

inc word[ball_x]
dec word[ball_y]

jmp bmfin

bmrdown:

inc word[ball_x]
inc word[ball_y]

jmp bmfin

bmfin:

inc word[tickCnt]

mov al,0x20
out 0x20,al

popa
call draw
iret





p1wins:
call clrscr
mov ah,0x09
mov dx,p1win
int 0x21

jmp exiting

p2wins:
call clrscr
mov ah,0x09
mov dx,p2win
int 0x21
jmp exiting

;;;;;;;;;;;;;;;;;;;;;;;;;;;;          [MAIN]          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

start:
    call clrscr
   call spawn
    call clrBoard

    call clrGame

    mov bx,paddleMove
    mov [KhookOff],bx

    call KHooker 

    mov bx,ballMove
    mov word [ThookOff],bx

    call THooker
loopp:
    mov dx, [p1Score]

	
    cmp dx, 5
    je unhook_interrupts
    mov dx, [p2Score]
    cmp dx, 5
	je unhook_interrupts
    jmp loopp

unhook_interrupts:
    ; Unhook Timer Interrupt
    pusha
    cli                         ; Disable interrupts
    xor ax, ax
    mov es, ax                  ; Load segment 0 (IVT)
    mov ax, word [oldtb]
    mov word [es:8*4], ax       ; Restore original offset for timer interrupt
    mov ax, word [oldtb+2]
    mov word [es:8*4+2], ax     ; Restore original segment for timer interrupt

    ; Unhook Keyboard Interrupt
    mov ax, word [oldkb]
    mov word [es:9*4], ax       ; Restore original offset for keyboard interrupt
    mov ax, word [oldkb+2]
    mov word [es:9*4+2], ax     ; Restore original segment for keyboard interrupt
    sti                         ; Re-enable interrupts
    popa

    ; Determine winner
    mov bx,[p1Score]
	cmp bx,5
	je p1wins
    jmp p2wins


exiting:
    mov ax, 0x4c00
    int 0x21