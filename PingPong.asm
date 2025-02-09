[org 0x0100]


jmp start

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|||||||> VARIABLES <||||||;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; // STORE DEFAULT TIMER

timerOffset: dw 0x0

timerSegment: dw 0x0

;; // TICK COUNT

tickCnt: dw 0x0


;; // STORE DEFAULT KEYPAD

keypadOffset: dw 0x0

keypadSegment: dw 0x0

;; // FLOAT PING PONG

fd: dw 0x0

;; // OFFSET VARIABLES
  
readupOff: dw 0x0

   ;; HOOKER

ThookOff: dw 0x0
KhookOff: dw 0x0

;; // GAME OBJECTS


;; DIFF ARGUMENTS

timeSwitch: dw 2
paddleWidth: dw 16
ono: dw 0

;; CHARS

paddlech: dw 0xdb

ballch: dw 0x09

;; POSITION VARIABLES

paddleOne: dw 0x0

paddleTwo: dw 0x0


ball_x: dw 39
ball_y: dw 23

;; // BALL DIRECTION

ball_dy: dw 0x0
ball_dx: dw 0x0

;; // GAME SCORE

p1Score: dw 0
p2Score: dw 0



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BOOLS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; // TERMINATE LOADER

boolTmnt: dw 0x0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SCREEEN MAPS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; // LOAD LOADING SCREEN

ping: db '       ]]]]   ]  ]    ]   ]]]]]            ]]]]   ]]]]]]   ]    ]   ]]]]]       ', 
     db '      ]]]]]] ]] ]]]   ]] ]]]]]]]          ]]]]]] ]]]]]]]] ]]]   ]] ]]]]]]]      ',
     db '      ]]  ]] ]] ]]]]  ]] ]]]              ]]  ]] ]]]  ]]] ]]]]  ]] ]]]          ',
     db '      ]]  ]] ]] ]]]]] ]] ]]               ]]  ]] ]]    ]] ]]]]] ]] ]]           ',
     db '      ]]]]]] ]] ]] ]]]]] ]]  ]]]          ]]]]]] ]]    ]] ]] ]]]]] ]]  ]]]      ',
     db '      ]]]]]  ]] ]]  ]]]] ]] ]]]]          ]]]]]  ]]    ]] ]]  ]]]] ]] ]]]]      ',
     db '      ]]     ]] ]]   ]]] ]]   ]]          ]]     ]]    ]] ]]   ]]] ]]   ]]      ',
     db '      ]]     ]] ]]    ]] ]]]  ]]          ]]     ]]]  ]]] ]]    ]] ]]]  ]]      ',
     db '      ]]     ]] ]]    ]] ]]]]]]]          ]]     ]]]]]]]] ]]    ]] ]]]]]]]      ',
     db '       ]     ]   ]    ]   ]]]]]            ]      ]]]]]]   ]    ]   ]]]]]       '

pinh: db '                                                                                ', 
     db '         ]]]]    ]]]]   ]] ]]    ]]]      ]]]]     ]]]    ]] ]]    ]]]          ',
     db '        ]]   ]  ]]   ] ]] ]] ]  ]]  ]    ]]   ]   ]]  ]  ]] ]] ]  ]]  ]         ',
     db '        ]] ]] ]  ]] ]  ]]  ] ] ]] ]]]    ]] ]] ] ]] ]] ] ]]  ] ] ]] ]]]         ',
     db '        ]]   ]   ]] ]  ]] ]  ] ]] ]  ]   ]]   ]  ]] ]] ] ]] ]  ] ]] ]  ]        ',
     db '        ]] ]]    ]] ]  ]] ]] ] ]] ]] ]   ]] ]]   ]] ]] ] ]] ]] ] ]] ]] ]        ',
     db '        ]] ]    ]]   ] ]] ]] ]  ]]  ]    ]] ]     ]]  ]  ]] ]] ]  ]]  ]         ',
     db '         ]]      ]]]]   ]] ]]    ]]]      ]]       ]]]    ]] ]]    ]]]          ',
     db '                                                                                ',
     db '                                                                                '

pini: db '                                                                                ', 
     db '              ]]]]] ]]]]]] ]]  ]] ]]]]]   ]]]]] ]]]]]] ]]  ]] ]]]]]             ',
     db '              ]] ]] ]]]]]] ]]] ]] ]]]]]   ]] ]] ]]] ]] ]]] ]] ]]]]]             ',
     db '              ]] ]]   ]]   ]]] ]] ]]      ]] ]] ]]] ]] ]]] ]] ]]                ',
     db '              ]] ]]   ]]   ]]]]]] ]] ]]   ]] ]] ]]] ]] ]]]]]] ]] ]]             ',
     db '              ]]]]]   ]]   ]] ]]] ]]  ]   ]]]]] ]]] ]] ]] ]]] ]]  ]             ',
     db '              ]]    ]]]]]] ]]  ]] ]]]]]   ]]    ]]]]]] ]]  ]] ]]]]]             ',
     db '              ]]    ]]]]]] ]]  ]] ]]]]]   ]]    ]]]]]] ]]  ]] ]]]]]             ',
     db '                                                                                ',
     db '                                                                                '

;; // GAME READY SCREEN

startmap: db '                                                                                ', 
     db '                        ]]]]] ]]]]]] ]]]]]] ]]]]] ]]]]]]                        ',
     db '                        ]]]]] ]]]]]] ]]] ]] ]] ]] ]]]]]]                        ',
     db '                        ]]      ]]   ]]] ]] ]] ]]   ]]                          ',
     db '                        ]]      ]]   ]]]]]] ]] ]]   ]]                          ',
     db '                        ]]]]]   ]]   ]]] ]] ]]]]]   ]]                          ',
     db '                           ]]   ]]   ]]] ]] ]]]]    ]]                          ',
     db '                        ]]]]]   ]]   ]]] ]] ]] ]]   ]]                          ',
     db '                                                                                ',
     db '                                                                                '

map3: db '                                                                                ', 
     db '                                      ]]]]                                      ',
     db '                                      ]]]]                                      ',
     db '                                        ]]                                      ',
     db '                                      ]]]]                                      ',
     db '                                        ]]                                      ',
     db '                                      ]]]]                                      ',
     db '                                      ]]]]                                      ',
     db '                                                                                ',
     db '                                                                                '

map2: db '                                                                                ', 
     db '                                     ]]]]]                                      ',
     db '                                     ]]]]]                                      ',
     db '                                        ]]                                      ',
     db '                                        ]]                                      ',
     db '                                      ]]]]                                      ',
     db '                                      ]]                                        ',
     db '                                     ]]]]]                                      ',
     db '                                                                                ',
     db '                                                                                '

map1: db '                                                                                ', 
     db '                                       ]]                                       ',
     db '                                      ]]]                                       ',
     db '                                       ]]                                       ',
     db '                                       ]]                                       ',
     db '                                       ]]                                       ',
     db '                                       ]]                                       ',
     db '                                      ]]]]                                      ',
     db '                                                                                ',
     db '                                                                                '


;; // EXIT LOAD SCREEN

curtain: db '65000000000000000000000000000000000000000000000000000000000000000000000000000057', 
     db '21550000000000000000000000000000000000000000000000000000000000000000000000005512',
     db '22115500000000000000000000000000000000000000000000000000000000000000000000551122',
     db '32221155000000000000000000000000000000000000000000000000000000000000000055112223',
     db ' 322221155000000000000000000000000000000000000000000000000000000000000551122223 ',
     db '  3322221155000000000000000000000000000000000000000000000000000000005511222233  ',
     db '    332222115550000000000000000000000000000000000000000000000000055511222233    ',
     db '      33222211155500000000000000000000000000000000000000000000555111222233      ',
     db '        3322222111555000000000000000000000000000000000000005551112222233        ',
     db '          332222221115550000000000000000000000000000000055511122222233          ',
     db '            33322222211155550000000000000000000000005555111222222333            ',
     db '               33322222211115555000000000000000055551111222222333               ',
     db '                  33322222221111555500000000555511112222222333                  ',
     db '                     33322222222111155555555111122222222333                     ',
     db '                        33332222222211111111222222223333                        ',
     db '                            333322222222222222223333                            ',
     db '                                3333222222223333                                ',
     db '                                    33333333                                    ',
     db '                                                                                '
    

spike: db '00000000000000000000000000000000000000000000000000000000000000000000000000000000', 
         db '00000000000000000000000000000000000000000000000000000000000000000000000000000000',
         db '00000000000000000000000000000000000000000000000000000000000000000000000000000000',
         db '00000000000000000000000000000000000000000000000000000000000000000000000000000000',
         db '00000000000000000000000000000000000000000000000000000000000000000000000000000000',
         db '00000000000000000000000000000000000000000000000000000000000000000000000000000000',
         db '00000000000000000000000000000000000000000000000000000000000000000000000000000000',
         db '00000000000000000000000000000000000000000000000000000000000000000000000000000000',
         db '00000000000000000000000000000000000000000000000000000000000000000000000000000000',
         db '00000000000000000000000000000000000000000000000000000000000000000000000000000000',
         db '00000000000000000000000000000000000000000000000000000000000000000000000000000000',
         db '00000000000000000000000000000000000000000000000000000000000000000000000000000000',
         db '00000000000000000000000000000000000000000000000000000000000000000000000000000000',
         db '00000000000000000000000000000000000000000000000000000000000000000000000000000000',
         db '000  0000000  0000000  0000000  0000000  0000000  0000000  0000000  0000000  000',
         db '00    00000    00000    00000    00000    00000    00000    00000    00000    00',
         db '0      000      000      000      000      000      000      000      000      0',
         db '        0        0        0        0        0        0        0        0        ',
         db '                                                                                '
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CLEAR SCREEN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DELAY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; WRITE PING PONG SCREEN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pong:
    push bp
    mov bp, sp

    pusha

    push cs
    pop ds

    mov ax, 0xb800
    mov es, ax

    mov di, [bp+4]
    mov si, pini

    mov cx, 800
p1:
    mov ah, 0x46
    mov al, [si]

    cmp al,']'
    jnz pskip

    mov al,0xDB

    pskip:

    mov [es:di], ax

    add di, 2
    inc si

    loop p1

    mov [bp+4],di

    popa
   

    pop bp

    ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LOAD LOADING SCREEN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

load_page:
    pusha

    push cs
    pop ds

    mov ax, 0xb800
    mov es, ax

    xor di, di

    mov ax, 0x46b2

    mov cx, 80
    rep stosw

    mov ax, 0x46e8

    mov cx, 80
    rep stosw

    mov ax, 0x46b1

    mov cx, 80
    rep stosw

    mov ax, 0x46b0

    mov cx, 80
    rep stosw

    mov ax, 0x4600

    mov cx, 320
    rep stosw

    push di
    call pong
    pop di

    

    mov ax, 0x4600

    mov cx, 240
    rep stosw

    mov ax, 0x46b0

    mov cx, 80
    rep stosw

    mov ax, 0x46b1

    mov cx, 80
    rep stosw

    mov ax, 0x46e8

    mov cx, 80
    rep stosw

    mov ax, 0x46b2

    mov cx, 80
    rep stosw

    popa
    ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FLOAT PING PONG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

flo:
cli

push bp
mov bp,sp

pusha

push cs
pop ds

mov ax,0xb800
mov es,ax
push es
pop ds

mov di,[bp+4]

mov ax,0x4600

cmp word[cs:fd],0x0
jz up

cmp word[cs:fd],0x1
jz down

up:
cmp di,960
jz setdown

mov dx,di
sub dx,160

mov si,di
sub di,160

cld


jmp fsink
setdown:

mov word[cs:fd],1

jmp fend


down:
cmp di,1440
jz setup

mov dx,di
add dx,160

mov si,di
add si,1598
mov di,si
add di,160

std

jmp fsink
setup:

mov word[cs:fd],0

jmp fend

fsink:

mov cx,800
rep movsw


mov di,dx

cmp word[cs:fd],0
jz clup

cmp word[cs:fd],1
jz cldown

clup:

add di,1600

jmp fein

cldown:

sub di,160

jmp fein


fein:
cld
mov cx,80
rep stosw

mov [bp+4],dx

fend:

popa
pop bp

sti

ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXIT LOAD SCREEN ARROW  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


exitLoad:

push bp

mov bp,sp

pusha

push cs
pop ds

mov ax,0xb800
mov es,ax

xor dx,dx
mov ax,[bp+4]

mov bx,160

mul bx

mov di,ax


mov si,curtain

mov cx,1520

elsc:

mov ax,word[es:di]

cmp byte[si],'0'
jnz ar1

mov ax,0x0720

ar1:
cmp byte[si],'1'
jnz ar2

mov ax,0x46b0

ar2:
cmp byte[si],'2'
jnz ar3

mov ax,0x46b1

ar3:
cmp byte[si],'3'
jnz ar4

mov ax,0x46b2

ar4:
cmp byte[si],'5'
jnz ar5

mov ax,0x0720

ar5:
cmp byte[si],'6'
jnz ar6

mov ax,0xc611


ar6:
cmp byte[si],'7'
jnz endar

mov ax,0xc610




endar:

mov word[es:di],ax

inc si

add di,2

loop elsc



popa

pop bp

ret 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CALCULATE DECIMAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


calDec:

push bp
mov bp,sp

pusha


shr word[bp+4],16
shl word[bp+6],16

mov ax,[bp+8]
mov bx,0xA

mov si,0

cdl1:

mov dx,0

div bx

mov cx,0

cdl2:

shr dx,1
rcr word[bp+4],1
rcr word[bp+6],1

inc cx 

cmp cx,4
jnz cdl2

inc si

cmp ax,0
jnz cdl1


shl si,2



cdl3:

mov ax,[bp+6]
shl ax,1

rcl word[bp+4],1
rcl word[bp+6],1

dec si

cmp si,0
jnz cdl3

popa

pop bp

ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DISPLAY PRINT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ABSOLUTE DELAY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DECIMAL DISPLAY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

disp:

sub sp,4

push bp
mov bp,sp


push ax


push word[bp+8]
push word[bp+2]
push word[bp+4]

call calDec

pop word[bp+4]
pop word[bp+2]
pop word[bp+8]


mov ax,0xf90

push ax
push word[bp+4]

call print


mov ax,0xf98

push ax
push word[bp+2]

call print


pop ax

pop bp

add sp,4

ret 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    STORE DEFAULT TIMER    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


storeTDefault:

pusha

push cs
pop ds

xor ax,ax
push ax
pop es

mov ax,[es:0x8*4]

mov word [timerOffset],ax

mov bx,[es:0x8*4+2]

mov word [timerSegment],bx


popa

ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    RESTORE DEFAULT TIMER    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


restoreTDefault:

pusha

push cs
pop ds

xor ax,ax
push ax
pop es

cli
mov ax, [timerOffset]
mov word[es:0x8*4], ax

mov bx, [timerSegment]
mov word[es:0x8*4+2], bx
sti

popa

ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    STORE DEFAULT KEYBOARD    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


storeKDefault:

pusha

push cs
pop ds

xor ax,ax
push ax
pop es

mov ax,[es:0x9*4]

mov word [keypadOffset],ax

mov bx,[es:0x9*4+2]

mov word [keypadSegment],bx


popa

ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    RESTORE DEFAULT KEYBOARD    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


restoreKDefault:

pusha

push cs
pop ds

xor ax,ax
push ax
pop es

cli
mov ax, [keypadOffset]
mov word[es:0x9*4], ax

mov bx, [keypadSegment]
mov word[es:0x9*4+2], bx
sti

popa

ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    CLEAR GAME PAD     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

cmp word [es:di],0x4611
jz clgskip1

cmp word [es:di],0x4610
jz clgskip1


and word [es:di],0xff00
or word [es:di],0x0020


clgskip1:

add di,2

loop clg1


popa

ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      CLEAR BOARD PAD      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

clrBoard:

pusha

mov di,0
mov si,158

mov ax,0xb800
mov es,ax

mov cx,25

clb0:

mov word [es:di],0x4611

mov word [es:si],0x4610

add di,160

add si,160

loop clb0


xor di,di

mov cx,2000

clb1:

cmp word [es:di],0x4611
jz clbskip1

cmp word [es:di],0x4610
jz clbskip1


mov word[es:di],0x0720 


clbskip1:

add di,2

loop clb1


popa

ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      DRAW GAME PAD      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     SPAWN OBJ     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

spawn:

pusha


mov word [paddleOne],80
mov word [paddleTwo],-14

xor di,di

mov ax,0xb800
mov es,ax

mov cx,50

sp1:

mov dx,5
push dx
call delay

call clrGame

push cx

mov di,[paddleOne]
shl di,1
add di,3840

mov cx,[paddleWidth]

sp2:

cmp word [es:di],0xc611
jz sp2skip

cmp word [es:di],0xc610
jz sp2skip

push ax

mov ax,[es:di]
and ax,0x7f00
or ax,0x8000
or ax,[paddlech]

mov word [es:di],ax

pop ax

sp2skip:

add di,2


loop sp2


mov di,[paddleTwo]
shl di,1
add di,0

mov cx,[paddleWidth]

sp3:

cmp word [es:di],0xc611
jz sp3skip

cmp word [es:di],0xc610
jz sp3skip

push ax

mov ax,[es:di]
and ax,0x7f00
or ax,0x8000
or ax,[paddlech]

mov word [es:di],ax

pop ax

sp3skip:

add di,2

loop sp3

pop cx

mov bx,[paddleWidth]
shr bx,1

push dx

mov dx,40
sub dx,bx
mov bx,dx

pop dx

cmp word[paddleTwo],bx
jz spnxtCmp

inc word[paddleTwo]

spnxtCmp:

cmp word[paddleOne],bx
jz sp1end

dec word[paddleOne]

sp1end:

dec cx
jnz sp1


popa

ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    SHUTTER ON     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    SHUTTER OFF     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    READY UP     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

readyUp:

pusha


    push cs
    pop ds

    mov ax, 0xb800
    mov es, ax


    mov di, 1280
    mov si, [readupOff]

    mov cx, 800
ru1:
    mov ah, 0x70
    mov al, [si]

    cmp al,']'
    jnz ruskip

    mov al,0xdb

    ruskip:

    mov [es:di], ax

    add di, 2
    inc si

    loop ru1


popa

ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   PLAYER TWO ENVO   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


playerTwoEnvo:

push bp

mov bp,sp

pusha

push cs
pop ds

mov ax,0xb800
mov es,ax

xor dx,dx
mov ax,[bp+4]

mov bx,160

mul bx

mov di,ax


mov si,spike
add si,0

mov cx,1520

p2evo:

mov ax,word[es:di]

cmp al,0x11
jz endp2ar

cmp al,0x10
jz endp2ar

cmp byte[si],'0'
jnz endp2ar

and ax,0x00ff
or ax,0x4f00


endp2ar:

mov word[es:di],ax

inc si

add di,2

loop p2evo



popa

pop bp

ret 2



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   PLAYER ONE ENVO   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


playerOneEnvo:

push bp

mov bp,sp

pusha

push cs
pop ds

mov ax,0xb800
mov es,ax

xor dx,dx
mov ax,[bp+4]

mov bx,160

mul bx

mov di,ax


mov si,spike
add si,1519

mov cx,1520

p1evo:

mov ax,word[es:di]

cmp al,0x11
jz endp1ar

cmp al,0x10
jz endp1ar

cmp byte[si],'0'
jnz endp1ar

and ax,0x00ff
or ax,0x6f00

endp1ar:

mov word[es:di],ax

dec si

add di,2

loop p1evo



popa

pop bp

ret 2


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    TIMER HOOK     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; <TIMER HOOK>

THooker:

pusha

push cs
pop ds

xor ax,ax
mov es,ax

cli

mov bx,[ThookOff]
mov word [es:8*4],bx

mov word [es:8*4+2],cs

sti


popa
ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    KEYBOARD HOOK     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; <KEYBOARD HOOK>

KHooker:

pusha

push cs
pop ds

xor ax,ax
mov es,ax

cli

mov bx,[KhookOff]
mov word [es:9*4],bx

mov word [es:9*4+2],cs

sti


popa
ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;     PADDLE MOVEMENT   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  <ISR>


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;     BALL MOVEMENT    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  <ISR>


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

mov cx,0x7f
push cx
call delay

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

mov cx,0x7f
push cx
call delay

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;     LOADING TERMINATE   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  <ISR>


terminateEnter:

pusha




push cs
pop ds



in al,0x60
cmp al,0x36

jnz teskip




mov word[boolTmnt],0x1



teskip:


mov al,0x20
out 0x20,al

popa

iret




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;          [MAIN]          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

start:


    call storeTDefault
    call storeKDefault

   

    call load_page


    mov bx,terminateEnter
    mov word [ThookOff],bx

    call THooker


    mov di,1280

    f1:
 
    mov cx,12
    push cx
    call delay

    cli
    push di
    call flo
    pop di

    sti

    mov bx,0xfa0
    push bx
    push word[boolTmnt]
    call print

    cmp word[boolTmnt],1
    jz efi

    
    

   

    efi:

        mov di,-18

 a1:
 
    mov cx,6
    push cx
    call delay

    push di
    call exitLoad
    
    inc di


    cmp di,25
    jnz a1

    call restoreTDefault


    ;; LOADING ENDS

    call spawn

    mov bx,map3
    mov word [readupOff],bx
    call readyUp

    mov cx,20
    push cx
    call delay

    mov bx,map2
    mov word [readupOff],bx
    call readyUp

     mov cx,19
    push cx
    call delay

    mov bx,map1
    mov word [readupOff],bx
    call readyUp

     mov cx,18
    push cx
    call delay

    mov bx,startmap
    mov word [readupOff],bx
    call readyUp

     mov cx,17
    push cx
    call delay
    
    call clrBoard

    call clrGame

    mov bx,paddleMove
    mov [KhookOff],bx

    call KHooker 

    mov bx,ballMove
    mov word [ThookOff],bx

    call THooker


    gmloop:





    cmp word[ono],1
    jnz gmloop

    ;; bkcodi

cmp word[ball_dy],0
jnz p2enie

push cx

mov cx,25

bmevo1:

push cx
call playerOneEnvo


loop bmevo1

mov word[ono],0

pop cx

jmp gmloop

p2enie:

push cx
push dx

mov cx,25

bmevo2:

mov dx,6
sub dx,cx

push dx
call playerTwoEnvo


loop bmevo2

mov word[ono],0

pop dx
pop cx

;; bkcodi


    jmp gmloop

   


    mov ax, 0x4c00
    int 0x21