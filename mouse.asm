.486
data segment use16
assume ds: data
    titleMsg byte " Welcome to the mouse driver program!", 0ah, 0dh, 0
             byte "Here are the list of implemented functions:", 0ah, 0dh, 0
             byte "Mouse cursor display, position display, left/right button clicked display", 0ah, 0dh, 0
             byte "Press Esc to quit."
    titleLen equ $ - titleMsg

    infoPos  byte "Mouse position: ("
    infoPosX byte 0, 0, 0, ", "
    infoPosY byte 0, 0, 0, ")"
    infoLen  equ $ - infoPos

    posX     word 0
    posY     word 0
    status   byte 0
    posXold  word 0
    posYold  word 0


    ; mouse shape, each word type data represents a pixel
    mousePixels word 0000h

                word 0001h, 0101h

                word 0002h, 0102h, 0202h

                word 0003h, 0103h, 0203h, 0303h

                word 0004h, 0104h, 0204h, 0304h, 0404h

                word 0005h, 0105h, 0205h, 0305h, 0405h, 0505h

                word 0006h, 0106h, 0206h, 0306h, 0406h, 0506h, 0606h

                word 0007h, 0107h, 0207h, 0307h, 0407h, 0507h, 0607h, 0707h

                word 0008h, 0108h, 0208h, 0308h, 0408h, 0508h, 0608h, 0708h, 0808h

                word 0009h, 0109h, 0209h, 0309h, 0409h, 0509h, 0609h, 0709h, 0809h, 0909h

                word 000ah, 010ah, 020ah, 030ah, 040ah, 050ah, 060ah

                word 000bh, 010bh, 020bh, 030bh, 040bh, 050bh, 060bh

                word 000ch, 010ch, 020ch, 030ch, 040ch, 050ch, 060ch, 070ch

                word 000dh, 010dh, 020dh,               050dh, 060dh, 070dh

                word 000eh, 010eh,                      050eh, 060eh, 070eh, 080eh

                word 000fh,                             050fh, 060fh, 070fh, 080fh

    mousePixelsLen   equ $ - mousePixels
    mousecolor       byte mousePixelsLen dup (0fh)
    mousePixelsCnt   word 0

    saveold  byte mousePixelsLen dup (0)    ;保存鼠标位置的屏幕图象
    savenew  byte mousePixelsLen dup (0)    ;用于和saveold交替使用

data ends


code segment use16
assume cs:code

; init procedure
init proc far
    pusha
    mov ax, data
    mov ds, ax
    mov es, ax

    ; ---------------------------------------------------
    ; Initiate the screen with 10h interrupt
    ; Entry args:
    ; ah:     06h: Scroll up, 07h: Scroll down
    ; al:     Rolling lines(0: clear the window)
    ; bh:     Default value for the blank space
    ; cl, ch: The upper-left corner of the window(x, y)
    ; dl, dh: The lower-right corner of the window(x, y)
    ; ---------------------------------------------------
    mov al, 0
    mov ah, 6
    mov bh, 7
    mov cl, 0
    mov ch, 0
    mov dl, 200
    mov dh, 200
    int 10h

    ; ------------------------------------
    ; Set display mode with 10h interrupt
    ; Entry args:
    ; ax: 12h: 640*480, 16-color mode
    ; ------------------------------------
    mov al, 12h
    mov ah, 00h
    int 10h

    ; -----------------------------------------------------
    ; Display string under teletype mode with 10h interrupt
    ; Entry args:
    ; ah:     13h
    ; al:     Display output mode
    ; bl:     Display property(with al = 00h or 01h)
    ; bh:     Page number
    ; cx:     Length of the target string
    ; dh, dl: coordinate (row, column)
    ; -----------------------------------------------------
    mov bp, offset titleMsg
    mov cx, titleLen
    mov dh, 0
    mov dl, 0
    mov bh, 0
    mov al, 0
    mov bl, 7
    mov ah, 13h
    int 10h

    popa
    ret

init endp

; install mouse device handler
installHandler proc far
    pusha
    mov ax, 0c201h
    int 15h

    ; Install mouse device handler with interrupt 15h
    ; Entry args:
    ; ax:    0c207h: set device handler address
    ; es:bx: far user device handler or 0000h:0000h to cancel
    mov ax, seg handler
    mov es, ax
    mov ax, 0c207h
    mov bx, OFFSET handler
    int 15h
    ; CF set to 1 on error
    jc errInstall

    ; Enable mouse device handler with interrupt 15h
    ; Entry args:
    ; ax: 0c200h
    ; bh: 00h: disabled, 01h: enabled
    mov ax, 0c200h
    mov bh, 1
    int 15h
    ; CF set to 1 on error
    jc errEnable

errInstall:
errEnable:
    popa
    ret

installHandler endp

listenEsc proc far
    mov ah, 11h
    int 16h
    ; no key pressed
    jz noKeyPressed
    mov ah, 10h
    int 16h
    ; key pressed, but not Esc
    cmp al, 1Bh
    jne noKeyPressed
    jmp escPressed

noKeyPressed:
    mov ah, 11h
    int 16h
    jz noKeyPressed
    mov ah, 10h
    int 16h
    ; key pressed, but not Esc
    cmp al, 1Bh
    jne noKeyPressed

escPressed:
    ; clear handler
    mov ax, seg nullhandler
    mov es, ax
    mov bx, OFFSET nullhandler
    mov ax, 0c207h
    int 15h

    mov ax, 0c201h
    int 15h

    mov bx, 0
    mov dx, 0
    mov ax, 4f05h
    int 10h

    mov al, 02
    mov ah, 0
    int 10h
    mov ah, 4ch
    int 21h

listenEsc endp

nullhandler proc far
    ret
nullhandler endp

; mouse device handler procedure
; stack status:
; mouse device status, status (sp + 10)
; movement in x axis, deltaX (sp + 8)
; movement in y axis, deltaY (sp + 6)
; 0000h (sp + 4)
; return address (sp + 2)

; bitfields for device status:
; bit(s)    description
; 15 - 8    reserverd (0)
; 7         Y data overflowed
; 6         X data overflowed
; 5         Y data is negative
; 4         X data is negative
; 3         reserved (1)
; 2         reserved (0)
; 1         right button pressed
; 0         left button pressed
handler proc far
    mov ax, data
    mov ds, ax

    mov ax, ss
    mov es, ax
    mov bx, sp

    add bx, 6
    mov dx, es:[bx]
    push dx
    add bx, 2
    mov dx, es:[bx]
    push dx
    add bx, 2
    mov dx, es:[bx]
    push dx

first:
    pop dx
    mov status, dl

    ;保存老的鼠标的x, y坐标信息
    mov ax, posX
    mov posXold, ax
    mov ax, posY
    mov posYold, ax

;左键
    test dx, 1
    jnz ld
    mov rectcolor, 0fh
    call drawleftbutton
    jmp n
ld:
    mov ax, posX
    mov ax, posY
    mov rectcolor, 03h    ;左键变色
    call drawleftbutton
;右键
n:
    test dx, 2
    jnz rd                ;按下右键
    mov rectcolor, 0fh    ;右键没有按下按钮变白
    call drawrightbutton
    jmp second
rd:
    mov ax, posX
    mov ax, posY
    mov rectcolor, 02h
    call drawrightbutton

;X位移处理过程
second:
    pop dx
    cmp dx, 0
    jnz movedx
    jmp third
movedx:
    test status, 10h
    jnz  xnegative
    add posX, dx

    ;防止鼠标移出屏幕
    cmp posX, 639
    jnb big640
    jmp third
big640:
    mov posX, 639
    jmp third
xnegative:
    neg dl
    sub posX, dx
    cmp posX, 0
    jl xless0
    jmp third
xless0:
    mov posX, 0
    jmp third

;Y位移处理过程
third:
    pop dx
    cmp dx, 0
    jnz movedy
    jmp complete

movedy:
    test status, 20h
    jnz ynegative

    sub posY, dx
    cmp posY, 0
    jl yless0
    jmp complete
yless0:
    mov posY, 0
    jmp complete

ynegative:
    neg dl
    add posY, dx
    cmp posY, 479
    jnb big480
    jmp complete
big480:
    mov posY, 479
    jmp complete

complete:
    ; push cs
    ; pop ax
    mov ax, data
    mov es, ax
    mov ds, ax
    mov si, OFFSET savenew
    mov di, OFFSET saveold
    mov cx, mousePixelsLen*2
    cld
    rep movsb

    ;使用saveold恢复原屏幕值
    call restore

    ;保存新鼠标位置的屏幕值到savenew缓冲中
    call save_mouse

showms:
    call show_mouse

    ; display mouse position
    pusha
    mov ax, data
    mov ds, ax
    mov es, ax

    mov ax, posX
    mov bx, offset infoPosX
    call btoasc

    mov ax, posY
    mov bx, offset infoPosY
    call btoasc

    ; display mouse position
    mov bp, offset infoPos
    mov cx, infoLen
    mov dh, 6
    mov dl, 23
    mov bh, 0
    mov al, 0
    mov bl, 7
    mov ah, 13h
    int 10h
    popa

    ret
handler endp

;恢复老鼠标位置屏幕
restore proc far
    pushad
    mov ax, data
    mov ds, ax
    mov ebx, 0
    mov edx, 0
    mov ecx, 0
    mov eax, 0

    mov ax, mousePixelsLen
    shr ax, 1                ;count/2 (WORD)
    mov mousePixelsCnt, ax ;mousePixelsCnt = mousePixelsLen/2
    mov di, 0
    mov si, 0

restorepixel:
    mov cx, posXold          ;将前一个鼠标位置x坐标放到cx中
    mov ax, mousePixels[di]    ;将鼠标中的一个点放到ax中
    push ax
    shr ax, 8
    and ax, 0fh
    add cx, ax               ;cx存储x坐标
    pop ax
    mov dx, posYold
    and ax, 0fh
    add dx, ax               ;dx存储y坐标

    mov ah, 0ch
    mov al, saveold[si]
    mov bh, 0
    int 10h
    inc si

CT:
    add di, 2                   ;next word
    dec mousePixelsCnt
    jnz restorepixel
    popad
    ret

restore endp

;保存当前鼠标位置的屏幕内容
save_mouse proc far
    pushad

    mov ax, data
    mov ds, ax

    mov bx, 0

    mov ax, mousePixelsLen
    shr ax, 1
    mov mousePixelsCnt, ax
    mov di, 0
    mov si, 0
savepixel:
    mov cx, posX
    mov ax, mousePixels[di]
    movzx bx, ah
    add cx, bx

    mov dx, posY
    movzx bx, al
    add dx, bx

    mov ah, 0dh
    mov bh, 0
    int 10h
    mov savenew[si], al
    inc si

    add di, 2               ;next word
    dec mousePixelsCnt
    jnz savepixel
    popad
    ret
save_mouse endp

;功能 :显示鼠标
show_mouse proc far
    pushad
    mov ax, data
    mov ds, ax

    mov bx, 0

    mov ax, mousePixelsLen
    shr ax, 1
    mov mousePixelsCnt, ax
    mov di, 0
    mov si, 0

lodrmos:
    mov ax, mousePixels[di]

    mov cx, posX
    movzx bx, ah
    add cx, bx

    mov dx, posY
    movzx bx, al
    add dx, bx

    cmp cx, 639
    jg next                ;超过边框不用画
    jmp notexceed

notexceed:
    mov ah, 0ch
    mov al, mousecolor[si]
    mov bh, 0
    int 10h
    inc si

next:
    add di, 2               ;next word
    dec mousePixelsCnt
    jnz lodrmos
    popad
    ret
show_mouse endp

;画水平线
drawhorline proc far
    pusha
    mov cx, lineend2x
    sub cx, lineend1x
    inc cx              ;循环次数
    mov bx, lineend1x   ;bx暂存posx
    mov dx, lineend1y

horlinedraw:
    push cx
    mov cx, bx
    mov ah, 0ch
    mov al, linecolor
    mov bh, 0
    int 10h
    inc cx
    mov bx, cx
    pop cx
    loop horlinedraw
    popa
    ret
drawhorline ENDP

;画竖直线
drawverline proc far
    pusha
    mov cx, lineend2y
    sub cx, lineend1y
    inc cx              ;循环次数
    mov bx, lineend1x   ;暂存x
    mov dx, lineend1y

verlinedraw:
    push cx
    mov cx, bx
    mov ah, 0ch
    mov al, linecolor
    mov bh, 0
    int 10h
    inc dx
    mov bx, cx
    pop cx
    loop verlinedraw
    popa
    ret
drawverline endp

;画矩形
drawrect proc far
    pusha
    mov ax, rectend1x
    mov lineend1x, ax
    mov ax, rectend2x
    mov lineend2x, ax
    mov cx, rectend2y
    sub cx, rectend1y
    inc cx
    mov al, rectcolor
    mov linecolor, al
    mov ax, rectend1y
    mov lineend1y, ax

rectdraw:
    call drawhorline
    inc lineend1y
    loop rectdraw
    popa
    ret
drawrect endp

;画左键
drawleftbutton proc far
    pusha
    mov rectend1x, 250
    mov rectend1y, 160
    mov rectend2x, 310
    mov rectend2y, 260
    call drawrect
    popa
    ret
drawleftbutton endp

;画右键
drawrightbutton proc far
    pusha
    mov rectend1x, 330
    mov rectend1y, 160
    mov rectend2x, 390
    mov rectend2y, 260
    call drawrect
    popa
    ret
drawrightbutton endp

btoasc proc far
    mov si, 3
    mov cx, 10
btoasc1:
    xor dx, dx
    div cx
    add dl, 30h
    dec si
    mov [bx][si], dl
    or si, si
    jnz btoasc1
    ret
btoasc endp

main:
    call init

    pusha
    mov rectcolor, 0fh
    call drawleftbutton
    call drawrightbutton
    popa

    call installHandler
    call save_mouse
    call listenEsc


lineend1x WORD 0
lineend1y WORD 0
lineend2x WORD 0
lineend2y WORD 0
rectend1x WORD 0
rectend1y WORD 0
rectend2x WORD 0
rectend2y WORD 0
linecolor BYTE 0fh
rectcolor BYTE 0fh

code ends
end main
