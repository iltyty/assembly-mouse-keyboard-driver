.486
data segment use16
assume ds: data
    titleMsg byte "   O_o    Hey, my eyes are RGB lights!    O_o"
    titleLen equ $ - titleMsg

    promptMsg byte "Press Esc to kill me T^T"
    promptLen equ $ - promptMsg

    infoPos  byte "Mouse position: ("
    infoPosX byte 0, 0, 0, ", "
    infoPosY byte 0, 0, 0, ")"
    infoLen  equ $ - infoPos

    status   byte  0
    deltaX   word  0
    deltaY   word  0
    posX     sword 0
    posY     sword 0
    posXPrev word  0
    posYPrev word  0

    leftPressed  word 0
    rightPressed word 0

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
    mouseColor       byte mousePixelsLen dup (0fh)
    mousePixelsCnt   word 0

    pixelPrev  byte mousePixelsLen dup (0)
    pixelCurr  byte mousePixelsLen dup (0)

data ends


code segment use16
assume cs:code

;; al: color
draw macro x, y, color
    push ax
    push cx
    push dx

    mov cx, x
    mov dx, y
    mov al, color
    mov ah, 0ch
    mov bh, 0
    int 10h

    pop dx
    pop cx
    pop ax
endm

drawverline macro x, y1, y2, color
    ;; starting from (x, y) assume y1 < y2
    push ax

    mov ax, y1
    .while ax <= y2
        draw x, ax, color
        inc ax
    .endw

    pop ax
endm

drawhorline macro x1, x2, y, color
    ;; starting from (x1, y) assume x1 < x2
    push ax

    mov ax, x1
    .while ax <= x2
        draw ax, y, color
        inc ax
    .endw

    pop ax
endm

;; (x, y) left top point
;; w, h for width, height
drawrect macro x, y, w, h, color
    push ax
    push bx
    push cx
    push dx

    mov ax, x
    mov cx, ax
    mov bx, y
    mov dx, bx
    add cx, w
    add dx, h

    .while cx >= ax
        drawverline cx, bx, dx, color
        dec cx
    .endw

    pop dx
    pop cx
    pop bx
    pop ax
endm

drawframe macro x, y, w, h, color
    pusha

    mov ax, x
    mov cx, ax
    mov bx, y
    mov dx, bx
    add cx, w
    add dx, h

    drawhorline ax, cx, bx, color
    drawhorline ax, cx, dx, color
    drawverline ax, bx, dx, color
    drawverline cx, bx, dx, color

    popa
endm

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
    mov al, 0
    mov ah, 13h
    mov bp, offset titleMsg
    mov bh, 0
    mov bl, 7
    mov cx, titleLen
    mov dl, 17
    mov dh, 0
    int 10h

    mov al, 0
    mov ah, 13h
    mov bp, offset promptMsg
    mov bh, 0
    mov bl, 7
    mov cx, promptLen
    mov dl, 0
    mov dh, 27
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
    mov ax, cs
    mov es, ax
    mov ax, 0c207h
    mov bx, offset handler
    int 15h

    ; Enable mouse device handler with interrupt 15h
    ; Entry args:
    ; ax: 0c200h
    ; bh: 00h: disabled, 01h: enabled
    mov ax, 0c200h
    mov bh, 1
    int 15h

    popa
    ret

installHandler endp

listenEsc proc far
    mov al, 0

    .while al != 1bh
        mov ah, 10h
        int 16h
    .endw

    ; reset mouse device
    mov ax, 0c201h
    int 15h

    ; exit
    mov al, 02
    mov ah, 0
    int 10h
    mov ah, 4ch
    int 21h

listenEsc endp

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
    mov deltaY, dx
    add bx, 2
    mov dx, es:[bx]
    mov deltaX, dx
    add bx, 2
    mov dx, es:[bx]

    mov status, dl

    ;; store previous position
    mov ax, posX
    mov posXPrev, ax
    mov ax, posY
    mov posYPrev, ax

    ;; check button pressed
    mov ax, dx
    and ax, 1

    .if ax > 0 && leftPressed == 0
        ;; left button pressed
        mov leftPressed, 1
        drawrect 250, 160, 30, 30, 0ah
    .elseif ax == 0 && leftPressed == 1
        ;; left button released
        mov leftPressed, 0
        drawrect 250, 160, 30, 30, 0fh
    .endif
    mov ax, dx
    and ax, 2

    .if ax > 0 && rightPressed == 0
        ;; right button pressed
        mov rightPressed, 1
        drawrect 330, 160, 30, 30, 0ch
    .elseif ax == 0 && rightPressed == 1
        ;; right button released
        mov rightPressed, 0
        drawrect 330, 160, 30, 30, 0fh
    .endif

    ;; delta x
    mov dx, deltaX
    .if dx != 0
        mov al, status
        and al, 10h
        .if al > 0
            neg dl
            sub posX, dx
            .if posX <= 0
                mov posX, 0
            .endif
        .else
            add posX, dx
            .if posX >= 640
                mov posX, 640
            .endif
        .endif

    .endif

    mov dx, deltaY
    .if dx != 0
        mov al, status
        and al, 20h
        .if al > 0
            neg dl
            add posY, dx
            .if posY >= 480
                mov posY, 480
            .endif
        .else
            sub posY, dx
            .if posY <= 0
                mov posY, 0
            .endif
        .endif

    .endif

    mov ax, data
    mov es, ax
    mov ds, ax

    mov si, offset pixelCurr
    mov di, offset pixelPrev
    mov cx, mousePixelsLen * 2
    cld
    rep movsb

    call restorePixels
    call savePixels

showms:
    call drawMouse

    ; display mouse position
    pusha
    mov ax, data
    mov ds, ax
    mov es, ax

    mov ax, posX
    mov bx, offset infoPosX
    call updatePosStr

    mov ax, posY
    mov bx, offset infoPosY
    call updatePosStr

    ; display mouse position
    mov bp, offset infoPos
    mov cx, infoLen
    mov dh, 20
    mov dl, 25
    mov bh, 0
    mov al, 0
    mov bl, 7
    mov ah, 13h
    int 10h
    popa

    ret
handler endp

restorePixels proc far
    pusha

    mov ax, data
    mov ds, ax

    mov ax, mousePixelsLen
    shr ax, 1
    mov mousePixelsCnt, ax
    mov di, 0
    mov si, 0

    .while mousePixelsCnt > 0
        mov cx, posXPrev
        mov dx, posYPrev
        mov ax, mousePixels[di]

        movzx bx, ah
        add cx, bx
        movzx bx, al
        add dx, bx

        draw cx, dx, pixelPrev[si]

        inc si
        add di, 2
        dec mousePixelsCnt
    .endw

    popa
    ret
restorePixels endp

savePixels proc far
    pusha

    mov ax, data
    mov ds, ax

    mov ax, mousePixelsLen
    shr ax, 1
    mov mousePixelsCnt, ax
    mov di, 0
    mov si, 0

    .while mousePixelsCnt > 0
        mov cx, posX
        mov dx, posY
        mov ax, mousePixels[di]

        movzx bx, ah
        add cx, bx
        movzx bx, al
        add dx, bx

        mov ah, 0dh
        mov bh, 0
        int 10h
        mov pixelCurr[si], al

        inc si
        add di, 2
        dec mousePixelsCnt
    .endw

    popa
    ret
savePixels endp

drawMouse proc far
    pusha

    mov ax, data
    mov ds, ax

    mov ax, mousePixelsLen
    shr ax, 1
    mov mousePixelsCnt, ax
    mov di, 0
    mov si, 0

    ; draw mouse pixel by pixel
    .while mousePixelsCnt > 0
        mov cx, posX
        mov dx, posY
        mov ax, mousePixels[di]

        movzx bx, ah
        add cx, bx
        movzx bx, al
        add dx, bx

        .if cx < 640
            draw cx, dx, mouseColor[si]
        .endif

        add di, 2
        dec mousePixelsCnt
    .endw

    popa
    ret
drawMouse endp

updatePosStr proc far
    mov si, 3
    mov cx, 10

    .while si > 0
        mov dx, 0
        div cx
        add dl, 48
        dec si
        mov [bx][si], dl
    .endw

    ret
updatePosStr endp

main:
    call init

    drawframe 200, 120, 180, 80, 0fh
    drawrect 250, 160, 30, 30, 0fh
    drawrect 330, 160, 30, 30, 0fh
    drawhorline 290, 320, 230, 0fh

    call installHandler
    call listenEsc

code ends
end main
