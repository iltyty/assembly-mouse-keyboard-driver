title Mouse driver under DOSBox 0.74

.486
assume ds: data, cs: code

data segment use16
titleMsg byte " Welcome to the mouse driver program!", 0ah, 0dh, 0
         byte "Here are the list of implemented functions: ", 0ah, 0dh, 0
         byte "Mouse position display, left/right mouse button click reponse.", 0ah, 0dh, 0
         byte "Press Esc or to quit the program.", 0ah, 0dh, 0
titleLen equ $ - titleMsg

; x coordinate of mouse pointer
posX word 0
; y coordinate of mouse pointer
posY word 0
; display the mouse position
infoPos  byte "Mouse position: ("
; display posX (min: 0, max: 640)
infoPosX byte 0, 0, 0, ", "
; display posY (min: 0, max: 480)
infoPosY byte 0, 0, 0, ")"
; length of the mouse position string
infoLen  equ $ - infoPos

data ends


stack segment use16

stack ends


code segment use16

; init procedure
init proc far
    pusha

    ; ---------------------------------------------------
    ; Initiate the screen with 10h interrupt
    ; Entry args:
    ; ah:     06h: Scroll up, 07h: Scroll down
    ; al:     Rolling lines(0: clear the window)
    ; bh:     Default value for the blank space
    ; cl, ch: The upper-left corner of the window(x, y)
    ; dl, dh: The lower-right corner of the window(x, y)
    ; ---------------------------------------------------
    mov ah, 6
    mov al, 0
    mov bh, 7
    mov ch, 0
    mov cl, 0
    mov dh, 24
    mov dl, 79
    int 10h

    ; ------------------------------------
    ; Set display mode with 10h interrupt
    ; Entry args:
    ; ax: 12h: 640*480, 16-color mode
    ; ------------------------------------
    mov ah, 00h
    mov al, 12h
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
    mov ax, data
    mov es, ax
    mov al, 0
    mov ah, 13h
    mov bh, 0
    mov bl, 7
    mov bp, offset titleMsg
    mov cx, titleLen
    mov dh, 0
    mov dl, 0
    int 10h

    popa
    ret

init endp


; mouse device handler procedure
; stack status:
; mouse device status, status (sp + 10)
; movement in x axis, deltaX (sp + 8)
; movement in y axis, deltaY (sp + 6)
; return address (sp + 4)

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
    local status: word, deltaX: word, deltaY: word
    pusha

    mov ax, ss
    mov es, ax
    mov bx, sp

    add bx, 10
    mov di, es:[bx]
    mov status, di
    sub bx, 2
    mov di, es:[bx]
    mov deltaX, di
    sub bx, 2
    mov di, es:[bx]
    mov deltaY, di

    ; Check if any button is clicked
    .if dx == 1
        ; right button clicked
        ; TODO
    .elseif dx == 0
        ; left button clicked
        ; TODO
    .endif

    ; Check the mouse move event
    .if deltaX != 0
        ; movement in x axis
        .if status == 10h
            ; deltaX < 0
            mov dx, deltaX
            sub posX, dx
            .if posX < 0
                mov posX, 0
            .endif
        .else
            ; deltaX > 0
            mov dx, deltaX
            add posX, dx
            .if posX > 638
                mov posX, 638
            .endif
        .endif
    .elseif deltaY != 0
        ; movement in y axis
        .if status == 20h
            ; deltaY < 0
            neg deltaY
            mov dx, deltaY
            add posY, dx
            .if posY > 480
                mov posY, 480
            .endif
        .else
            ; deltaY > 0
            mov dx, deltaY
            sub posY, dx
            .if posY < 0
                mov posY, 0
            .endif
        .endif
    .endif

    call updatePosInfo

    ; Display the mouse position with interrupt 10h
    ; Entry arg: see init procedure
    mov ax, data
    mov es, ax

    mov ah, 13h
    mov al, 0
    mov bh, 0
    mov bl, 0
    mov bp, offset infoPos
    mov cx, infoLen
    mov dh, 6
    mov dl, 23
    int 10h

    popa
    ret

handler endp

; Update the display string of mouse position
updatePosInfo proc far
    ; base: the base in which the position info will be displayed, 10 in default
    ; digits: the digits of the infoPosX/infoPosY string's position buffer
    local base: word, digits: word
    mov base, 10
    mov digits, 3
    pusha

    mov ax, data
    mov es, ax

    mov ax, posX
    mov bx, offset infoPosX
    mov dx, 0
    .while digits != 0
        div base
        ; transfer from numeric to ASCII character
        add dl, 48
        dec digits
        mov di, digits
        mov [bx][di], dl
    .endw

    mov digits, 3
    mov ax, posY
    mov bx, offset infoPosY
    mov dx, 0
    .while digits != 0
        div base
        ; transfer from numeric to ASCII character
        add dl, 48
        dec digits
        mov di, digits
        mov [bx][di], dl
    .endw

    popa
    ret
updatePosInfo endp

nullHandler proc far
    ret
nullHandler endp

; Install mouse device handler with interrupt 15h
; Entry args:
; ax:    0c207h: set device handler address
; es:bx: far user device handler or 0000h:0000h to cancel
installHandler proc far
    pusha

	mov ax, 0c201h
	int 15h

    mov ax, seg nullHandler
    mov es, ax
    mov ax, 0c207h
    mov bx, offset nullHandler
    int 15h
    ; Installation failed if CF == 1
    jc errInstall

    ; Installation succeeded
    ; Enable ps/2 mouse device with interrupt 15h
    ; Entry args:
    ; ax: 0c200h: pointing device BIOS interface (ps)
    ; bh: 00h: disabled, 01h: enabled
    mov ax, 0c200h
    mov bh, 1
    int 15h
    ; Enable failed if CF == 1
    jc errEnable

errInstall:
    mov ax, data
    mov es, ax
    mov al, 0
    mov ah, 13h
    mov bh, 0
    mov bl, 7
    mov bp, offset titleMsg
    mov cx, titleLen
    mov dh, 30
    mov dl, 30
    int 10h
    popa
    ret

errEnable:

    mov al, 02h
    mov ah, 0
    int 10h
    mov ah, 4ch
    int 21h

    popa
    ret

installHandler endp

main:
    call init
    ; call installHandler

code ends
end main
