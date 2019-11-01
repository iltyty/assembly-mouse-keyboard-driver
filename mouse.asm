title Mouse driver under DOSBox 0.74

.486
assume ds: data, cs: code

data segment use16
titleMsg byte "Welcome to the mouse driver program!", 0ah, 0dh, 0
        byte "Here are the list of implemented functions: ", 0ah, 0dh, 0
        byte "Mouse position display, left/right mouse button click reponse", 0ah, 0dh, 0
        byte "Press Esc or to quit the program", 0ah, 0dh, 0
titleLen equ $ - titleMsg

data ends


stack segment use16

stack ends


code segment use16

start:

; initiate procedure
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
    mov ah, 06h
    mov al, 0
    mov bh, 0
    mov ch, 0
    mov cl, 0
    mov dh, 24
    mov dl, 79
    int 10h

    ; ------------------------------------
    ; Set display mode with 10h interrupt
    ; Entry args:
    ; ax: 107h: 1280*1024, 256-color mode
    ; ------------------------------------
    mov ax, 107h
    int 10h

    ; -----------------------------------------------------
    ; Display string under teletype mode with 10h interrupt
    ; Entry args:
    ; ah: 13h
    ; al: Display output mode
    ; bl: Display property(with al = 00h or 01h)
    ; bh: page number
    ; cx: Length of the target string
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

    call init

code ends
end start
