title keyboard driver

include Irvine16.inc

.data
prompt db "keyboard driver display :) $"
PIC_CMD         EQU     20h
NONSPEC_EOI     EQU     20h

.code
public keys
keys db 256 dup(0)

buffer db 64 dup(0)

buffertail db 0      ; point to buffer tail
cursorposition db 0  ; point to position of cursor
wordcolor db 7
flagCapLock db 0     ; if CapLock is open

;--------------------------------------
; toASCII: transfer table between scancode and AscII
; toASCII[scancode] = AscII
; e.x. toASCII[1] = 27, key'Esc' scancode = 1, AscII = 27
;--------------------------------------
toASCII db 0,27,'1234567890-=',0,9
		db 'qwertyuiop[]',10,0,'as'
		db 'dfghjkl;',39,96,0,92,'zxcv'
		db 'bnm,./',0,'*',0,' ',0,0,0,0,0,0
		db 0,0,0,0,0,0,0,0,0,0,'-',0,0,0,'+',0
		db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		db 0,27,'!@#$%^&*()_+',0,9
		db 'QWERTYUIOP{}',13,0,'AS'
		db 'DFGHJKL:"',126,0,124,'ZXCV'
		db 'BNM<>?',0,'*',0,' ',0,0,0,0,0,0
		db 0,0,0,0,0,0,0,'789-456+1'
		db '230.',0,0,0,0,0,0,0,0,0,0,0,0
		db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

keyboard_handler_set proc
    ; set keyboard_handler as new interrupt 09h
    
    ; cli/sti: shut down/start listen to interrupt
    cli
    push ds

    ;---------------------------------
    ; int 21h/ah = 25h: set new interrupt
    ; al = type of interrupt
    ; ds: dx = interrupt vector
    ;---------------------------------
    mov ax, @code
    mov ds, ax
    mov ah, 25h
    mov al, 09h                      ; rerwite 09h interrupt
    mov dx, offset keyboard_handler
    int 21h

    pop ds
    sti
    ret
keyboard_handler_set endp

keyboard_handler proc far
    pushad
    ;---------------------------------
    ; in/out: get info from external port 0-255
    ; only al/ax
    ; 256-65535 port use mov to get info, only dx, then in/out between dx and ax
    ;---------------------------------

    ; port 60h: keyboard
    ; port 61h: sound (key button pressed)
    ; 开放键盘响应：
    in al, 61h
    and al, 7fh
    out 61h, al
    ; 屏蔽键盘响应：
    ; in al,61h
    ; or al,80h
    ; out 61h,al

    in al, 60h   ; get scancode in al
                 ; sign byte 0: button down, 1: up
    mov ah, al
    mov bx, 0
    mov bl, ah              ; store scancode in bl and ah
    and bl, 01111111b       ; bl has no sign byte

    rol ah, 1               ; store sign byte in CF
    jc keyrelease

keypress:
    mov keys[bx], 1         ; No.bx key is pressed
    xor ax, ax

    ;---------------------------------
    ; special key check
    ; CapLock, NumLock, Shift
    ;---------------------------------
    mov al, keys[45h]     ; if NumLock
    mov ah, keys[3ah]     ; if CapLock
    mov bh, keys[2ah]     ; if LShift
    or bh, keys[36h]      ; if RShift


keycheck:
    ;---------------------------------
    ; 1:Esc   2-0B:1234567890    0C:-_   0D:=+   0E:BackSpace
    ; 0F:Tab   10-19:qwertyuiop   1A:[{   1B:]}   1C:Enter   1D:Ctrl
    ; 1E-26:asdfghjkl   27:;:   28:'"   29: `~   2A:LShift  2B:\|
    ; 2C-32:zxcvbnm   33:,<   34:.>   35:/?   36:RShift
    ; 37:*(Num)   38:Alt   39:SpaceBar  3A:CapsLock
    ; 3B-44: F1-F10(85:F11, 86:F12)

    ;-----37, 45-53:NumberKeyboard----
    ; 45:NumLock   46:ScrollLock
    ; 47:Home&7  48:ArrorUp&8  49:PgUp&9  4A:-  4B:ArrowLeft&4
    ; 4C:5  4D:ArrowRight&6  4E:+  4F:End&1  50:ArrowDown&2  51:PgDn&3
    ; 52:Ins&0  53:Del&.

    ; 54-5D:(SH+F1-F10)(87:SH+F11, 88:SH+F12)
    ; 5E-67:(Crtl+F1-F10)(89, 8A:Crtl+F11, F12)
    ; 68-71:(Alt+F1-F10)(8B, 8C:Alt+F11, F12)
    ;---------------------------------
    cmp bl, 02h
    jb handlerfinished        ; Esc pressed
    cmp bl, 0dh
    jbe keys_CapLock          ; 1234567890-=
    cmp bl, 10h
    jb keys_Command           ; Backspace Tab
    cmp bl, 1ch
    jbe keys_CapLock          ; qwertyuiop[]
    cmp bl, 1eh
    jb keys_Command           ; Crtl
    cmp bl, 29h
    jbe keys_CapLock          ; asdfghjkl;'`
    cmp bl, 2bh
    jb keys_Command           ; LeftShift
    cmp bl, 35h
    jbe keys_CapLock          ; zxcvbnm,./
    cmp bl, 39h
    je keys_CapLock           ; ' '
    cmp bl, 47h
    jb keys_Command           ; RightShift SpaceBar F CapsLock NumLock
    cmp bl, 53h
    jbe keys_NumLock          ; NumberKeyboard/*-789+4561230.

    jmp handlerfinished


keys_CapLock:
    ; keys needed to consider CapLock is pressed
    xor bh, ah                ; if capitalized
    jmp keys_toASCII

keys_NumLock:
    ; keys needed to consider NumLock is pressed
    cmp al, 1
    jb keys_Command           ; NumLock off

    mov bh, al
    jmp keys_toASCII

keys_Command:
    cmp bx, 0eh               ; BackSpace pressed
    je key_backspace
    cmp bx, 4bh               ; LeftArrow pressed
    je key_leftarrow
    cmp bx, 4dh               ; RightArrow pressed
    je key_rightarrow
    cmp bx, 3bh               ; F1 pressed
    je key_F1
    cmp bx, 3ch               ; F2 pressed
    je key_F2
    cmp bx, 3dh               ; F3 pressed
    je key_F3
    cmp bx, 3eh               ; F4 pressed
    je key_F4
    cmp bx, 3fh               ; F5 pressed
    je key_F5
    cmp bx, 40h               ; F6 pressed
    je key_F6
    cmp bx, 41h               ; F7 pressed
    je key_F7
    cmp bx, 42h               ; F8 pressed
    je key_F8
    cmp bx, 43h               ; F9 pressed
    je key_F9
    cmp bx, 44h               ; F10 pressed
    je key_F10
    cmp bx, 85h               ; F11 pressed
    je key_F11
    cmp bx, 86h               ; F12 pressed
    je key_F12
    ; cmp bx, 1ch             ; Enter pressed
    ; je key_Enter

    jmp handlerfinished

    ;---------------------------------
    ; set sound tone of F1-F12
    ;---------------------------------
key_F1:
    mov di, 196
    jmp soundplay
key_F2:
    mov di, 262
    jmp soundplay
key_F3:
    mov di, 294
    jmp soundplay
key_F4:
    mov di, 330
    jmp soundplay
key_F5:
    mov di, 349
    jmp soundplay
key_F6:
    mov di, 392
    jmp soundplay
key_F7:
    mov di, 440
    jmp soundplay
key_F8:
    mov di, 494
    jmp soundplay
key_F9:
    mov di, 523
    jmp soundplay
key_F10:
    mov di, 587
    jmp soundplay
key_F11:
    mov di, 349
    jmp soundplay
key_F12:
    mov di, 392
    jmp soundplay

    jmp handlerfinished

soundplay:
    ;---------------------------------
    ; sound play, use Counter2(0b6h)
    ;---------------------------------
    ; initialize counter2
    mov al, 0b6h
    out 43h, al               ; 43h: public counter

    ; calculate value of counter
    mov dx, 12h
    mov ax, 34dch             ; count number->ax
    div di

    out 42h, al
    mov al, ah
    out 42h, al

    ; open loudspeaker
    in al, 61h
    mov ah, al
    or al, 3
    out 61h, al

    ; time delay
wait1:
    mov cx, 6000
delay1:
    loop delay1

    ; stop loudspeaker
    dec bx
    jnz wait1
    mov al, ah
    out 61h, al
    
    jmp handlerfinished

key_backspace:
    cmp [cursorposition], 0
    je handlerfinished        ; no buffer

    mov bx, 0
    mov cx, 0

    mov cl, [buffertail]
    sub cl, [cursorposition]
    cmp cl, 0
    je delete_end             ; cursor is at the end

    mov bl, [cursorposition]
    dec bl
    ; a a a a a a a a a a a a a a a a _
    ;             ^ ^               ^ ^
    ;             | |---------------| |
    ;            bl cp      cx        bt    
moveleft:
    inc bl
    mov dl, buffer[bx]
    dec bl
    mov buffer[bx], dl
    inc bl
    loop moveleft

delete_end:
    ; delete last word as 0
    dec [cursorposition]
    mov bl, [buffertail]
    dec bl
    mov buffer[bx], 0

    ;---------------------------------
    ; int 10h/ah = 03h
    ; get cursor position in dl
    ;---------------------------------
    mov bh, 0
    mov ah, 03h
    int 10h

    ;---------------------------------
    ; int 10h/ah = 02h
    ; reset cursorposition as dl
    ;---------------------------------
    dec dl
    mov ah, 02h
    int 10h

    ;---------------------------------
    ; int 10h.ah=13h print
    ; ES:BP print bufffer
    ;---------------------------------
    mov ax, @code
	mov es, ax
	mov ah, 13h
	xor cx, cx
	mov cl, [buffertail]
	sub cl, [cursorposition]

	push ax
	xor ax, ax
	mov ax, offset buffer
	add al, [cursorposition]
	mov bp, ax
	pop ax

	mov al, 0
	mov bl, [wordcolor]
	int 10h

	dec [buffertail]
	jmp handlerfinished

key_leftarrow:
    cmp [cursorposition], 0   ; cursor is on the left
    je handlerfinished

    mov ah, 03h
    mov bh, 0
    int 10h

    mov ah, 02h
    dec dl
    int 10h

    dec [cursorposition]
    jmp handlerfinished

key_rightarrow:
    mov bl, [cursorposition]
    cmp bl, [buffertail]
    je handlerfinished        ; cursor is on the right

    mov ah, 03h
    mov bh, 0
    int 10h

    mov ah, 02h
    inc dl
    int 10h

    inc [cursorposition]
    jmp handlerfinished

keys_toASCII:
    ;---------------------------------
    ; keys needed to be displayed(not command)
    ; change their scancode to ASCII
    ;---------------------------------
    ror bh, 1
    add bl, bh
    xor bh, bh
    mov al, toASCII[bx]

    ; Enter key comes here
    cmp al, 0
    jne buffercheck
    jmp handlerfinished

buffercheck:
    ;---------------------------------
    ; check if buffer is full
    ; if full, only Enter key is allowed
    ;---------------------------------
    cmp [buffertail], 62
    jne moveall
    cmp al, 10                ; buffer is full and no Enter key
    jne handlerfinished

moveall:
    ;---------------------------------
    ; if cursor is not at the end, move letters between
    ; cursorposition and buffertail 1 position right
    ;---------------------------------
    xor bx, bx
    mov bl, [buffertail]
    sub bl, [cursorposition]
    cmp bl, 0
    je inputbuffer            ; cursorposition at the end
    mov cx, bx
    mov bl, [buffertail]
move:
    dec bl
    mov dl, buffer[bx]
    inc bl
    mov buffer[bx], dl
    dec bl
    loop move

inputbuffer:
    ;---------------------------------
    ; put the input at the cursorposition
    ;---------------------------------
    mov bx, 0
    mov bl, [cursorposition]
    mov buffer[bx], al
    inc bx
    inc [buffertail]
    cmp al, 10
    jne print
    mov buffer[bx], 13        ; enter pressed
    inc bx
    mov [buffertail], bl

print:
    ;---------------------------------
    ; int 10h/ah=13h read/write double-byte character
    ; ES:BP buffer for characters
    ; output string in buffer
    ;---------------------------------
    mov ax, @code
    mov es, ax
    mov bh, 0
    mov ah, 03h
    int 10h
    mov ah, 13h
    mov cx, 0
    mov cl, [buffertail]
    sub cl, [cursorposition]

    push ax
    mov ax, 0
    mov ax, offset buffer
    add al, [cursorposition]
    mov bp, ax
    pop ax

    mov al, 0
    mov bl, [wordcolor]
    int 10h

    mov bx, 0
    mov bl, [buffertail]
    dec bl
    ; reset cursor if Enter pressed
    cmp buffer[bx], 13
    jne setcursor

    ; Enter pressed, reset buffertail and cursorposition
    mov [buffertail], 0
    mov [cursorposition], 0

setcursor:
    ;---------------------------------
    ; reset cursor at dl
    ; if Enter pressed, cursorposition = 0
    ; else cursorposition + 1
    ;---------------------------------
    mov bh, 0
    mov ah, 03h
    int 10h
    cmp buffer[bx], 13
    jne set1
    inc dh
    mov dl, 0
    jmp set2
set1:
    inc dl
    inc [cursorposition]
set2:
    mov ah, 02h
    int 10h
    jmp checkrowfull

keyrelease:
    mov keys[bx], 0

checkrowfull:
    ;---------------------------------
    ; too many rows
    ; clear the screen and reset cursor
    ;---------------------------------
    mov ah, 03h
    int 10h
    cmp dh, 20
    jb handlerfinished
    mov ax, 03h
    int 10h

handlerfinished:
    mov al, NONSPEC_EOI
    out PIC_CMD, al

    popad
    iret

keyboard_handler endp

main proc
    ; mov ax, data
    ; mov es, ax
    ;---------------------------------
    ; int 21h/ah = 09h: string display
    ; dx->addr of string
    ;---------------------------------
    ; mov dx, offset prompt
    ; mov ah, 09h
    ; int 21h

    ;---------------------------------
    ; int 10h/ah = 03h: get cursor info
    ; out: ch: start line; cl: end line;
    ;      dh: Y; dl: X
    ;---------------------------------

    mov ax, 03h
    int 10h
    call keyboard_handler_set
    exit
main endp
end main