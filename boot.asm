
%macro sleep 2
	; Use BIOS interrupt to sleep
	push dx
  push cx
  push ax
	mov ah, 86h
	mov cx, %1
	mov dx, %2
	int 15h
  pop ax
  pop cx
	pop dx
%endmacro


bits 16
org 0x7c00

start:
  xor ax, ax   ; make it zero
  mov ds, ax   ; DS=0
  mov ss, ax   ; stack starts at 0
  mov ax, 0x0040
  mov gs, ax  ; set gs to 0x0040
  mov sp, 0x7BF0   ; stack address
  mov ax, 0xb800  ; video memory
  mov es, ax  ; put videomem to es
  mov byte [disknum], dl ; save number of drive where we were booted from

  ;the whole RNG is copied from Nool's Haemolacria
  ;seed random number generator
	xor ax, ax
	int 1Ah ; get system time
	xor cx, dx
	mov [rng], cx ; use system time as rng seed

  call clear_screen_s ; clear screen and set it to 80x25

; [Welcome message] ==================================
  mov si, welcome ; print welcome message
  mov ah, 0x0F ; formatting for sprint
  xor bl, bl
  call sprint

; wait before going to "main menu"
  mov cl, 0
sleeploop:
  sleep 0x0, 30000 ;30 ms
  inc cl
  cmp cl, 40 ; 30ms * 40 = 1200ms
  jne sleeploop

  call clear_screen_s

  ; print out main menu
  xor bl, bl
  mov si, m1 ; first line
  mov ah, 0x04 ; red fg
  mov byte [ypos], 10
  mov byte [xpos], 30
  call sprint
  mov si, m2
  mov ah, 0x02 ; green fg
  mov byte [ypos], 11
  mov byte [xpos], 32
  call sprint
  mov si, m3
  mov ah, 0x0F ; white fg
  mov byte [ypos], 12
  mov byte [xpos], 29
  call sprint


; load other sectors from disk
loaddisk:
  xor ax, ax
  mov es, ax                      ; set to 0
	mov ah, 0x02                    ; load second stage to memory
	mov al, 12                      ; numbers of sectors to read into memory
  mov byte dl, [disknum]          ; sector read from fixed/usb disk
	mov ch, 0                       ; cylinder number
	mov dh, 0                       ; head number
	mov cl, 2                       ; sector number
	mov bx, kernel                  ; load into es:bx segment :offset of buffer
	int 0x13                        ; disk I/O interrupt

  jmp kernel ; goto kernel

welcome db 'running in real mode',0x0A,'bootloader initialized', 0x0A, 0
m1 db 'No step on snek game',0
m2 db 'made by: enp0s25',0
m3 db 'use WASD or arrow keys',0
m4 db 'press any key...',0
xpos db 0
ypos db 0
disknum db 0
rng dw 0

;========================================
; Calls
;========================================

clear_screen_l: ; set screen to 80x50 chars
  mov ax, 0x0000 ; set 40x25 chars
  int 0x10
  mov ah, 1   ; hide cursor
	mov cx, 0x2607
	int 0x10
  mov byte [xpos], 0  ;reset cursor
  mov byte [ypos], 0
  ret

clear_screen_s:
  mov ax, 0x0003 ;set 80x25 chars
  int 0x10
  mov ah, 1   ; hide cursor
	mov cx, 0x2607
	int 0x10
  mov byte [xpos], 0 ;reset cursor
  mov byte [ypos], 0
  ret

; bl=0 default, bl=1 fast, bl=2 keepBG
dochar:
  call cprint         ; print one character
sprint:
  or bl, bl ;check if bl=0
  jnz .nosleep
  sleep 0x0, 30000 ; sleep
.nosleep:
  lodsb      ; string char to AL
  cmp al, 0  ; is it zero?
  je .done   ; if yes, we're done
  cmp al, 0x0A ; check if newline
  jne dochar
  add byte [ypos], 1   ;down one row
  mov byte [xpos], 0   ;back to left
  jmp sprint
.done:
  ret

; print char
cprint:   
  mov cx, ax    ; save char/attribute
  movzx ax, byte [ypos]
  mov dx, 160   ; 2 bytes (char/attrib)
  mul dx      ; for 80 columns
  movzx dx, byte [xpos]
  shl dx, 1    ; times 2 to skip attrib

  mov di, 0        ; start of video memory
  add di, ax      ; add y offset
  add di, dx      ; add x offset
  cmp bl, 2 ;check if bl=2
  jne .nokeep
  mov byte ah, [es:di+1] ;load original formatting
  and ah, 0xF0 ; remove old FG
  and ch, 0x0F ; remove prevoius bg (if any)
  or ah, ch ; add new FG
  mov al, cl
  jmp .write

.nokeep:
  mov ax, cx        ; restore char/attribute
.write:
  stosw              ; write char/attribute
  add byte [xpos], 1  ; advance to right

  ret

gchar:
  mov ah, 1
  mov al, 0
  int 0x16   ; scan for keypress
  jz .done  ; ret if none
.clearbuf:
  ; push ax
  mov bx, [gs:001Ah] ; get head of keyboard buffer
  push word [gs:bx]
  mov word [gs:bx], 0x0000 ; zero out key
  xor ah, ah ; remove key from buffer
  int 0x16
  pop ax

.done:
  ret ; char in al

; print amount of free bytes in boot sector when compiling
%assign bootsectfree 510-($-$$)
%warning bootsectfree bytes free in bootsector

  times bootsectfree db 0 ; 1st sector padding
  dw 0AA55h ; some BIOSes require this signature

; =======================================================================
; [after boot sector] ===================================================
; =======================================================================

kernel:
; press any key
  mov ax, 0xb800 ; set vide mem back to es
  mov es, ax
  mov si, m4 ; last line of main menu
  mov ah, 0x0E ; yellow fg
  mov byte [ypos], 13
  mov byte [xpos], 32
  mov bl, 1
  call sprint
  
  ; clear keyboard buffer
  pushf
  cli
  mov ax,[gs:001Ah] ; keyboard buffer Head
  mov [gs:001Ch],ax ; keyboard buffer Tail
  popf

; last line blinking
blinkloop:
  mov cl, 0
sl2:
  sleep 0x0, 30000 ;30 ms
  call gchar ; scan for char
  or al, al
  jnz gamestart
  inc cl
  cmp cl, 15 ; 30ms * 40 = 1200ms
  jne sl2


; blinkloop:
  mov cl, 0
  mov di, 2144
clearline:
  mov byte [es:di], 0
  inc cl
  inc di
  cmp cl, 32
  jb clearline

  mov cl, 0
sl1:
  sleep 0x0, 30000 ;30 ms
  call gchar ; scan for char
  or al, al
  jnz gamestart
  inc cl
  cmp cl, 15 ; 30ms * 40 = 1200ms
  jne sl1

  mov si, m4
  mov ah, 0x0E
  mov byte [ypos], 13
  mov byte [xpos], 32
  mov bl, 1
  call sprint

  call gchar ; scan for char
  or al, al
  jnz gamestart
  jmp blinkloop

; =====================================================
; [Start game] ========================================
; =====================================================
gamestart:
  call clear_screen_l

; make walls ==========================================
; each line has its own loop
makewalls:
  mov di, 0
  mov ah, 0x70 ;grey bg
  mov al, '#'
.loop:
  mov word [es:di], ax
  add di, 2
  cmp di, 80
  jne .loop
  mov di, 1920
.loop1:
  mov word [es:di], ax
  add di, 2
  cmp di, 2000
  jne .loop1
  mov di, 0
.loop2:
  mov word [es:di], ax
  add di, 80
  cmp di, 1920
  jne .loop2
  mov di, 78
.loop3:
  mov word [es:di], ax
  add di, 80
  cmp di, 1998
  jne .loop3

mov byte [ypos], 0
mov byte [xpos], 24
mov si, scorestr
mov ah, 0x71
xor bl, bl
call sprint

; spawn the food
call xorshift ; do magic with rng
call RNG25 ; gen random number for Y position
mov [posy], ah
call RNG40 ; ge random number for X position
mov [posx], ah
call drawfood ; draw it

snakeinit:
; init snake
; let's pretend that it can't overwrite food :)
mov ax, 0x7020 ; space & grey bg
mov di, 996
mov word [es:di], ax
add di, 2
mov word [es:di], ax
add di, 2
mov word [es:di], ax
mov ah, 0x40 ; red bg head
add di, 2
mov word [es:di], ax

mainloop:
  xor ax, ax
  call gchar ; scan for char
  call printlchar ; print last pressed char to left corner + check if it is valid control char
  xor ax, ax

  ; RNG info in top center
  call xorshift ; do magik
  mov word ax, [rng] ; move rng state to ax
  mov word [reg16], ax ; then to reg16
  mov byte [ypos], 12 ; top line
  mov byte [xpos], 16 ; approx center
  mov ah, 0x70
  call printreg16 ; print it

  ; food info, right of RNG info
  ; (coords in hex)
  mov byte [xpos], 22
  mov byte [ypos], 12
  mov byte ah, [posx]
  mov byte al, [posy]
  mov word [reg16], ax
  mov ah, 0x70
  call printreg16

snakepront:
  call printsnek
  sleep 0x1, 0x86a0 ; 100 ms
  jmp mainloop  ; infinite loop

; in case I need to freeze stuff, usually debug
freeze:
  cli ;clear interrupt flag
  hlt ;stop CPU

; snake control
printlchar:
  mov word [reg16], ax
  or al, al ; check if 0
  jz .done
  push ax
  mov di, 0   ;first char
  mov ah, 0x70 ; grey bg
  stosw ; write ax to [es:di]
  pop ax
  cmp al, 'd' ; check if key is valid
  je .right ; and jump to code accordingly
  cmp ah, 0x4D
  je .right
  cmp al, 'a'
  je .left
  cmp ah, 0x4B
  je .left
  cmp al, 'w'
  je .up
  cmp ah, 0x48
  je .up
  cmp al, 's'
  je .down
  cmp ah, 0x50
  je .down
  jmp .done ; else skip setting direction
.right:
  cmp byte [movex], 255 ; don't allow 180 turn
  je .done
  mov byte [movex], 1 ; move by 1
  mov byte [movey], 0 ; don't move this way
  jmp .done
.left:
  cmp byte [movex], 1 ; don't allow 180 turn
  je .done
  mov byte [movex], 255 ; byte + 255 = byte - 1 because overflow
  mov byte [movey], 0 ; don't move this way
  jmp .done
.up:
  cmp byte [movey], 1 ; don't allow 180 turn
  je .done
  mov byte [movex], 0 ; don't move this way
  mov byte [movey], 255 ; byte + 255 = byte - 1 because overflow
  jmp .done
.down:
  cmp byte [movey], 255 ; don't allow 180 turn
  je .done
  mov byte [movex], 0 ; don't move this way
  mov byte [movey], 1 ; move by 1
  jmp .done
.done:
  mov byte [xpos], 0
  mov byte [ypos], 12
  mov ah, 0x70 ; grey bg
  call printreg16
  ret

; do the snek
printsnek:
; remove color from first square (make it grey)
  mov ax, 0
  mov bx, snakebody ; start of snake body info
  add word bx, [first] ; start + head index
  call getpos ; get position in video mem
  inc di ; don't change char, only attributes
  mov byte [es:di], 0x70 ; set attribute to grey bg

ps_calcnext:
; calculate next head position
  mov bx, snakebody
  add word bx, [first]
  mov byte al, [bx]
  add byte al, [movey]
  mov ah, al
  mov byte al, [bx+1]
  add byte al, [movex]
  cmp word [first], 1998 ; if we're at end of snake info buffer, start from beginning
  je .reset
  mov byte [bx+2], ah ; otherwise write to next 2 bytes
  mov byte [bx+3], al
  add word [first], 2
  jmp col
.reset:
  mov bx, snakebody
  mov byte [bx], ah
  mov byte [bx+1], al
  mov word [first], 0
col:
  jmp collision ; check for collision


ps_remlast:
; remove last square
  mov bx, snakebody
  add word bx, [last]
  call getpos
  cmp byte [es:di], '@' ; if food spawned inside snake, don't delete it
  je .check
  inc di
  mov byte [es:di], 0 ; otherwise make it black
.check:
  ; check if we're at the end of buffer
  cmp word [last], 1998
  je .reset
  add word [last], 2
  jmp ps_drawhead
.reset:
  mov word [last], 0

ps_drawhead:
; draw head
  xor ax, ax
  mov bx, snakebody
  add word bx, [first]
  call getpos ; get new pos of head in video mem
  mov word [es:di], 0x4020 ; overwrite with space and red bg

  ret

getpos:
  mov byte al, [bx] ; Y position
  mov dl, 80
  mul dl ; al*80
  mov cx, ax ; save for later
  mov ah, 0
  mov byte al, [bx+1] ; X position
  shl al, 1 ; al*2
  add ax, cx 
  mov di, ax ; Y*80 + X*2
  ret

collision:
  mov ax, 0
  mov bx, snakebody ; start of snake body
  add word bx, [first] ; start + head index
  call getpos ; get future head position in video mem
  inc di ; inc to check attributes
  mov word ax, [es:di]
  cmp al, 0x02 ; green fg = food
  je .grow
  and al, 0xF0
  cmp al, 0x70 ; grey bg = wall or snake body
  je death
  jmp ps_remlast ; continue in printing snake if there isn't collision
.grow:
  ; generate new food
  call xorshift
  call RNG25
  mov [posy], ah
  call RNG40
  mov [posx], ah
  call drawfood
  inc word [score]
  ; add points
  mov byte [ypos], 0
  mov byte [xpos], 30
  mov word ax, [score]
  mov word [reg16], ax
  mov ah, 0x71  ; grey bg, blue fg
  call printreg16
  jmp ps_drawhead ; draw snake head (skip removing last square)

death:
  mov si, deathmsg
  mov ah, 0x04 ; red fg
  mov byte [ypos], 5
  mov byte [xpos], 16
  mov bl, 2
  call sprint ; print "You Died"
  
  ; jmp $

  sleep 0x3, 0x0d40 ; 200 ms

  ; clear keyboard buffer
  mov di, 001Eh
  xor cl, cl
  pushf
  cli
  mov ax,[gs:001Ah] ; keyboard buffer Head
  mov [gs:001Ch],ax ; keyboard buffer Tail
  clearloop:
    mov byte [gs:di], 0
    inc di
    inc cl
    cmp cl, 31
    jne clearloop
  popf

  xor ah, ah
  int 0x16 ; wait for keypress

  mov cx, 20  ; number of bytes to copy
  mov si, dinit ; source data
  push es ; save es
  xor ax, ax
  mov es, ax ; set es to 0
  mov di, score ; start of data to rewrite
  call memcpy
  pop es ; restore es
  call xorshift ; do RNG
  jmp gamestart ; start games
  ; jmp freeze ; freeze CPU

memcpy:
  lodsb ; load data from ds:si to al, inc si
  stosb ; save data from al to es:di, inc di
  dec cx
  jne memcpy
  ret

drawfood:
  ;check if it's going to spawn in head
  xor ax, ax
  mov bx, snakebody
  add word bx, [first] ;head
  call getpos ; get new pos of head in video mem
  mov dx, di ; save for later
  xor ax, ax
  mov bx, posy
  call getpos ; get position in video mem where to draw it
  cmp dx, di
  je .regen
  mov al, '@' ; food symbol
  mov ah, 0x02 ; green fg
  mov word [es:di], ax
  ret
.regen:
  call xorshift
  call RNG25
  mov [posy], ah
  call RNG40
  mov [posx], ah
  jmp drawfood


; xorshift copied from Nool's Haemolacria
; pretty much magik
xorshift:
	push bx
	mov bx, [rng]
	shl bx, 7
	xor [rng], bx
	mov bx, [rng]
	shr bx, 9
	xor [rng], bx
	mov bx, [rng]
	shl bx, 8
	xor [rng], bx
	pop bx
	ret

RNG25:
  call xorshift ; do magik
  xor ax, ax
  mov byte al, [rng] ; move first byte of RNG state
  cmp al, 253 ; make sure all numbers have equal chance
  jge RNG25 ; gen again if the chances aren't equal
  mov cl, 23
  div cl ; ax / 23, quotient in al, remainder in ah
  inc ah ; inc remainder (avoid spawning in wall)
  ret

RNG40:
  call xorshift ; do magik
  xor ax, ax
  mov byte al, [rng] ; move first byte of RNG state
  cmp al, 228 ; make sure all numbers have equal chance
  jge RNG40 ; gen again if the chances aren't equal
  mov cl, 38
  div cl ; ax / 38, quotient in al, remainder in ah
  inc ah ; inc remainder (avoid spawning in wall)
  ret


;------------------------------------
; DEBUG registers
printreg16:
  push ax
  mov di, outstr16 ; buffer for hex string
  mov ax, [reg16] ; content to write as hex
  mov si, hexstr ; hex chars
  mov cx, 4   ;four places
hexloop:
  rol ax, 4   ;leftmost will
  mov bx, ax   ; become
  and bx, 0x0f   ; rightmost
  mov bl, [si + bx] ;index into hexstr
  mov [di], bl
  inc di
  dec cx
  jnz hexloop

  mov si, outstr16
  ;mov ah, 0x70 ; grey bg
  pop ax
  mov bl, 1
  call sprint ; print it

  ret
;------------------------------------

; mainly data
hexstr   db '0123456789ABCDEF' ; hex chars
outstr16   db '0000', 0  ;register value string
reg16   dw    0  ; pass values to printreg16
deathmsg db 'You died',0
scorestr db 'Score:0000',0
nool db 0

; data backup
dinit db 0,0,0,0,1,0,8,0,0,0,12, 17, 12, 18, 12, 19, 12, 20, 12, 21

score dw 0
posy db 0 ; food pos Y
posx db 0 ; food pos X
movex db 1 ; movement on X
movey db 0 ; movement on Y
first dw 8 ; head index in snakebody
last dw 0 ; tail index in snakebody
snakebody db 12, 17, 12, 18, 12, 19, 12, 20, 12, 21 ; first bytes of snakebody arr
times 1990 db 0 ; the rest of snake body buffer (even tho it doesn't have to be defined)

; If I want to keep this under 4kB, tell me how much is free
; don't forget to add 1990 to have real free bytes
%assign physsector 4096-($-$$)
%warning physsector bytes free in 4kB physical sector
; sector padding
times physsector db 0
