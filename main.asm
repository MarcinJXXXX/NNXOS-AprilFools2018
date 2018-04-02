[org 0x7c00]
[bits 16]

PART equ board+4096


board equ add_board+10
widht equ PART+12
X equ widht + 4
height equ widht+8
Y equ height + 2 
PRTB equ PART + 64
hB equ PRTB + 4
wB equ PRTB + 8
yB equ PRTB + 12
xB equ PRTB + 16

jmp main

main:
xor ax, ax
mov ds, ax
mov es, ax
mov [BOOT_DRIVE], dl
mov ax, 0x07E0                  


mov ah, 10h
mov al, 3
mov bh, 0
int 10h

cli
mov ss, ax
mov sp, 0x1200                 		
sti

call ClearScreen

mov si, loadingModules
call PrintString

call CollectDriveInfo

call LoadSectors

jmp halt

ClearScreen:
	mov ah, 2
	mov bh, 0
	mov dx, 0
	int 10h
	mov ah, 9
	mov al, ' '
	mov bh, 0
	mov bl, 0x2F
	mov cx, 0x7D0
	int 10h
	mov ah, 2
	mov bh, 0
	mov dx, 0
	int 10h
	ret

PrintString:
	pusha
.loop:
	lodsb
	cmp al, 0
	je .end
	mov ah, 0xe
	xor bx, bx
	int 10h
	jmp .loop
.end:
	popa
	ret

	
halt:
	hlt
	jmp halt
	
LoadSector:
	pusha
	
	; push bx
	; xor dx, dx
	; mov bx, [SPH]
	; div bx
	; push ax
	
	; mov ax, dx
	; xor dx, dx
	; mov bx, [head]
	; div bx
	
	; mov ch, dl
	; xor dx, dx
	; mov dh, al
	; pop ax
	; mov dl, [BOOT_DRIVE]
	
	; mov cl, al
	; mov ah, 2
	; mov al, 1
	; pop bx
	; int 13h
	
	
	; dec ax
	
	; push bx
	; xor dx, dx
	; mov cx, [SPH]
	; div cx
	; xchg bx, bx
	; push dx
	; xor dx, dx
	; mov cx, [head]
	; div cx
	; xchg bx, bx
	; pop cx
	; push ax
	; mov al, 1
	; mov ah, 2
	; pop bx
	; mov ch, bl
	; mov dh, dl
	; mov dl, [BOOT_DRIVE]
	; pop bx
	; inc cl
	; xchg bx, bx
	
	mov cl, al
	mov dl, [BOOT_DRIVE]
	mov dh, 0
	mov ch, 0
	mov ah, 2
	mov al, 1
	
	int 13h
	;xchg bx, bx
	
	popa
	ret
	
sectors: dd 0	

LoadSectors:
	mov byte [sectors], 7
    mov ch, 0x00            
    mov dh, 0x00            
    mov cl, 0x02    
	mov dl, [BOOT_DRIVE]
	mov bx, 0x7e00

.next:
    mov di, 5

.again: 
    mov ah, 0x02           
    mov al, [sectors]
    int 0x13
    jc   .retry
    sub [sectors], al 
    jz  .ready
    mov cl, 0x01           
    xor dh, 1               
    jnz .next
    inc ch                    
    jmp .next

.retry:
    mov ah, 0x00            
    int 0x13
    dec di
    jnz .again
    jmp .error

.ready:
	jmp NEW

.error:
    mov ah, 0x0e
    mov al, 'E'
    int 0x10
    jmp $

DISK_ERROR_MSG: db "disk error", 0

CollectDriveInfo:
	push ax
	
	mov ax, 0
	mov ds, ax
	
	pop ax
	
	pusha
	mov ah, 48h
	mov dl, [BOOT_DRIVE]
	mov si, DRIVE_INFO
	int 13h

	mov ax, [cyl]
	mov cx, [head]
	mul cx
	mov bx, ax
	
	mov ax, [sec]
	xor dx, dx
	div bx

	mov [SPH], ax
	
	popa
	ret
	

	
	
data:
	BOOT_DRIVE: db 0
	loadingModules: db 'Loading modules... please wait...',0xa,0xd,0
DRIVE_INFO:
	dw 1eh
	dw 0
	cyl: dd 0
	head: dd 0
	spt: dd 0
	sec: dq 0
	dw 0
	dq 0
SPH: dw 0

times 510 - ($ - $$) db 0
dw 0xaa55
data2:
itoa_16_data:
	digit db "0123456789ABCDEF",0
	p dw 0
	shifter dw 0
	i dw 0
	base dw 0
	hmc dw 0

__itoa_16:

	push bp
	mov bp, sp
	add bp, 2
	mov ax, [bp+8]
	mov [i], ax
	mov [shifter], ax
	mov ax, [bp+6]
	mov [p], ax
	mov ax, [bp+4]
	mov [base], ax
	mov ax, [bp+2]
	mov [hmc], ax
	
	pusha
	cmp ax, 0
		je .case0
	mov bx, [bp+6]
.casenot0:
	
	mov byte [bx], '0'
	mov bx, [p]
  	inc bx
  	mov [p], bx 

   
 	mov ax, [hmc]
 	dec ax
 	mov [hmc], ax
	
  	cmp ax, 0
  	jne .casenot0
	jmp .end_if
.case0:
	.loop0:

   	        mov bx, [p]
   	        inc bx
  	        mov [p], bx 
   
           	mov ax, [shifter]
   	        xor dx, dx
   	        mov bx, [base]
           	div bx
   	        mov [shifter], ax
			
   	        cmp ax, 0
   		        jne .loop0
.end_if:
	popa
	pusha
	mov bx, [p]
	mov byte [bx], 0
	mov [p], bx

	popa
	pusha
	.loop1:
		mov bx, [p]
		dec bx
		mov [p], bx
		
		mov ax, [i];i
		xor dx, dx
		mov cx, [base]
		div cx
		
		mov bx, [p]
		add dx, digit
		mov si, dx
		mov cl, [si]
		mov byte [bx], cl
		mov [i], ax
		cmp ax, 0
		jne .loop1
	popa
	pop bp
	
	ret

hello: db "(NNXFSv01) HDDA: Loading $krnl",0xa,0xd,0

row: db 0

new_part_timer: db 0


Tick:
	pusha

	mov si, add_board
	xor ax, ax	
.loop7:
	cmp ax, 10
		je .endloop7
	
	cmp byte [si], 0
		jne gameOver
	
	inc si
	
	inc ax
	jmp .loop7
.endloop7:
	
	mov byte [__down], 1
	cmp byte [new_part_timer], 0
		jg .decrease
	mov si, board

	
	call GenNewPart
	mov byte [new_part_timer], 20
	
	jmp .over3
.decrease:
	;dec byte [new_part_timer]
.over3:
	popa
down:
	pusha
	mov byte [__down], 1
	
	xor cl, cl
.loop1:
	cmp cl, [height]
		je .endloop1
	mov bl, 0
.loop2:
	cmp bl, [widht]
		je .endloop2
	

	
	push cx
	push bx
	mov si, PART
	mov al, cl
	mov ah, [widht]
	mul ah
	xor ah, ah
	add si, ax
	xor bh, bh
	add si, bx

	
	mov di, board
	add cl, [Y]

	add bl, [X]
	xor bh, bh
	add di, bx
	xor ax, ax
	mov al, cl
	inc al
	mov ah, 10
	mul ah
	xor ah, ah
	add di, ax
	pop bx
	pop cx
	
	mov al, [Y]
	add al, [height]
	cmp al, 15
		jge .set0
	
	cmp byte [si], 0
		je .overS
	

	

	cmp byte [di], 1
		je .set0
	jmp .overS
.set0:
	mov byte [__down], 0
.overS:
	
	inc bl
	jmp .loop2
.endloop2:
	inc cl
	jmp .loop1
.endloop1:
	

	cmp byte [__down], 1
		je .mdownP
	
	cmp byte [Y], -1
		je gameOver
	
	mov byte [new_part_timer], 0
	
	call Materialize
	
	jmp .over666
.mdownP:
	
	inc byte [Y]
.over666:
xor cx, cx
mov si, board
.loop3:
	cmp cx, 15
		je .endloop3
	mov bx, 0
	mov al, 0
.loop4:
	cmp bx, 10
		je .endloop4
	
	cmp byte [si], 1
		je .ij
	jmp .oij
.ij:
	inc al
.oij:
	
	inc si
	
	inc bx
	jmp .loop4
.endloop4:
	cmp al, 10
		je .mdown
	jmp .omdown
times 10 db 0
.tmpbuffer: times 15*20 db 1
.mdown:
	; push cx
	
	; mov si, board
	; mov di, .tmpbuffer
	; push cx
	; mov cx, 150
	; cld
	; rep movsb
	; pop cx
	; mov si, .tmpbuffer 
	; mov di, board
	; add di, 10
	
	; mov ax, 10
	; ;dec cx
	; ;dec cx
	; mul cx
	
	; mov cx, ax

	; cld
	
	; rep movsb
	
	
	; pop cx
	
	pusha
	
	add word [score], 100
	
.loop6:

	dec cx
	
	mov ax, cx
	mov bx, 10
	mul bx
	mov bx, ax
	
	mov si, board
	
	add si, bx
	mov di, si
	add di, 10
	
	push cx
	mov cx, 10
	rep movsb
	pop cx
	
	cmp cx, 0
		je .endloop6
	jmp .loop6
.endloop6:
	popa
	
.omdown:	

	inc cx
	jmp .loop3
.endloop3:
	popa

	ret
__down: db 0
tempBuffer: times 14 db 0
	
Materialize:
	pusha
	xor cl, cl
.loop1:
	cmp cl, [height]
		je .endloop1
	mov bl, 0
.loop2:
	cmp bl, [widht]
		je .endloop2
	
	
	push cx
	push bx
	mov si, PART
	mov al, cl
	mov ah, [widht]
	mul ah
	xor ah, ah
	add si, ax
	xor bh, bh
	add si, bx
	
	mov di, board
	add cl, [Y]
	add bl, [X]
	xor bh, bh
	add di, bx
	xor ax, ax
	mov al, cl
	mov ah, 10
	mul ah
	xor ah, ah
	add di, ax
	
	mov al, [si]
	cmp al, 0
		je .over
	mov [di], al
.over:
	pop bx
	pop cx
	
	inc bl
	jmp .loop2
.endloop2:
	inc cl
	jmp .loop1
.endloop1:
	popa
	ret
	
RotateLeft:
RotateLeftNoPosFix:
	pusha
	
mov bx, 0
	
mov di, tempBuffer
mov si, PART

.loop1:
	cmp bx, 12
		je .endloop1
	
	mov al, [si]
	mov [di], al
		
	inc bx
	inc di
	inc si
	jmp .loop1
.endloop1:

mov byte [.j], 0

.loop2:
	mov al, [height]
	cmp [.j], al
		je .endloop2
	mov byte [.i], 0
.loop4:
	mov al, [widht]
	cmp [.i], al
		je .endloop4
		
	mov di, tempBuffer
	mov al, [.j]
	xor ah, ah
	add di, ax
	xor ax, ax
	mov al, [.i]
	mov ah, [height]
	mul ah
	add di, ax
	
	mov si, PART
	mov al, [.i]
	xor ah, ah
	add si, ax
	xor ax, ax
	mov al, [height]
	sub al, [.j]
	dec al
	mov ah, [widht]
	mul ah
	add si, ax
		
	mov al, [si]
	mov [di], al
		
	inc byte [.i]
	jmp .loop4
	
.endloop4:
	inc byte [.j]	
	jmp .loop2
.endloop2:


mov bx, 0

mov si, tempBuffer
mov di, PART

.loop3:
	cmp bx, 12
		je .endloop3
	
	mov al, [si]
	mov [di], al
		
	inc bx
	inc di
	inc si
	jmp .loop3
.endloop3:
	call switch
	popa
	ret
.j: db 0
.i: db 0

RotateRight:
	times 3 call RotateLeftNoPosFix
	ret

	
	
;;; OK, I GIVE UP, LET'S DO IT LAZY WAY... (SEE ABOVE)
; RotateRight:
	; pusha
	
; mov bx, 0
	
; mov di, tempBuffer
; mov si, PART

; .loop1:
	; cmp bx, 12
		; je .endloop1
	
	; mov al, [si]
	; mov [di], al
		
	; inc bx
	; inc di
	; inc si
	; jmp .loop1
; .endloop1:

; mov byte [.j], 0

; .loop2:
	; mov al, [height]
	; cmp [.j], al
		; je .endloop2
	; mov byte [.i], 0
; .loop4:
	; mov al, [widht]
	; cmp [.i], al
		; je .endloop4
		
	; mov di, tempBuffer
	; mov al, [height]
	; sub al, [.j]
	; dec al
	; xor ah, ah
	; add di, ax
	; xor ax, ax
	; mov al, [.i]
	; mov ah, [height]
	; mul ah
	; add di, ax
	
	; mov si, PART
	; mov al, [.i]
	; xor ah, ah
	; add di, ax
	; xor ax, ax
	; mov al, [.j]
	; mov ah, [widht]
	; mul ah
	; add di, ax
		
	; mov al, [si]
	; mov [di], al
		
	; inc byte [.i]
	; jmp .loop4
	
; .endloop4:
	; inc byte [.j]	
	; jmp .loop2
; .endloop2:


; mov bx, 0

; mov si, tempBuffer
; mov di, PART

; .loop3:
	; cmp bx, 12
		; je .endloop3
	
	; mov al, [si]
	; mov [di], al
		
	; inc bx
	; inc di
	; inc si
	; jmp .loop3
; .endloop3:
	; call switch
	; popa
	; ret
; .j: db 0
; .i: db 0
	
switch:
	push ax
	push bx
	
	mov al, [height]
	mov bl, [widht]
	mov [widht], al
	mov [height], bl
	
	pop bx
	pop ax
	ret

sc: db "Score: ",0
scb: times 10 db 0
	
Render:
	pusha
	;call ClearScreen
	
	xor dx, dx
	mov ah, 2
	int 10h
	
	mov si, sc
	call PrintString
	
	
	
	push word [score]
	push word scb
	push word 10
	push word 6
	
	call __itoa_16
	
	add sp, 8
	
	mov si, scb
	call PrintString
	
	;mov si, PART
	;call PrintString
	
	;popa
	;ret
	;jmp .endloop1
	;call ClearScreen
	
	mov cx, 3
.loop1:
	cmp cx, 18
		jge .endloop1
	mov dx, 20
.loop2:
	cmp dx, 30
		jge .endloop2
		
	mov ah, 2
	xor bx, bx
	
	push dx
	mov dh, cl
	
	;xchg bx, bx
	int 10h
	pop dx
	
	mov bl, 0
	
	mov di, board
	
	add di, dx
	sub di, 20
	
	push cx
	push dx
	push bx
	push ax
	mov ax, cx
	sub ax, 3
	mov bx, 10
	imul bx
	add di, ax
	pop ax
	pop bx
	pop dx
	pop cx
	
	
	cmp [di], bl 
		jne .case2
.aelse:
	mov al, 176
	jmp .fin
.case2:
	mov al, 219
.fin:
	mov ah, 0xe
    int 10h
	
	inc dx
	
	jmp .loop2
.endloop2:
	
	inc cx
	
	jmp .loop1
.endloop1:
	
	mov dx, 0
	mov cx, 0
.loop3:
	cmp cl, [height]
		jge .endloop3
	mov dx, 0
.loop4:
	cmp dl, [widht]
		jge .endloop4
		
	mov ah, 2
	xor bx, bx
	
	push dx
	mov dh, cl
	add dh, 3
	add dh, [Y]
	add dl, 20
	add dl, [X]
	
	;xchg bx, bx
	int 10h
	pop dx
	
	mov bl, 0
	
	mov di, PART
	
	add di, dx
	;sub di, [height]
	
	push cx
	push dx
	push bx
	push ax
	mov ax, cx
	;sub ax, 3
	mov bx, [widht]
	imul bx
	add di, ax
	pop ax
	pop bx
	pop dx
	pop cx
	
	
	cmp [di], bl 
		jne .case4
.aelse2:
	jmp .fin3
.case4:
	mov al, 219
.fin2:
	mov ah, 0xe
    int 10h
.fin3:
	inc dx
	
	jmp .loop4
.endloop4:
	
	inc cx
	
	jmp .loop3
.endloop3:
	
	popa
	ret

Backup:
	pusha
	
	mov si, PART
	mov di, PRTB
	
	mov cx, 12
	
	rep movsb
	
	mov al, [Y]
	mov [yB], al
	mov al, [X]
	mov [xB], al
	mov al, [height]
	mov [hB], al
	mov al, [widht]
	mov [wB], al
	
	popa
	ret
	

Unbackup:
	pusha
	
	mov si, PRTB
	mov di, PART
	
	mov cx, 12
	
	rep movsb
	
	mov al, [yB]
	mov [Y], al
	mov al, [xB]
	mov [X], al
	mov al, [hB]
	mov [height], al
	mov al, [wB]
	mov [widht], al
	
	popa
	ret
	
Check:
	pusha
	xor cx, cx
.loop1:
	cmp cl, [height]
		je .endloop1
	xor bx, bx
.loop2:
	cmp bl, [widht]
		je .endloop2
	
	mov si, PART
	xor bh, bh
	add si, bx
	
	mov al, cl
	mov dl, [widht]
	mul dl
	
	add si, ax
	
	mov di, board
	mov al, [X]
	xor ah, ah
	add di, ax
	xor bh, bh
	add di, bx
	
	mov al, [Y]
	add al, cl
	mov dl, 10
	mul dl
	
	add di, ax
	
	mov al, [di]
	and al, [si]
	
	cmp al, 1
		je .return
	jmp .oreturn
.return:
	call Unbackup
	popa
	mov al, 0
	ret
.oreturn:
	
	inc bl
	jmp .loop2
.endloop2:
	inc cl
	jmp .loop1
.endloop1:
	popa
	mov al, 1
	ret
	
NEW:

	mov si, hello
	call PrintString
	
	mov ah, 86h
	mov cx, 20h
	xor dx, dx
	xor al, al
	int 15h

	mov si, aprilfools
	call PrintString
	
		
	mov ah, 86h
	mov cx, 20h
	xor dx, dx
	xor al, al
	int 15h
	
	call ClearScreen
	
	mov si, board
	
gameLoop:
	
	mov ah, 86h
	mov cx, 03h
	xor dx, dx
	xor al, al
	int 15h
	
    mov ah, 01h
    int 16h
    jz .over
	
	xor ax, ax
	int 16h
	
	cmp ah, 0x50
		je .down
	cmp ah, 0x2c
		je .k2
	cmp ah, 0x48
		je .k
	cmp ah, 0x4d
		je .right
	cmp ah, 0x4b
		je .left
	jmp .over
.k:
	call Backup
	call RotateLeft
	call Check
	jmp .over
.k2:
	call Backup
	call RotateRight
	call Check
	jmp .over
.left:
	call Backup
	cmp byte [X], 0
		je .over
	dec byte [X]
	call Check
	jmp .over
.down:
	call down
	cmp byte [__down], 1
		je .incr
	jmp .over2
.incr:
	inc word [score]
.over2:
	jmp .over
.right:
	call Backup
	push ax
	mov al, [X]
	add al, [widht]
	cmp al, 10
		jge .over
	inc byte [X]
	pop ax
	call Check
.over:
	
	push 0000h
pop  gs
pushf
cli
mov ax,[gs:041Ah] ;Head
mov [gs:041Ch],ax ;Tail
popf 

	call Tick
	call Render
	
	jmp gameLoop

gameOver:
	call ClearScreen
	mov si, thx
	call PrintString
	
	mov ah, 86h
	mov cx, 15h
	xor dx, dx
	xor al, al
	int 15h
	
	call ClearScreen
	
	call PrintError
	
	jmp $
	
CriticalError: db "[NNXOS Critical Error]",0xa,0xd,"Error: Game Over",0xa,0xd,"Score: ",0

CriticalError2: db 0xa,0xd,0xa,0xd,"EAX: 0x54455452 EBX: 0x49532042",0xa,0xd,"ECX: 0x59204E4E EDX: 0x5821",0xa,0xd
				db "Eflags: 0x0",0xa,0xd,"CS: 0x0 DS: 0x0 SS: 0x0",0xa,0xd,"EIP: ???? (??)",0xa,0xd
				db "ESI: 0x0 EDI: 0x0 EBP: 0x0 ESP: 0x0 (no usermode)",0xa,0xd,"Control Registers:",0xa,0xd,
				db "CR0: 0x0 CR1: non-existent CR2: 0x0",0xa,0xd,"CR3: (paging disabled) CR4: 0x0",0
PrintError:
	mov si, CriticalError
	call PrintString
	
	push word [score]
	push word scb
	push word 10
	push word 6
	
	call __itoa_16
	
	add sp, 8
	
	mov si, scb
	call PrintString
	
	mov si, CriticalError2
	call PrintString


.hlt:
	hlt
	jmp .hlt

random7:
	push dx
	push bx
	
	call random
	xor dx, dx
	mov bx, 7
	div bx
	mov ax, dx
	pop bx
	pop dx
	ret
	
	
ClearPart:
	pusha
	mov di, PART
	
	mov byte [X], 0
	mov byte [Y], 0
	
	mov byte [widht], 0
	mov byte [height], 0
	
	mov bx, 0
.loop:
	cmp bx, 12
		je .end
	mov byte [di], 0
	inc bx
	inc di
	jmp .loop
.end:	
	popa
	ret
	
GenNewPart:
	pusha
	call ClearPart
	
	call random7
	
	add ax, ax
	
	mov di, parts
	add di, ax
	
	mov si, [di]
	
	mov al, [si]
	mov ah, [si+1]
	
	xchg bx, bx
	
	mov [widht], al
	mov [height], ah
	mov byte [X], 3
	mov byte [Y], -1
	
	mul ah
	
	add si, 2
	
	mov bx, 0
	mov di, PART
.loop:
	cmp bx, ax
		je .end
	
	mov dl, [si]
	mov [di], dl
	
	inc di
	inc si
	inc bx
	
	jmp .loop
.end:
	
	popa
	ret

parts: dw sq, I, L, L2, T, S, Z, sq
	
	
	sq: db 2,2,1,1,1,1
	I: db 1,4,1,1,1,1
	L: db 2,3,1,0,1,0,1,1
	L2: db 2,3,0,1,0,1,1,1
	T: db 3,2,1,1,1,0,1,0
	S: db 3,2,0,1,1,1,1,0
	Z: db 3,2,1,1,0,0,1,1
	
random:         ; generate a rand no using the system time
   push cx
   push dx
   MOV AH, 00h  ; interrupts to get system time        
   INT 1AH      ; CX:DX now hold number of clock ticks since midnight      

   mov  ax, dx
   xor  dx, dx
   mov  cx, 10    
   div  cx       ; here dx contains the remainder of the division - from 0 to 9    
   pop dx
   pop cx
   RET   
	
DATA:
rnum: dw 0
aprilfools: db "just kiddin, april fools, play some tetris",0	
thx: db "                            Thx for playing.",0xa,0xd,"                                             ~NNX",0

score: dw 0
	
times 0x1000 - ($ - $$) db 0
	add_board:
	

	
