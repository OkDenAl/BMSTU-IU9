assume cs: code, ds: data

data segment
    newline db 0Ah, "$"
    destptr db 100, 99 dup ('$')
    srcptr db 100, 99 dup ('$')
data ends

code segment

scanstr proc 
        mov ax, 0
		mov ah, 0Ah
		int 21h

        mov si,dx
        xor bh, bh  
        mov bl, [si+1]
        mov ch, '$'
        add bx, 2
        mov [si+bx], ch
		
		mov dx, offset newline
		mov ah, 09h
		int 21h
        ret
scanstr endp

strcpy proc
    push bp
    mov bp, sp
    mov ax, [bp+6] ;src
    mov cx, [bp+4] ;dest

    add ax,2
    mov si, ax
    add cx,2
    mov di,cx
    for:
        mov dl, [si]
        cmp dl, '$'
            je exit
        movsb
        jmp for
    exit:
        mov di, offset destptr
        mov [bp+4],di
        mov sp,bp
        pop bp
        ret
strcpy endp

printstr proc
    push bp
	mov bp, sp
	
	mov dx, [bp+4]
    add dx, 2
	mov ah, 09h
	int 21h
	pop bp
	pop bx
	xor ax, ax
	push ax
	push bx
	ret
printstr endp


start:
    mov ax, data
    mov ds, ax
    mov es,ax

    ; read strings
    mov dx, offset srcptr
    call scanstr
    mov dx, offset destptr
    call scanstr
    
    mov dx,offset srcptr
    push dx
    mov dx,offset destptr
    push dx
    call strcpy
    
    ; output
    call printstr
    mov ah, 4ch
    int 21h
code ends
end start