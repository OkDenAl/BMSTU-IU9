assume CS:code, DS:data
data segment
   a db 3
   b db 4
   c db 5
   d db 7
   str db ?,?,'$'
data ends

code segment 

main:
	mov AX, data
	mov DS, AX


	mov bl, a
    mov dl,b
    mov cl,3
	shl bl,cl  ;AX=8*a=24
    mov ax,bx
    div dl
    
    mov cl, c
    mov bl, d
    add cl,bl ;CX=c+d=12
    mov bx,cx ;AX=12
    shr bx,1 ;AX=6
    add ax,bx ;6+6=12

    mov dl,10
    div dl
    mov di, offset str
	ADD al, 30H                  
    mov [di],al
    ADD ah, 30H             
    mov [di + 1],ah
    mov dx, offset str
    mov ah,09h
    int 21h

mov ax,4c00h
int 21h

code ends
end main