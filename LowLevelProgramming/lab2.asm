assume CS:code, DS:data

data segment
    arr DB -1,-2,10,20,5,7
    arr_size EQU ($-arr)
    ans db 100 
    msg db ?,'$'
data ends

code segment
start:
    mov AX, data
    mov DS, AX
    mov SI, 0
    mov AX, 0
    mov CX, arr_size
    mov BL, ans
run:
    mov al, arr[si]
    inc si
    cmp al, arr[si]
    JL getabs
    sub al,arr[si]
    endofabs:
    cmp ax, bx
    JNG changeans
    cmp SI, arr_size
    je exit
    loop run
getabs:
    mov cl,al
    mov al,arr[si]
    sub al,cl
    jmp endofabs
changeans:
    dec si
    mov dx,si
    inc si
    mov bx, ax
    loop run
exit:
    mov di, offset msg
    mov al,dl
    add al, 30h
    mov [di],al
    MOV ah, 09h
	mov dx, offset msg
    mov ah,09h
    int 21h
    mov ax,4c00h
    int 21h
code ends

end start