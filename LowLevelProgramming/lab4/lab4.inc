printchar macro char
    push ax
    push dx
    mov ah, 2
    mov dl, char
    int 21h
    pop dx
    pop ax
endm

println macro str
    push ax
    mov ah, 09h
    lea dx, str
    add dx, 2 
    int 21h
    pop ax
    push ax
    mov ah, 09h
    lea dx, newline
    int 21h
    pop ax
endm

ifnotnumber macro symbol, endmark
    push ax
    mov al, '/'
    mov ah, '0'
    add ah, notation
    cmp symbol, ah
    jge _&endmark
    cmp al, symbol
    jge _&endmark
        pop ax
        jmp endmark
    _&endmark&:   
    pop ax
endm

ifnotminus macro symbol, endmark
    push ax
    push bx
    mov ah, '-'
    mov bh, symbol
    cmp bh, ah
    pop bx
    pop ax
    je endmark
endm

error_print macro message
    println message
    mov ah, 4ch
    int 21h
endm