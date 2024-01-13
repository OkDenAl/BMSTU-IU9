include lab4.inc
assume cs: code, ds: data

data segment
    newline db 0Ah, "$" 
    string db 100, 99 dup ('$')
    max_len dw 16
    num_a db 100, 99 dup (0) 
    num_b db 100, 99 dup (0) 
    res db 100, 99 dup (0)
    notation db 10 ; change this to 16 to check hex system operations
    cmpres db 0

    error_wrong db 100, " error: non-numerical symbol $"
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

tohex proc
    mov cl, 60h
    cmp cl,ch
    jge tohexret
        sub ch, 'a'
        add ch, ':'
    tohexret:
    ret
tohex endp

fromhex proc
    mov cl, '9'
    cmp cl,ch
    jge fromhexret
        sub ch, ':'
        add ch, 'a'
    fromhexret:
    ret
fromhex endp

tonum proc
    mov bp, sp
    mov di, [bp + 2]
    xor ax, ax
    mov al, string[1]
    mov bx, max_len
    sub bx, ax  
    add ax, 2
    mov si, 2 
    xor dx, dx
    mov [di], dx 
    loop_tonum:
        mov ch, string[si]
        call tohex
        ifnotnumber ch, number_case
        ifnotminus ch, minus_case
            error_print  error_wrong, ch
        minus_case:
            push ax
            add di, max_len
            mov ax, [di]
            not ax
            mov [di], ax
            sub di, max_len
            pop ax
            jmp endcase
        number_case:
            sub ch, '0'
            mov [di + bx], ch
        endcase:
            inc si
            inc bx
            cmp si, ax
            jge break_tonum
                jmp loop_tonum
        break_tonum:
    ret
tonum endp

numtostring proc
    mov bp, sp
    mov si, [bp + 2] 
    mov ax, max_len
    xor di, di 

    add si, max_len
    mov bl, [si]
    cmp bx, 0
    je plus
        printchar '-'
        jmp endsign
    plus:
        printchar '+'
    endsign:
        sub si, max_len

    mov bx, 2
    loop_numtostring:
        mov ch, [si]
        add ch, '0'
        call fromhex
        mov string[bx], ch

        inc si
        inc di
        inc bx
        cmp di, ax
        jge break_numtostring
            jmp loop_numtostring
        break_numtostring:
    ret
numtostring endp

printnum macro num
    mov dx, offset num
    push dx
    call numtostring
    println string
endm

scannum macro num
    mov dx, offset string
    call scanstr
    mov dx, offset num
    push dx
    call tonum
endm

compare_nums proc
    push di
    push ax
    push bx    
    xor ax, ax
    xor bx, bx
    xor si, si

    mov di, max_len
    mov al, num_a[di]
    mov bl, num_b[di]

    cmp ax, bx
    je loop_comp
    jl sign_less
        mov cmpres, 2        
        jmp endcompare_nums
    sign_less:
        mov cmpres, 1
        jmp endcompare_nums
    
    loop_comp:
        mov al, num_a[si]
        mov bl, num_b[si]
        cmp ax, bx
        je cmp_equal
        jl equal_less
            mov cmpres, 1
            jmp endcompare_nums
        equal_less:
            mov cmpres, 2
            jmp endcompare_nums
        cmp_equal:

        inc si
        cmp si, max_len
        jge endcompare_nums
        jmp loop_comp
    endcompare_nums:
    pop bx
    pop ax
    pop di
    ret
compare_nums endp

invert_sign macro num
    push di
    push ax
    mov di, max_len
    mov al, num[di]
    not al
    mov num[di], al
    pop ax
    pop di
endm

calculate_sum proc
    mov di, max_len
    mov al, num_a[di]
    mov bl, num_b[di]
    cmp al, bl
    je skipdiff
        invert_sign num_b
        call calculate_diff
        ret
    skipdiff:
    mov di, max_len
    mov al, num_a[di]
    cmp al, 0
    je invert_sign_in_diff_
        invert_sign res
    invert_sign_in_diff_:

    mov si, max_len
    sub si, 1
    loop_sum:
        xor cx, cx
        mov ah, num_a[si]
        mov bh, num_b[si]
        mov ch, res[si]

        add ch, ah
        add ch, bh
        mov cl, notation
        dec cl
        cmp cl,ch
        jge sum_overflow
            sub ch, notation
            mov cl, 1
            mov res[si - 1], cl
        sum_overflow:

        mov res[si], ch

        dec si
        cmp si, 0
        jl break_sum
        jmp loop_sum
    break_sum:
    ret
calculate_sum endp

calculate_diff proc
    mov di, max_len
    mov al, num_a[di]
    mov bl, num_b[di]
    cmp al, bl
    je skipsum
        invert_sign num_b
        call calculate_sum
        ret
    skipsum:

    call compare_nums
    cmp cmpres, 2
    jne not_swap
        push si
        push ax
        push bx
        mov si, max_len
        dec si
        loop_swap:
            mov al, num_a[si]
            mov bl, num_b[si]
            mov num_a[si], bl
            mov num_b[si], al

            dec si
            cmp si, 0
            je break_swap
            jmp loop_swap
        break_swap:    
            pop bx
            pop ax
            pop si
        invert_sign res
    not_swap:

    mov di, max_len
    mov al, num_a[di]
    cmp al, 0
    je invert_sign_in_diff
        invert_sign res
    invert_sign_in_diff:

    mov si, max_len
    sub si, 1
    mov dh,0

    loop_diff:
        mov cx,0
        mov ah, num_a[si]
        mov bh, num_b[si]

        add ch, ah
        sub ch, bh
        sub ch, dh
        
        mov cl,0
        mov dh,0
        cmp ch, cl
        jge diff_overflow
            add ch, notation
            mov dh, 1
        diff_overflow:

        mov res[si], ch
        dec si
        cmp si, 0
        jl break_diff
        jmp loop_diff
    break_diff:
    ret
calculate_diff endp

calculate_prod proc
    mov di, max_len
    sub di, 1
    xor bx, bx
    
    loop_sumprod:
        mov si, max_len
        sub si, 1
        loop_prod:
           
            xor ax, ax
            xor cx, cx
            xor dx, dx
            mov al, num_a[si]
            mov dl, num_b[di]
            
            mul dx
            mov cl, notation
            div cl
            sub si, bx
            add res[si - 1], al
            add res[si], ah
            add si, bx

            dec si
            cmp si, 0
            jl break_prod
            jmp loop_prod
        break_prod:

        inc bx
        dec di
        cmp di, 0
        jl break_sumprod
        jmp loop_sumprod
    break_sumprod:

    mov di, max_len
    sub di, 1
    loop_fix:
        xor ax, ax
        mov cl, notation
        mov al, res[di]
        div cl
        add res[di - 1], al
        mov res[di], ah

        dec di
        cmp di, 0
        jl break_fix
        jmp loop_fix
    break_fix:
    mov di, max_len
    push ax
    push bx
    mov al, num_a[di]
    mov bl, num_b[di]
    xor al, bl
    mov res[di], al
    pop bx
    pop ax
    ret
calculate_prod endp

start:
    mov ax, data
    mov ds, ax
    mov es,ax

    mov dx,0
    mov res[0], dh

    scannum num_a
    scannum num_b

    ; uncomment the desired function and comment the undesired

    call calculate_sum
    ;call calculate_diff
    ;call calculate_prod
    
    printnum num_a
    printnum num_b
    printnum res

    mov ah, 4ch
    int 21h
code ends
end start