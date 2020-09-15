	.text
	.global	add_bignum
	.type	add_bignum, @function
add_bignum:
    # rdi = a, rsi = b, rdx = r
    
    # wyciagamy n z a i b
    movl (%rdi), %ebx
    movl (%rsi), %eax
    
    # jesli a.n < b.n to zamieniamy a i b miejscami
    cmp %ebx, %eax
    jng skip_swapping
    mov %rdi,%rcx
    mov %rsi,%rdi
    mov %rcx,%rsi
    movl (%rsi), %eax
    
    skip_swapping:
    xor %r9, %r9    # r9 = i
    xor %r13, %r13  # r13 = carry_bit
    xor %r14, %r14  # r14 = second_carry_bit
    
    loop1:
        mov 4(%rdi, %r9), %r10b # r10b = a.digits[i] 
        mov 4(%rsi, %r9), %r11b # r11b = b.digits[i]
        
        # r.digits[i] = a.digits[i] + b.digits[i] + carry_bit
        add %r13b, %r10b
        setb %r13b
        add %r10b, %r11b
        setb %r14b
        or %r14b, %r13b
        mov %r11b, 4(%rdx, %r9)
        
        add $1, %r9d    # i++
        
        # jesli i >= b.n to zakoncz petle
        cmp %eax, %r9d 
        jl loop1
    
    movl (%rdi), %eax
    loop2:
        mov 4(%rdi, %r9), %r10b # r10b = a.digits[i]  
        
        # r.digits[i] = a.digits[i] + carry_bit
        add %r13b, %r10b
        setb %r13b
        mov %r10b, 4(%rdx, %r9)
        
        add $1, %r9d            # i++
        
        # jesli i >= a.n to zakoncz petle
        cmp %eax, %r9d
        jl loop2
    
    # jesli wyszlismy z petli, a carry_bit dalej zapalony, to
    # to dodajemy ten bit na koniec i zwiekszamy r.n o 1
    cmp $1,%r13b
    jne exit
    mov %r13b, 4(%rdx, %r9)
    add $1, %r9d
    
    exit:
    movl %r9d,(%rdx)
    ret

	.size	add_bignum, . - add_bignum
