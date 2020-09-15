	.text
	.global	insert_sort
	.type	insert_sort, @function
insert_sort:
    # rax - i; rdx - j; rbx - n
    mov $8, %rax
    mov %rsi, %rbx
    sub %rdi, %rbx # dla listy 5 elem n == 32, dlatego dodajemy 8
    add $8, %rbx
    
    loop1:
        cmp %rbx, %rax              # jesli i >= n to zakoncz
        jge exit
        mov (%rdi, %rax,1), %rcx    # rcx = array[i]
        mov %rax, %rdx              # j = i - 1
        sub $8, %rdx
    
        loop2:
            cmp $0, %rdx    # jesli j < 0, to wyjdz z second_loop
            jl exit2
            mov (%rdi, %rdx, 1), %r9
            cmp %rcx, %r9   # jesli array[j] <= array[i], to wyjdz z second_loop
            jle exit2
            mov %r9, 8(%rdi, %rdx, 1)   # array[j+1] = array[j]
            sub $8, %rdx                # j--
            jmp loop2
        
        exit2:
        mov %rcx, 8(%rdi, %rdx, 1)      # array[j+1] = array[i]
        add $8, %rax                    # i++
        jmp loop1
    
    exit:
    ret

	.size	insert_sort, . - insert_sort
