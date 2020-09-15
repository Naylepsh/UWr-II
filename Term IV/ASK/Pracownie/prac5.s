read = 0
write = 1
exit  = 60

        .global _start
        .type _start, @function
        .section .text

_start: 
        mov %rsp, %r8
        sub $1, %rsp
        movb $'\n', (%rsp)  # znak nowej linii na poczatek stosu
        sub $1, %rsp
        mov $0, %rbx        # rbx bedzie naszym indeksem
        
        read_loop:
            mov $0, %rax        # 0 w raxie oznacza opcje read
            mov $0, %rdi        # 0 w rdi oznacza uzycie stdin
            mov %rsp, %rsi      # input zostanie zapisany na stosie
            mov $1, %rdx        # pobieramy po jednym znaku
            syscall
            sub $1, %rsp
            add $1, %rbx
            cmp $0, %rax        # jesli pobralismy null to konczymy petle
            je write_stack
            jmp read_loop
        
        write_stack:
        add $2, %rsp            # pomijamy pierwsze dwa znaki, bo to smieci
        mov $1, %rax            # 1 w rax, czyli write
        mov $1, %rdi            # 1 w rdi, czyli stdout
        mov %rsp, %rsi          # wypisujemy ze stosu
        mov %rbx, %rdx
        syscall
        
        # sprzatamy stos
        mov %r8, %rsp
        
        movq    $exit, %rax
        movq    $0, %rdi
        syscall
        
        .size   _start, . - _start