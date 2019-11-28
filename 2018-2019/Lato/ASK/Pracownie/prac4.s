	.text
	.global	sqrf
	.type	sqrf, @function
sqrf:
    shl $1, %edi
    cmp $0, %edi
    jne go_on
    mov $0, %eax
    ret
    
    go_on:
    shr $1, %edi
    mov %edi, %r8d   # r8 - cecha
    mov $0x7f800000, %r9d
    and %r9d, %r8d
    # shr o 23 aby dostac faktyczna wartosc cechy
    # shl o 1, bo kwadrat (czyli 2 razy cecha), czyli lacznie shr o 22
    shr $22, %r8d
    sub $127, %r8d      # ale -127 bo offset policzy sie dwa razy
    
    mov %edi, %r10d     # r10 - mantysa
    mov $0x7fffff, %r9d
    and %r9d, %r10d
    add $0x800000, %r10d # dodajemy jedynke
    mov %r10d, %eax
    mul %rax
    shr $23, %rax
    mov $0x1000000, %r10    # sprawdzamy czy mantysa nie jest za dluga
    cmp %r10, %rax
    jl merge_results
    # mantysa zajmuje ponad 23 bity wiec musimy to naprawic
    shr $1, %eax    # dzielimy mantyse przez 2
    add $1, %r8d    # zwiekszamy ceche o 1
    
    merge_results:
    shl $23, %r8d
    and %r9d, %eax
    or %r8d, %eax
    ret


	.size	sqrf, . - sqrf
