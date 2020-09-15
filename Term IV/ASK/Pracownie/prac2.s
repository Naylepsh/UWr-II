# Count Leading Zeros za pomoca Binary Search.
	.text
	.global	clz
	.type	clz, @function
clz:
    xor %eax,%eax
    # Sprawdzamy czy x == 0, jesli tak to eax++
    cmp $0,%rdi
    jne i0
    add $1,%eax
i0:
    # Sprawdzamy czy pierwsze 32 bity sa samymi zerami.
    # Jesli tak, to eax+=32, a sprawdzone bity wyrzucamy,
    # tzn. przesuwamy w lewo o 32.
    mov %rdi,%rsi
    mov $0xFFFFFFFF00000000,%rdx
    and %rdx,%rsi
    cmp $0,%rsi
    jne i1
    add $32,%eax
    shl $32,%rdi
i1:
    # Tym razem sprawdzamy kolejne 16 bitow.
    # Jesli sa samymi zerami to eax+=16 i bity wyrzucamy.
    mov %rdi,%rsi
    mov $0xFFFF000000000000,%rdx
    and %rdx,%rsi
    cmp $0,%rsi
    jne i2
    add $16,%eax
    shl $16,%rdi
i2:
    # Analogicznie kolejne 8 bitow.
    mov %rdi,%rsi
    mov $0xFF00000000000000,%rdx
    and %rdx,%rsi
    cmp $0,%rsi
    jne i3
    add $8,%eax
    shl $8,%rdi
i3:
    # 4 bity
    mov %rdi,%rsi
    mov $0xF000000000000000,%rdx
    and %rdx,%rsi
    cmp $0,%rsi
    jne i4
    add $4,%eax
    shl $4,%rdi
i4:
    # 2 bity
    mov %rdi,%rsi
    mov $0xC000000000000000,%rdx
    and %rdx,%rsi
    cmp $0,%rsi
    jne i5
    add $2,%eax
    shl $2,%rdi
i5:
    # 1 bit
    mov %rdi,%rsi
    mov $0x8000000000000000,%rdx
    and %rdx,%rsi
    cmp $0,%rsi
    jne i6
    add $1,%eax
i6:    
    ret

	.size	clz, .-clz
