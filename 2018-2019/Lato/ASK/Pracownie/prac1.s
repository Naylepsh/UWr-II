# Rozwiazanie na podstawie zadania 6 z listy 0.
# Najpierw zamieniamy kazda pare bitow miejscami (1<->2, 3<->4, ..., 63<->64)
# Nastepnie zamieniamy miejscamy pary ((1,2)<->(3,4), ..., (61,62)<->(63,64))
# I tak dalej, az ostatnim krokiem bedzie ((1,2,...,32)<->(33,34,...,64))
	.text
	.global	bitrev
	.type	bitrev, @function
bitrev:
    # zal ze postac rax to abcde...
    mov %rdi,%rax
    
    # Ustawiamy maske 1010...1010
    mov $0xAAAAAAAAAAAAAAAA,%r9
    mov %rax,%rsi
    # Przesuwamy ciag bitow w lewo o jedna pozycje
    shl %rsi
    # and-ujemy maske i ciag otrzymujac b0d0f0...
    and %r9,%rsi
    # Teraz ustawiamy maske 0101...0101
    mov $0x5555555555555555,%r10
    shr %rax
    # teraz otrzymamy 0a0c0e0...
    and %r10,%rax
    # or-ujemy i dostajemy badcfe...
    or  %rsi,%rax
    
    # operacje ponizej dzialaja w analogiczny sposob
    # jedyna roznica w uzytych maskach i przesunieciach
    
    mov $0xCCCCCCCCCCCCCCCC,%r9
    mov %rax,%rsi
    shl $0x2,%rsi
    and %r9,%rsi
    mov $0x3333333333333333,%r10
    shr $0x2,%rax
    and %r10,%rax
    or  %rsi,%rax
    
    mov $0xF0F0F0F0F0F0F0F0,%r9
    mov %rax,%rsi
    shl $0x4,%rsi
    and %r9,%rsi
    mov $0x0F0F0F0F0F0F0F0F,%r10
    shr $0x4,%rax
    and %r10,%rax
    or  %rsi,%rax
    
    mov $0xFF00FF00FF00FF00,%r9
    mov %rax,%rsi
    shl $0x8,%rsi
    and %r9,%rsi
    mov $0x00FF00FF00FF00FF,%r10
    shr $0x8,%rax
    and %r10,%rax
    or  %rsi,%rax
    
    mov $0xFFFF0000FFFF0000,%r9
    mov %rax,%rsi
    shl $0x10,%rsi
    and %r9,%rsi
    mov $0x0000FFFF0000FFFF,%r10
    shr $0x10,%rax
    and %r10,%rax
    or  %rsi,%rax
    
    # Niech A to pierwsze 32 bity, a B to ostatnie 32 bity
    # Wtedy aby uzyskac BA przesuwamy AB w nastepujacy sposob
    mov %rax,%rsi
    # w lewo o 32 otrzymujac B0
    shl $0x20,%rsi
    # w prawo o 32 otrzymujac 0A
    shr $0x20,%rax
    # or-ujemy i uzyskujemy BA
    or %rsi,%rax
    
    ret

	.size	bitrev, .-bitrev