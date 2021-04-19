.586
.model use16 tiny

psp_offset equ 100h

dos_services_int equ 21h
terminate_fn equ 4Ch
display_char_fn equ 02h
display_text_fn equ 09h

null equ 0

bits_in_byte equ 8

.code
org psp_offset

start proc
	; prolog
	push 255
	push 33
	push 1488
	push 3802
	push offset string1
	push offset string2
	push 255
	push 33
	push 100
	push 3802
	push offset string1
	push offset format_string
	; prolog
	call printf
	; epilog
	add sp, word * 12
	; epilog

	mov ah, terminate_fn
	xor al, al
	int dos_services_int
start endp

;----------------------------------------------------------------------------------------------------------------------
; Input:     1 <- format string offset, ... <- format string arguments
; Output:    none
; Destroyed: ax, bx, cx, si, di
;----------------------------------------------------------------------------------------------------------------------
printf proc
	push bp
	mov bp, sp

	mov di, [bp + word * (1 + 1)]
	xor bx, bx
	mov si, word * (1 + 2)

analyze_format_string:
	cmp buffer_size, buffer_capacity

	jbe continue_bufferizing

	; prolog
	push bx
	; prolog
	call flush_buffer
	; epilog
	pop bx
	; epilog

continue_bufferizing:
	cmp byte ptr [di + bx], null
	je finish_analyzing_format_string

	cmp byte ptr [di + bx], '%'
	je analyze_format_specificator

	mov dl, [di + bx]
	push bx
	mov bx, offset buffer
	add bx, buffer_size
	mov [bx], dl
	inc buffer_size
	pop bx

	jmp finish_analyzing_char

analyze_format_specificator:
	inc bx

	xor dh, dh
	mov dl, [di + bx]
	imul dx, byte + word
	add dx, offset jump_table
	jmp dx

print_procent_char:
	push bx
	mov bx, offset buffer
	add bx, buffer_size
	mov byte ptr [bx], '%'
	inc buffer_size
	pop bx

	jmp finish_analyzing_char

print_char:
	mov dl, [bp + si]

	push bx
	mov bx, offset buffer
	add bx, buffer_size
	mov [bx], dl
	inc buffer_size
	pop bx

	jmp finish_analyzing_format_specificator

print_str:
	; prolog
	push di si bx ax

	push word ptr [bp + si]
	; prolog
	call print_str_bufferized
	; epilog
	add sp, word

	pop ax bx si di
	; epilog

	jmp finish_analyzing_format_specificator

print_decimal:
	push di si bx ax

	; prolog
	push word ptr [bp + si]
	; prolog
	call convert_to_decimal
	; epilog
	add sp, word
	; epilog

	; prolog
	push offset conversion_result
	; prolog
	call print_str_bufferized
	; epilog
	add sp, word
	; epilog

	pop ax bx si di
	jmp finish_analyzing_format_specificator

print_binary:
	push di si bx ax

	; prolog
	push 1
	push 1
	push word ptr [bp + si]
	; prolog
	call convert_to_power_of_two
	; epilog
	add sp, word * 3
	; epilog

	; prolog
	push offset conversion_result
	; prolog
	call print_str_bufferized
	; epilog
	add sp, word
	; epilog

	pop ax bx si di
	jmp finish_analyzing_format_specificator

print_octal:
	push di si bx ax

	; prolog
	push 7
	push 3
	push word ptr [bp + si]
	; prolog
	call convert_to_power_of_two
	; epilog
	add sp, word * 3
	; epilog

	; prolog
	push offset conversion_result
	; prolog
	call print_str_bufferized
	; epilog
	add sp, word
	; epilog

	pop ax bx si di
	jmp finish_analyzing_format_specificator

print_hex:
	push di si bx ax

	; prolog
	push 0Fh
	push 4
	push word ptr [bp + si]
	; prolog
	call convert_to_power_of_two
	; epilog
	add sp, word * 3
	; epilog

	; prolog
	push offset conversion_result
	; prolog
	call print_str_bufferized
	; epilog
	add sp, word
	; epilog

	pop ax bx si di
	jmp finish_analyzing_format_specificator

finish_analyzing_format_specificator:
	add si, word

finish_analyzing_char:
	inc bx
	jmp analyze_format_string

finish_analyzing_format_string:
	cmp buffer_size, 0
	je return

	; prolog
	call flush_buffer
	; epilog

return:
	pop bp
	ret
printf endp

;----------------------------------------------------------------------------------------------------------------------
; Input:     buffer <- string to display, buffer_size <- current size of buffer
; Output:    none
; Destroyed: ax, bx, dx
;----------------------------------------------------------------------------------------------------------------------
flush_buffer proc
	mov bx, offset buffer
	add bx, buffer_size
	mov byte ptr [bx], '$'

	mov ah, display_text_fn
	mov dx, offset buffer
	int 21h

	mov buffer_size, 0

	ret
flush_buffer endp

;----------------------------------------------------------------------------------------------------------------------
; Input:     1 <- string to print
; Output:    none
; Destroyed: ax, bx, cx, si, di
;----------------------------------------------------------------------------------------------------------------------
print_str_bufferized proc
	push bp
	mov bp, sp

	push es
	mov bx, ds
	mov es, bx

	; prolog
	push word ptr [bp + word * (1 + 1)]
	; prolog
	call strlen
	; epilog
	pop si
	; epilog

	mov cx, ax
	add cx, buffer_size
	cmp cx, buffer_capacity

	jbe bufferize_str

print_while_cannot_fit_in_buffer:
	mov cx, buffer_capacity
	sub cx, buffer_size

	mov di, offset buffer
	add di, buffer_size
	mov buffer_size, buffer_capacity

	cld
	rep movsb

	; prolog
	push ax
	; prolog
	call flush_buffer
	; epilog
	pop ax
	; epilog

	mov cx, [bp + word * (1 + 1)]
	add cx, ax
	sub cx, si
	cmp cx, buffer_capacity

	jbe bufferize_str
	jmp print_while_cannot_fit_in_buffer

bufferize_str:
	mov cx, [bp + word * (1 + 1)]
	add cx, ax
	sub cx, si

	mov di, offset buffer
	add di, buffer_size
	add buffer_size, cx

	cld
	rep movsb

	pop es bp
	ret
print_str_bufferized endp

;----------------------------------------------------------------------------------------------------------------------
; Input:     1 <- input string offset
; Output:    ax <- input string length
; Destroyed: bx, cx, di
;----------------------------------------------------------------------------------------------------------------------
strlen proc
	push bp
	mov bp, sp

	push es
	mov bx, ds
    mov es, bx

	mov di, [bp + word * (1 + 1)]
	xor al, al
	mov cx, 0FFFFh

	cld
	repne scasb

	pop es

	not cx
	dec cx
	mov ax, cx

	pop bp
	ret
strlen endp

;----------------------------------------------------------------------------------------------------------------------
; Input:     1 <- number
; Output:    conversion_result <- number converted to decimal (string)
; Destroyed: ax, bx, cx, dx, si, di
;----------------------------------------------------------------------------------------------------------------------
convert_to_decimal proc
	push bp
	mov bp, sp

    xor dx, dx
	mov ax, [bp + word * (1 + 1)]
	xor bh, bh
	mov bl, 10
	mov di, word * bits_in_byte - 1
convert_decimal_digit:
	div bx
	add dx, '0'
	mov conversion_result[di], dl

	cmp ax, 0
	je end_decimal_digit_conversion

	xor dx, dx
	dec di
	jmp convert_decimal_digit
end_decimal_digit_conversion:
    mov cx, di
    jcxz no_leading_decimal_zeros

	; prolog
	push di
	; prolog
	call omit_leading_zeros
	; epilog
	add sp, word
	; epilog

no_leading_decimal_zeros:
	pop bp
	ret
convert_to_decimal endp

;----------------------------------------------------------------------------------------------------------------------
; Input:     1 <- number, 2 <- base (power of two), 3 <- bit mask (2^(power_of_two) - 1)
; Output:    conversion_result <- number converted to binary (string)
; Destroyed: ax, bx, cx, si, di
;----------------------------------------------------------------------------------------------------------------------
convert_to_power_of_two proc
	push bp
	mov bp, sp

	mov ax, [bp + word * (1 + 1)]
	mov cl, [bp + word * (1 + 2)]
    mov di, word * bits_in_byte - 1
convert_digit:
	mov bx, [bp + word * (1 + 3)]
	and bx, ax
	shr ax, cl
	mov dl, hex_digits[bx]
	mov conversion_result[di], dl

	cmp ax, 0
	je end_digit_conversion

	dec di
	jmp convert_digit
end_digit_conversion:
    mov cx, di
    jcxz no_leading_zeros

	; prolog
    push di
    ; prolog
    call omit_leading_zeros
    ; epilog
    add sp, word
	; epilog

no_leading_zeros:
	pop bp
	ret
convert_to_power_of_two endp

;----------------------------------------------------------------------------------------------------------------------
; Input:     1 <- offset to first significant digit
; Output:    conversion_result <- number without leading zeros
; Destroyed: bx, cx, si, di
;----------------------------------------------------------------------------------------------------------------------
omit_leading_zeros proc
	push bp
	mov bp, sp

	push es

	mov bx, ds
	mov es, bx

	mov cx, word * bits_in_byte + 1
	mov bx, [bp + word * (1 + 1)]
	sub cx, bx
	lea si, [conversion_result + bx]
	mov di, offset conversion_result

	cld
	rep movsb

	pop es

	pop bp
	ret
omit_leading_zeros endp

.data
hex_digits db '0123456789ABCDEF'
conversion_result db word * bits_in_byte dup(0), 0

buffer_capacity equ 15
buffer db buffer_capacity dup(0), '$'
buffer_size dw 0

jump_table:
rept '%'
jmp finish_analyzing_format_string
endm

jmp print_procent_char

rept 'b' - '%' - 1
jmp finish_analyzing_format_string
endm

jmp print_binary

jmp print_char

jmp print_decimal

rept 'o' - 'd' - 1
jmp finish_analyzing_format_string
endm

jmp print_octal

rept 's' - 'o' - 1
jmp finish_analyzing_format_string
endm

jmp print_str

rept 'x' - 's' - 1
jmp finish_analyzing_format_string
endm

jmp print_hex

rept 255 - 'x'
jmp finish_analyzing_format_string
endm

format_string db 'I %s %x %d %% %c %b %s, but I %s %x %d %% %c %b', 0
string1 db 'love', 0
string2 db 'MEOW', 0

end start
