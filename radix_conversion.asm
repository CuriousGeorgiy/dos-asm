psp_offset equ 100h

dos_services_int equ 21h
display_text_fn equ 09h
terminate_fn equ 4Ch

bits_in_byte equ 8

.model tiny, stdcall

.code
org psp_offset

start proc
    mov bx, 0B800h

    ; prologue
    push bx

	push bx
	; prologue

	call convert_to_decimal

	; epilogue
	pop bx
	; epilogue

    call print_result

    ; prologue
    push bx

    push bx
    ; prologue

    call convert_to_binary

    ; epilogue
    pop bx
    ; epilogue

    call print_result

    ; prologue
    push bx

    push bx
    ; prologue

    call convert_to_hex

    call print_result

	mov ah, terminate_fn
	xor al, al
	int dos_services_int
start endp

;----------------------------------------------------------------------------------------------------------------------
; Input:     1 <- number
; Output:    result <- number converted to decimal (string)
; Destroyed: ax, bx, si, di
;------------------------------------------------------------------------------
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
	mov result[di], dl

	cmp ax, 0
	je end_decimal_digit_conversion

	xor dx, dx
	dec di
	jmp convert_decimal_digit
end_decimal_digit_conversion:
    mov cx, di
    jcxz no_leading_decimal_zeros

	; prologue
	push di
	; prologue

	call omit_leading_zeros

no_leading_decimal_zeros:
	pop bp
	ret word
convert_to_decimal endp

;----------------------------------------------------------------------------------------------------------------------
; Input:     1 <- number
; Output:    result <- number converted to binary (string)
; Destroyed: ax, bx, si, di
;----------------------------------------------------------------------------------------------------------------------
convert_to_binary proc
	push bp
	mov bp, sp

	mov ax, [bp + word * (1 + 1)]
    mov di, word * bits_in_byte - 1
convert_bit:
	mov bx, 1
	and bx, ax
	add bl, '0'
	shr ax, 1
	mov result[di], bl

	cmp ax, 0
	je end_bit_conversion

	dec di
	jmp convert_bit
end_bit_conversion:
    mov cx, di
    jcxz no_leading_bit_zeros

	; prologue
    push di
    ; prologue

    call omit_leading_zeros

no_leading_bit_zeros:
	pop bp
	ret word
convert_to_binary endp

;----------------------------------------------------------------------------------------------------------------------
; Input:     1 <- number (word size)
; Output:    result <- number converted to hex (string)
; Destroyed: ax, bx, dx, si, di
;----------------------------------------------------------------------------------------------------------------------
convert_to_hex proc
	push bp
	mov bp, sp

	mov ax, [bp + word * (1 + 1)]
    mov di, word * bits_in_byte - 1
convert_hex_digit:
	mov bx, 0Fh; 15d
	and bx, ax
	shr ax, 4; log2(16)
	mov dl, hex_digits[bx]
	mov result[di], dl

	cmp ax, 0
	je end_hex_digit_conversion

	dec di
	jmp convert_hex_digit
end_hex_digit_conversion:
    mov cx, di
    jcxz no_leading_hex_zeros

	; prologue
	push di
	; prologue

	call omit_leading_zeros

no_leading_hex_zeros:
	pop bp
	ret word
convert_to_hex endp

;----------------------------------------------------------------------------------------------------------------------
; Input:     1 <- offset to first significant digit
; Output:    result <- number without leading zeros
; Destroyed: bx, cx, si, di
;----------------------------------------------------------------------------------------------------------------------
omit_leading_zeros proc
	push bp
	mov bp, sp

	push es
	mov bx, ds
	mov es, bx
	cld
	mov cx, word * bits_in_byte + 1
	mov bx, [bp + word * (1 + 1)]
	sub cx, bx
	lea si, [result + bx]
	mov di, offset result
	rep movsb
	pop es

	pop bp
	ret word
omit_leading_zeros endp

;----------------------------------------------------------------------------------------------------------------------
; Input:     result
; Output:    none
; Destroyed: ax, dx
;----------------------------------------------------------------------------------------------------------------------
print_result proc
    push bp
    mov bp, sp

    mov ah, display_text_fn
    mov dx, offset result
    int dos_services_int

    mov dx, offset new_line
    int dos_services_int

    pop bp
    ret
print_result endp

.data
result db word * bits_in_byte dup(0), '$'

new_line db 0Dh, 0Ah, '$'

hex_digits db '0123456789ABCDEF'

end start
