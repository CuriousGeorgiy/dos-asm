.586
.model use16 tiny

psp_offset equ 100h

terminate_fn equ 4Ch
display_text_fn equ 09h
read_from_file_via_handle equ 3Fh

dos_services_int equ 21h

stdin equ 0

cr_ascii_code equ 0Dh

crc_significant_digits equ 5
crc_bitmask_for_significant_digits equ 0F800h
bits_in_byte equ 8

password_hash equ 1021h

program_hash equ 4A66h

.code
org psp_offset

_start proc
	mov ah, display_text_fn
	mov dx, offset greeting_msg
	int dos_services_int
	
	mov ah, read_from_file_via_handle
	mov bx, stdin
	mov cx, offset end_of_program ; oops
	sub cx, offset buf            ; oops
	mov dx, offset buf
	int dos_services_int
	
	jmp end_of_buf
	
buf_capacity equ 256
buf db buf_capacity dup(0)

end_of_buf:
	; prolog
	push offset buf
	; prolog
	call crc_hash_stdin_string
	; epilog
	add sp, word
	; epilog
	
	cmp ax, password_hash
	je access_granted
	
	mov ah, display_text_fn
	mov dx, offset access_denied_msg
	int dos_services_int
	
	jmp exit
	
access_granted:
	; prolog
	call crc_hash_program
	; epilog
	
compare_program_hash:
	cmp ax, program_hash
end_of_compare_program_hash:
	jne report_hack

	mov ah, display_text_fn
	mov dx, offset access_granted_msg
	int dos_services_int
	
	jmp exit
	
report_hack:
	mov ah, display_text_fn
	mov dx, offset report_hack_msg
	int dos_services_int

exit:	
	mov ah, terminate_fn
	xor al, al
	int dos_services_int
_start endp

;------------------------------------------------------------------------------
; Input:     1 <- input string from stdin offset (terminated by CR)
; Output:    ax <- input string cyclic redundant code
; Destroyed: bx, cx, si, di
;------------------------------------------------------------------------------
crc_hash_stdin_string proc
	push bp
	mov bp, sp
	
	mov si, [bp + word * (1 + 1)]
	xor bx, bx
	xor ah, ah
crc_stdin_string_loop:
	lodsb
	cmp al, cr_ascii_code
	je exit_crc_stdin_string_loop

	mov dx, bx
	and dx, crc_bitmask_for_significant_digits
	shr dx, word * bits_in_byte - crc_significant_digits
	shl bx, crc_significant_digits
	xor bx, dx
	
	xor bx, ax

	jmp crc_stdin_string_loop
	
exit_crc_stdin_string_loop:
	mov ax, bx

	pop bp
	ret
crc_hash_stdin_string endp

;------------------------------------------------------------------------------
; Input:     1 <- initial cyclic redundant code, 2 <- string offset, 
;            3 <- string length
; Output:    ax <- program's cyclic redundant code
; Destroyed: bx, cx, si, di
;------------------------------------------------------------------------------
crc_hash_string proc
	push bp
	mov bp, sp
	
	mov cx, [bp + word * (1 + 3)]
	mov si, [bp + word * (1 + 2)]
	mov bx, [bp + word * (1 + 1)]
	xor ah, ah
crc_string_loop:
	lodsb

	mov dx, bx
	and dx, crc_bitmask_for_significant_digits
	shr dx, word * bits_in_byte - crc_significant_digits
	shl bx, crc_significant_digits
	xor bx, dx
	
	xor bx, ax
	
	loop crc_string_loop

	mov ax, bx
	
	pop bp
	ret
crc_hash_string endp

;------------------------------------------------------------------------------
; Input:     none
; Output:    ax <- program's cyclic redundant code
; Destroyed: bx, cx, si
;------------------------------------------------------------------------------
crc_hash_program proc
	push bp
	mov bp, sp
	
	; prolog
	mov ax, offset buf
	sub ax, offset _start
	push ax
	mov ax, offset _start
	push ax
	xor ax, ax
	push ax
	; prolog
	call crc_hash_string
	; epilog
	add sp, word * 3
	; epilog
	
	mov bx, ax
	
	; prolog
	mov ax, offset compare_program_hash
	sub ax, offset end_of_buf
	push ax
	mov ax, offset end_of_buf
	push ax
	push bx
	; prolog
	call crc_hash_string
	; epilog
	add sp, word * 3
	; epilog
	
	mov bx, ax
	
	; prolog
	mov ax, offset end_of_program
	sub ax, offset end_of_compare_program_hash
	push ax
	mov ax, offset end_of_compare_program_hash
	push ax
	push bx
	; prolog
	call crc_hash_string
	; epilog
	add sp, word * 3
	; epilog

	pop bp
	ret
crc_hash_program endp

.data
greeting_msg db 'Hi! Enter your password to gain access: ', '$'
access_granted_msg db 'Password matched: access granted.', 0Dh, 0Ah, '$'
access_denied_msg db 'Password mismatch: access denied.', 0Dh, 0Ah, '$'
report_hack_msg db 'Hack attempt detected: sent your identity to FBI for further investigation.', 0Dh, 0Ah, '$'

end_of_program:
end _start