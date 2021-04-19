.586
.model use16 tiny

psp_offset equ 100h

terminate_fn equ 4Ch
display_text_fn equ 09h
read_from_file_via_handle equ 3Fh

dos_services_int equ 21h

stdin equ 0

cr_ascii_code equ 0Dh

password_hash equ 1021h

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
	
	jmp across_buffer
	
buf_capacity equ 256
buf db buf_capacity dup(0)

across_buffer:
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
	mov ah, display_text_fn
	mov dx, offset access_granted_msg
	int dos_services_int

exit:	
	mov ah, terminate_fn
	xor al, al
	int dos_services_int
_start endp

;------------------------------------------------------------------------------
; Input:     1 <- input string offset
; Output:    ax <- input string length
; Destroyed: bx, cx, si, di
;------------------------------------------------------------------------------
crc_hash_stdin_string proc
	push bp
	mov bp, sp
	
	mov si, [bp + word * (1 + 1)]
	xor bx, bx
	xor ah, ah
crc_loop:
	lodsb
	cmp al, cr_ascii_code
	je exit_crc_loop

	mov dx, bx
	and dx, 0F800h
	shr dx, 16 - 5
	shl bx, 5
	xor bx, dx
	
	xor bx, ax

	jmp crc_loop
	
exit_crc_loop:
	mov ax, bx

	pop bp
	ret
crc_hash_stdin_string endp

.data
greeting_msg db 'Hi! Enter your password to gain access: ', '$'
access_granted_msg db 'Password matched, access granted.', 0Dh, 0Ah, '$'
access_denied_msg db 'Password mismatch, access denied.', 0Dh, 0Ah, '$'

end_of_program:
end _start