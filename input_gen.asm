.586
.model use16 tiny

psp_offset equ 100h

terminate_fn equ 4Ch
write_to_file_via_handle equ 40h

dos_services_int equ 21h

stdout equ 1

code_offset equ 0B7h - 06Ch

.code
org psp_offset

_start proc
	mov ah, write_to_file_via_handle
	mov bx, stdout
	mov cx, code_offset + 1
	mov dx, offset nop_buf
	int dos_services_int
	

	mov ah, terminate_fn
	xor al, al
	int dos_services_int
_start endp

.data
nop_buf db code_offset dup(0), 0Dh

end _start