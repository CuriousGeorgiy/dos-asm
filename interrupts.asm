psp_offset equ 100h
video_memory_text_mode_segment equ 0B800h

dos_services_int equ 21h
display_text_fn equ 09h
get_interrupt_vector_fn equ 35h
set_interrupt_vector_fn equ 25h
terminate_and_stay_resident_fn equ 31h

read_keyboard_port equ 60h

hotkey_scan_code equ 32; 'd'

null equ 0

screen_char_size equ 2
screen_width equ 80

frame_char_attr equ 15h
frame_width equ 11
frame_height equ 13
frame_x0 equ 63
frame_y0 equ 0
frame_x1 equ frame_x0 + (frame_width - 1)
frame_y1 equ frame_y0 + (frame_height - 1)

text_char_attr equ frame_char_attr

register_value_text_size equ 8

.model tiny, stdcall

.code
org psp_offset

start proc
	mov ah, get_interrupt_vector_fn
	mov al, 08h
	int dos_services_int

	mov ax, es
	mov word ptr old_08_int + word, ax
	mov word ptr old_08_int, bx

	mov ah, set_interrupt_vector_fn
	mov al, 08h
	mov dx, offset new_08_int
	int dos_services_int

    mov ah, get_interrupt_vector_fn
    mov al, 09h
    int dos_services_int

    mov ax, es
    mov word ptr old_09_int + word, ax
    mov word ptr old_09_int, bx

    mov ah, set_interrupt_vector_fn
    mov al, 09h
    mov dx, offset new_09_int
    int dos_services_int

    mov ah, terminate_and_stay_resident_fn
    xor al, al
    mov dx, offset end_of_program
    shr dx, 4
    inc dx
    int dos_services_int
start endp

new_08_int proc
	pushf
    call cs:old_08_int

	mov cs:prev_dx, dx
	mov cs:prev_sp, sp
	mov dx, ds
	mov cs:prev_ds, dx
	mov dx, ss
	mov ds, dx

	push ss
	push es
	push cs:prev_ds
	push cs:prev_sp
	push bp
	push di
	push si
	push cs:prev_dx
	push cx
	push bx
	push ax

    cmp cs:debug_frame_enabled, 1
    jne do_not_redraw_debug_frame

	cmp cs:debug_frame_active, 1
	je do_not_save_video_memory_snapshot

	call save_video_memory_snapshot

	xor cs:debug_frame_active, 1
do_not_save_video_memory_snapshot:
	call draw_debug_frame

	jmp end_int_08

do_not_redraw_debug_frame:
	cmp cs:debug_frame_active, 1
	jne end_int_08

	call restore_video_memory_snapshot

	xor cs:debug_frame_active, 1

end_int_08:
    pop ax
	pop bx
	pop cx
	pop dx
	pop si
	pop di
	pop bp
	add sp, word
	pop ds
	pop es
	pop ss

    iret
new_08_int endp

new_09_int proc
	push ax

    in al, read_keyboard_port

    pushf
    call cs:old_09_int

    cmp al, hotkey_scan_code
    jne end_int_09

	xor cs:debug_frame_enabled, 1

end_int_09:
    pop ax
    iret
new_09_int endp

;----------------------------------------------------------------------------------------------------------------------
; Input:     none
; Output:    none
; Destroyed: ax, bx, cx, dx, si, di
;----------------------------------------------------------------------------------------------------------------------
save_video_memory_snapshot proc
push bp
	mov bp, sp

	push ds
	push es

	mov ax, video_memory_text_mode_segment
	mov ds, ax
	mov ax, cs
	mov es, ax

	mov bx, frame_y0
	mov cx, frame_height
	mov di, offset cs:frame_snapshot
save_video_memory:
	push cx

	; calculate offset
	mov ax, screen_width
	xor dh, dh
	mov dl, bl
	mul dx
	mov dl, frame_x0
	add ax, dx
	mov dl, screen_char_size
	mul dx
	mov si, ax

	mov cx, frame_width

	cld
	rep movsw

	pop cx

	inc bx
	loop save_video_memory

	pop es
	pop ds

	pop bp
	ret
save_video_memory_snapshot endp

;----------------------------------------------------------------------------------------------------------------------
; Input:     none
; Output:    none
; Destroyed: none
;----------------------------------------------------------------------------------------------------------------------
restore_video_memory_snapshot proc
push bp
	mov bp, sp

	push ds
	push es

	mov ax, video_memory_text_mode_segment
	mov es, ax
	mov ax, cs
	mov ds, ax

	mov bx, frame_y0
	mov cx, frame_height
	mov si, offset cs:frame_snapshot
restore_video_memory:
	push cx

	; calculate offset
	mov ax, screen_width
	xor dh, dh
	mov dl, bl
	mul dx
	mov dl, frame_x0
	add ax, dx
	mov dl, screen_char_size
	mul dx
	mov di, ax

	mov cx, frame_width

	cld
	rep movsw

	pop cx

	inc bx
	loop restore_video_memory

	pop es
	pop ds

	pop bp
	ret
restore_video_memory_snapshot endp

;----------------------------------------------------------------------------------------------------------------------
; NOTA BENE: arguments are not passed explicitly, see caller structure
; Input:     1 <- ax value, 2 <- bx value, 3 <- cx value, 4 <- dx value, 5 <- si value, 6 <- di value, 7 <- bp value,
;			 8 <- sp value, 9 <- ds value, 10 <- es value, 11 <- ss value
; Output:    none
; Destroyed: ax, bx, cx, dx, di
; NOTA BENE: doesn't clean stack frame, see caller structure
;----------------------------------------------------------------------------------------------------------------------
draw_debug_frame proc
	push bp
	mov bp, sp

	call draw_frame

	; prologue
	push word ptr [bp + word * (1 + 1)]

	mov ax, 'xa'
	push ax
	; prologue
	call get_register_value

	; prologue
    mov ax, frame_y0 + 1
    push ax
	; prologue

	call draw_register_value_text

	; prologue
	push word ptr [bp + word * (1 + 2)]

	mov ax, 'xb'
	push ax
	; prologue
	call get_register_value

	; prologue
	mov ax, frame_y0 + 2
	push ax
	; prologue

	call draw_register_value_text

	; prologue
	push word ptr [bp + word * (1 + 3)]

	mov ax, 'xc'
	push ax
	; prologue
	call get_register_value

	; prologue
	mov ax, frame_y0 + 3
	push ax
	; prologue

	call draw_register_value_text

	; prologue
	push word ptr [bp + word * (1 + 4)]

	mov ax, 'xd'
	push ax
	; prologue
	call get_register_value

	; prologue
	mov ax, frame_y0 + 4
	push ax
	; prologue

    call draw_register_value_text

	; prologue
	push word ptr [bp + word * (1 + 5)]

	mov ax, 'is'
	push ax
	; prologue
	call get_register_value

	; prologue
	mov ax, frame_y0 + 5
	push ax
	; prologue

	call draw_register_value_text

	; prologue
	push word ptr [bp + word * (1 + 6)]

	mov ax, 'id'
	push ax
	; prologue
	call get_register_value

	; prologue
	mov ax, frame_y0 + 6
	push ax
	; prologue

	call draw_register_value_text

	; prologue
	push word ptr [bp + word * (1 + 7)]

	mov ax, 'pb'
	push ax
	; prologue
	call get_register_value

	; prologue
	mov ax, frame_y0 + 7
	push ax
	; prologue

	call draw_register_value_text

	; prologue
	push word ptr [bp + word * (1 + 8)]

	mov ax, 'ps'
	push ax
	; prologue
	call get_register_value

	; prologue
	mov ax, frame_y0 + 8
	push ax
	; prologue

	call draw_register_value_text

	; prologue
	push word ptr [bp + word * (1 + 9)]

	mov ax, 'sd'
	push ax
	; prologue
	call get_register_value

	; prologue
	mov ax, frame_y0 + 9
	push ax
	; prologue

	call draw_register_value_text

	; prologue
	push word ptr [bp + word * (1 + 10)]

	mov ax, 'se'
	push ax
	; prologue
	call get_register_value

	; prologue
	mov ax, frame_y0 + 10
	push ax
	; prologue

	call draw_register_value_text

	; prologue
	push word ptr [bp + word * (1 + 11)]

	mov ax, 'ss'
	push ax
	; prologue
	call get_register_value

	; prologue
	mov ax, frame_y0 + 11
	push ax
	; prologue

	call draw_register_value_text

	pop bp
	ret
draw_debug_frame endp

;----------------------------------------------------------------------------------------------------------------------
; Input:     none
; Output:    none
; Destroyed: ax, bx, cx, dx, di
;----------------------------------------------------------------------------------------------------------------------
draw_frame proc
	push bp
	mov bp, sp

	; prologue
	xor bh, bh
	mov bl, 0BBh; see CP437
    push bx

	mov bl, 0CDh; see CP437
	push bx

	mov  bl, 0C9h; see CP437
    push bx

	mov bl, frame_y0
	push bx
	; prologue

	call draw_frame_line

	; prologue
	xor bh, bh
	mov bl, 0BAh; see CP437
    push bx

    xor bl, bl; see CP437
	push bx

    mov bl, 0BAh; see CP437
	push bx
	; prologue

	call draw_frame_middle

	; prologue
	xor bh, bh
	mov bl, 0BCh; see CP437
    push bx

    mov  bl, 0CDh; see CP437
	push bx

	mov  bl, 0C8h; see CP437
	push bx

	mov bl, frame_y1
	push bx
	; prologue

	call draw_frame_line

	pop bp
	ret
draw_frame endp

;----------------------------------------------------------------------------------------------------------------------
; Input:     1 <- first frame line character, 2 <- intermediate frame line character,
;            3 <- last frame line character
; Output:    none
; Destroyed: ax, bx, cx, dx, di
;----------------------------------------------------------------------------------------------------------------------
draw_frame_middle proc
	push bp
	mov bp, sp

	mov bx, frame_y0 + 1
	mov cx, frame_height - 2
draw_frame_middle_lines:
	; prologue
	push cx

	xor ah, ah
	mov al, [bp + word * (1 + 3)]
	push ax

	mov al, [bp + word * (1 + 2)]
	push ax

	mov al, [bp + word * (1 + 1)]
	push ax

	push bx
	; prologue

	call draw_frame_line

	; epilogue
	pop cx
	; epilogue

	inc bx
	loop draw_frame_middle_lines

	pop bp
	ret word * 3
draw_frame_middle endp

;----------------------------------------------------------------------------------------------------------------------
; Input:     1 <- frame line y, 2 <- first frame line character, 3 <- intermediate frame line character,
;            4 <- last frame line character
; Output:    none
; Destroyed: ax, cx, dx, di
;----------------------------------------------------------------------------------------------------------------------
draw_frame_line proc
	push bp
	mov bp, sp

	; calculate frame line offset
	mov ax, screen_width
	xor dh, dh
	mov dl, [bp + word * (1 + 1)]
	mul dx
	mov dl, frame_x0
	add ax, dx
	mov dl, screen_char_size
	mul dx
	mov di, ax

	cld
	push es
	mov dx, video_memory_text_mode_segment
    mov es, dx

	mov ah, frame_char_attr
	mov al, [bp + word * (1 + 2)]
	stosw

	mov al, [bp + word * (1 + 3)]
	mov cx, frame_width - screen_char_size
	rep stosw

	mov al, [bp + word * (1 + 4)]
	stosw

	pop es

	pop bp
	ret word * 4
draw_frame_line endp

;--------------------------------------------------------------------------------------------------
; Input:     1 <- text y, register_value <- 'ax=0000'
; Output:    none
; Destroyed: ax, bx, cx, bp, di
;--------------------------------------------------------------------------------------------------
draw_register_value_text proc
	push bp
	mov bp, sp

	; calculate text offset
	mov ax, [bp + word * (1 + 1)]
	mov bx, screen_width
	mul bx
	add ax, frame_x0 + 2
	mov bx, screen_char_size
	mul bx
	mov di, ax

	xor bx, bx
	cld
	push es
	mov cx, video_memory_text_mode_segment
    mov es, cx
draw_char:
	cmp cs:register_value_text[bx], null
	je end_draw_char

	mov ah, text_char_attr
	mov al, cs:register_value_text[bx]
	stosw

	inc bx
	jmp draw_char
end_draw_char:
	pop es

	pop bp
	ret word
draw_register_value_text endp

;----------------------------------------------------------------------------------------------------------------------
; Input:     1 <- register name, 2 <- register value
; Output:    register_value <- 'ax=' hex register value
; Destroyed: ax, bx, dx, si, di
;----------------------------------------------------------------------------------------------------------------------
get_register_value proc
	push bp
	mov bp, sp

	mov ax, [bp + word * (1 + 1)]
	mov word ptr cs:register_value_text, ax

	mov ax, [bp + word * (1 + 2)]
    mov di, register_value_text_size - 2; omit last zero byte
convert_hex_digit:
	mov bx, 0Fh; 15d
	and bx, ax
	shr ax, 4; log2(16)
	mov dl, cs:hex_digits[bx]
	mov cs:register_value_text[di], dl

	cmp ax, 0
	je end_hex_digit_conversion

	dec di
	jmp convert_hex_digit
end_hex_digit_conversion:
	pop bp
	ret word * 2
get_register_value endp

.data
prev_dx dw 0
prev_ds dw 0
prev_sp dw 0

old_08_int dd 0
old_09_int dd 0

debug_frame_enabled db 0
debug_frame_active db 0

hex_digits db '0123456789ABCDEF'
register_value_text db 2 dup(0), '=', 4 dup('0'), 0

frame_snapshot db frame_height * frame_width * screen_char_size dup(0)

end_of_program:
end start
