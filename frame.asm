psp_offset equ 100h
video_memory_text_mode_segment equ 0B800h

dos_services_int equ 21h
terminate_fn equ 4Ch

null equ 0

screen_char_size equ 2
screen_width equ 80

frame_char_attr equ 15h
frame_width equ 60
frame_height equ 8
frame_x0 equ (screen_width - frame_width) / 2
frame_y0 equ 9
frame_x1 equ frame_x0 + (frame_width - 1)
frame_y1 equ frame_y0 + (frame_height - 1)

text_char_attr equ frame_char_attr
text_y0 equ frame_y0 + (frame_height + 1) / 2

.model tiny, stdcall

.code
org psp_offset

start proc
	mov bx, video_memory_text_mode_segment
	mov es, bx

	call draw_frame
	call draw_text

	mov ah, terminate_fn
	xor al, al
	int dos_services_int
start endp

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

	mov ah, frame_char_attr
	mov al, [bp + word * (1 + 2)]
	stosw

	mov al, [bp + word * (1 + 3)]
	mov cx, frame_width - screen_char_size
	rep stosw

	mov al, [bp + word * (1 + 4)]
	stosw

	pop bp
	ret word * 4
draw_frame_line endp

;--------------------------------------------------------------------------------------------------
; Input:     none
; Output:    none
; Destroyed: ax, bx, bp, di
;--------------------------------------------------------------------------------------------------
draw_text proc
	push bp
	mov bp, sp

	call calc_text_len

    ; get return value
	mov bx, ax

	; calculate text_x0
	mov ax, frame_x0 + frame_x1
	sub ax, bx
	xor bh, bh
	mov bl, 2
	div bl
	mov ah, 0

	; calculate text offset
	mov bx, screen_width * text_y0
	add ax, bx
	mov bx, screen_char_size
	mul bx
	mov di, ax

	xor bx, bx
	cld
draw_char:
	cmp text[bx], null
	je end_draw_char

	mov ah, text_char_attr
	mov al, text[bx]
	stosw

	inc bx
	jmp draw_char
end_draw_char:

	pop bp
	ret
draw_text endp

;----------------------------------------------------------------------------------------------------------------------
; Input:     text <- input string
; Output:    ax <- input string length
; Destroyed: bx, cx, di
;----------------------------------------------------------------------------------------------------------------------
calc_text_len proc
	push es
	mov bx, ds
    mov es, bx

	cld

	mov di, offset text
	xor al, al
	mov cx, 0FFFFh
	repne scasb

	not cx
	dec cx
	mov ax, cx

	pop es
	ret
calc_text_len endp

.data
text db 'Hello to $MS-DOS$ from Poltorashka! Meow.', 0

end start
