;--------------------------------------------------------------------
;
; Textures the complete screen
;
; 29.11.2004, Sven Woop
;

	; compute texture coordinates
	dph3 R0.x,C1,I1
	dph3 R0.y,C2,I1

	texld4x_word_fp C0,R0
	mov O,I0 + return or xyzw (>=0 or <1)

	add R0,I0,-C3 + jmp blue, or x <0 
	mov O,C.yxxx + return or xyzw (>=0 or <1)
blue:	mov O,C.xxyx + return or xyzw (>=0 or <1)




	

