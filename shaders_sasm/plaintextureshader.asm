;--------------------------------------------------------------------
;
; Texture shader
;
; 30.11.2004, Sven Woop -- nolight crippling: jofis
;

	; store uv-coordinates and return
;	mov R0, R4.xyx + return or xyzw (>=0 or <1)

	; compute hit point
	mad R0,R4.z,R1,R0

	; load texture transformation
	ld4x TRI,4

	; compute texture coordinates
par    (dph2 R1.x,I1,R4
	dph2 R1.y,I2,R4
        mov R2.xy,I0)

;mov R0, R4.xyx + exit 0

	; texture access
	texld4x_word_fp R2,R1

	; bilinear interpolation
	frac R2.xy,R1
	add R2.zw,C.y,-R2.xyxy
	mul R2,R2.zxzx,R2.wwyy
	mul R1,R2.x,I0
	mad R1,R2.y,I1,R1
	mad R1,R2.z,I2,R1
	mad R1,R2.w,I3,R1

        mov R0, R1 + return or xyzw (>=0 or <1)


	


	

