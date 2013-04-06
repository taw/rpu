;--------------------------------------------------------------------
;
; Global Illumination Shader, supports 128 light sources
;
; 17.01.2005, Sven Woop
;

	; store uv-coordinates and return
	;mov R0, R4.xyx + exit 0

	; compute hit point
	mad R0,R4.z,R1,R0

	; load texture transformation
	ld4x TRI,4

	; compute texture coordinates
par    (dph2 R1.x,I1,R4
	dph2 R1.y,I2,R4
        mov R2.xy,I0)

	; texture access
	;texld4x R2,R1
	texld I0,R2,R1
	mov R1,I0

	; bilinear interpolation
	;frac R2.xy,R1
	;add R2.zw,ONE,-R2.xyxy
	;mul R2,R2.zxzx,R2.wwyy
	;mul R1,R2.x,I0
	;mad R1,R2.y,I1,R1
	;mad R1,R2.z,I2,R1
	;mad R1,R2.w,I3,R1

	; initialize color accumulator and counter
	mov R3,C3.y

	; load normal 
	ld I0,TRI,7
	mov R2,I0

	; R0 = hit position
	; R1 = material color
	; R2 = normal
	; R3.xyz = color accumulator
	; R3.w = light source counter

loop:	; check if there are more light sources
	add R4.w,R3.w,-C0.w + jmp finished, and w >=0

	;mov R0,R4.xyy + exit 1

	; load light specification from texture
	mov R4.x,R3.w
	mov R4.y,C1.z

	mov R5,C1
	texld4x R5,R4

	; increment
	add R3.w,R3.w,C.y

	; shoot light ray
	mov R4,I0
	add R5,-I0,R0

	dp3_rsq R5.w,R5,R5
	mul R6,S.w,R5
	
	mov R5.w,S.w
	mul R5.w,R5,R5
	
	dp3 R1.w,R5,R2 + jmp neg, w <0
	;mov R5.w,S.w
	;mul R5.w,R5,R5

pos:	mul R1.w,R1.w,R5.w + jmp cont, and w (>=0 or <1)
neg:    mul R1.w,-R1.w,R5.w
cont:

	;mov R0,I2 + exit 1

	mul R9,R1.w,I2          ; store light intensity
	dp3_rcp R6.x,R5,C3.xyyy
	dp3_rcp R6.y,R5,C3.yxyy
	dp3_rcp R6.z,R5,C3.yyxy
par	(mov E0,R4 
	mov E1,S
	mov R8,C3.yyzz
	mov E2,C3 + trace push 4 rty 1)

	mad R3,R1,R9,R3 + jmp loop, and x (>=0 or <1)    ; multiply light with color and loop

finished:
	mad R0,C1.w,R1,R3 + return or xyzw (>=0 or <1)         ; add ambient part
	


	

