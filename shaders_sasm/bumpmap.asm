;--------------------------------------------------------------------
;
; Phong shader with bumpmap and reflections
;
; 13.12.2004, Sven Woop
;

mov R0,R4.xy + return or xyzw (>=0 or <1)

	; compute light vector 
	mad R6,R1,R4.z,R0
	add R5,-R6,C0
	dp3_rsq R5.w,R5,R5
	mul R5,S.w,R5

	; compute reflected direction
	dp3_rsq R1.w,R1,R1        ; normalize ray direction
	mul R1,S.w,R1

	dp3 R3.w,-C1,R1          ; dot product between normal and direction
	add R3.w,R3.w,R3.w       ; 2*
	mad R3,R3.w,C1,R1        ; d - 2 * (d*n) * n
	mad R6,C1.w,R3,R6
	dp3_rcp R7.x,R3,C.yxxx
	dp3_rcp R7.y,R3,C.xyxx
	dp3_rcp R7.z,R3,C.xxyx
	
	;mov O0.xyz,R6 + mov H.w,C3
	;mov D0.xyz,S
	;mov R10,C3.yyww
	;mov RT0.w,C3.z + trace 0 push 6
	;mov R0,R6 + exit 0

	;mov O0.xyz,R6 + mov H.w,C3
	;mov D0.xyz,S
	;mov R0,R6
	;mov R1,R7
	;mov R4,C3.yyww
	;mov RT0.w,C3.z + trace 0 push 0
	;mov R0,R0 + exit 0	

	; load texture transformation
	ld4x TRI,4

	; compute texture coordinates
par    (dph2 R2.x,I1,R4
	dph2 R2.y,I2,R4
	mov R3.xy,I0)

	; texture access
	texld4x R3,R2

	; bilinear interpolation
	frac R3.xy,R2
	add R3.zw,C.y,-R3.xyxy
	mul R3,R3.zxzx,R3.wwyy
	mul R2,R3.x,I0
	mad R2,R3.y,I1,R2
	mad R2,R3.z,I2,R2
	mad R2,R3.w,I3,R2

	; compute reflected direction
	dp3_rsq R1.w,R1,R1        ; normalize ray direction
	mul R1,S.w,R1
	dp3 R3.w,-R2,R1          ; dot product between normal and direction
	add R3.w,R3.w,R3.w       ; 2*
	mad R3,R3.w,R2,R1        ; d - 2 * (d*n) * n
	mad R6,C1.w,R3,R6

	; compute ambient light intensity
	dp3_sat R2.x,R2,R5	
	mul R0,R2.x,C1

	; compute specular light intensity
	dp3_sat R3.w,R3,R5
	add R3.w,R3.w,-C.y
	mul R3.w,R3.w,R3.w
	mul R3.w,R3.w,C2.w
	add R3.w,R3.w,C.y
	dp4_rcp R3.w,R3.w,C.xxxy
	
	mad_sat R0,S.w,C2,R0 

	; shoot reflection ray
	mov R1,R6
	dp3_rcp R2.x,R3,C.yxxx
	dp3_rcp R2.y,R3,C.xyxx
	dp3_rcp R2.z,R3,C.xxyx
par    (mov E0,R6 
	mov E1,S
	mov R5,C3.yyzz
	mov E2,C3 + trace push 1 rty 0)

	mad_sat R0,R1,C0.w,R0 
	+ return or xyzw (>=0 or <1)

	


	

