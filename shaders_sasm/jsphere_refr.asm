;--------------------------------------------------------------------
;
; Sphere shader
;
; 30.11.2004, Sven Woop; modified by Jörg Schmittler 18.2.05
;                        WARNING: not optimized!

	;mov R0,R4.z + exit 1

	; compute normal in world space
	mad R7,R4.z,R1,R0
	
	ld4x OBJ,0
	dph3 R6.x,I0,R7
	dph3 R6.y,I1,R7
	dph3 R6.z,I2,R7

	ld4x OBJ,4
	dp3 R6.x,I0,R6
	dp3 R6.y,I1,R6
	dp3 R6.z,I2,R6
	
	dp3_rsq R6.w,R6,R6        ; normalize normal
	mul R6,S.w,R6

	dp3_rsq R1.w,R1,R1        ; normalize ray direction
	mul R1,S.w,R1

        dp3 R0.w,R1,R6 + jmp cont, w >=0   ; cosinus
        mov R0.w,-R0.w
cont:
	; compute reflected direction	
	dp3 R3.w,-R6,R1          ; dot product between normal and direction
	add R3.w,R3.w,R3.w       ; 2*
	mad R3,R3.w,R6,R1        ; d - 2 * (d*n) * n

	; compute light vector 
	add R8,-R7,C0
	dp3_rsq R8.w,R8,R8
	mul R8,S.w,R8	

	; compute specular light intensity
	dp3_sat R3.w,R3,R8
	add R3.w,R3.w,-C.y
	mul R3.w,R3.w,R3.w
	mul R3.w,R3.w,C2.w
	add R3.w,R3.w,C.y
	dp4_rcp R3.w,R3.w,C.xxxy
	mul_sat R0.xyz,S.w,C2

	; compute refracted direction	
	mul R1,C1.x,R1           ; refr * D
	dp3 R2.x,R6,R1           ; N * (refr * D)
	mul R2.y,R2.x,R2.x       ; (N * (refr * D))^2
	add R2.y,C1.y,R2.y       ; 1 - refr^2 + R2.y
	dp3_rsq R2.y,R2,C.xyxx   ; 1 / sqrt R2.y
	mul R2.y,S.y,R2.y        ; sqrt R2.y
	add R2.x,-R2.x,-R2.y    
	mad R2,R2.x,R6,R1        ; R2 contains refracted direction
	mad R1,C1.w,R2,R7        ; go delta away

	; shoot refraction ray
	dp3_rcp R2.x,R2,C.yxxx
	dp3_rcp R2.y,R2,C.xyxx
	dp3_rcp R2.z,R2,C.xxyx
par    (mov E0,R1 
	mov E1,S
	mov R5,C3.yyzz
	mov E2,C3 + trace push 1 rty 0)

	; call material shader
	mov E0,H + call push 1 rty 0

	; combine color
        ; keine konstante frei -> benutze C1.y = -1.44
        mov R2.x,-C1.y ; R2.x = 0.44
        add R2.x,R2.x,R2.x ; R2.x = 0.88
        ; mov R2.x,C.y ; R2.x = 1.0

        add R3.w,C.y,-R2.x ; R3.w = 0.12
        mul R3,R0.www,R3.www ; Cosinus * R3.w
        ; mov R3.z,C.x  ; eine gelbe kugel (b=0.0)
        ; mov R3.yz,C.xx  ; eine rote  kugel (g,b=0.0)
        mov R3.xy,C.xx  ; eine blaue kugel (r,g=0.0)
        add R0,R0,R3 
        mad R0,R1,R2.x,R0
        ; mad R0,R1,C0.w,R0
	;mov R0,R1
	+ return or xyzw (>=0 or <1)




