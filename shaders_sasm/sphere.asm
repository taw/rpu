;--------------------------------------------------------------------
;
; sphere intersection shader
;
; 21.12.2004, Sven Woop
;

	; load object transformation data
	ld4x PREOBJ,0

	; transform ray to object space
par    (dph3 R2.x,I0,R0
	dph3 R2.y,I1,R0
	dph3 R2.z,I2,R0
	dp3 R3.x,I0,R1
	dp3 R3.y,I1,R1
	dp3 R3.z,I2,R1)

	; load cube data
	ld I3,PRETRI,3

	; compute a,b,c
par	(dp3 R8.x,R3,R3
	dp3 R8.y,R2,R3
	dp3 R8.z,R2,R2)

	add R8.z,R8,-C.y
	
	; solve R8.x * x^2 + 2 * R8.y * x + R8.z = 0
	add R8.xyz,R8,R8 
	mul R8.zw,R8.xxyx,R8.xxyz
	dp4_rcp R6.x,R8,C3.xyyy
	add R8.w,R8.z,-R8.w + jmp nohit, w <0
	dp4_rsq R6.y,R8,C3.yyyx
	mov R6,S
	dp4_rcp R6.y,R6,C3.yxyy

	; two solutions
par    	(add R6.x,-R8.y,S.y
	add R6.y,-R8.y,-S.y)

par     (mul R6.x,R6.x,S.x
	 mul R6.y,R6.y,S.x)

;	mov R6.xy,R6 + jmp nohit, and xy <0   ; terminate if both solutions negative
;	mov R6.xy,R6 + jmp first, and y <0   ; first solution is positive
;	add R6.z,R6.x,-R6.y + jmp first,z <0  ; if both are positive take closest one

;second:	mov R6.z,R6.y + jmp check, or z (>=0 or <1)
;first:  mov R6.z,R6.x 

	mov R6.z,R6.y  ; take only closer solution

	; terminate if d > hitdist 
check:	add R7.z,R6,-R4 
	+jmp nohit,z >=0

	; terminate if d < 0
	mov R7.z,R6
	+jmp nohit,z <0
	
	; save hit data and exit with hit found
par    (mov E2.z,R6.z
	mov E2.x,I3
	mov R4.xyz,R6 + return or xyzw (>=0 or <1))

nohit:	; exit with no hit found
	return or xyzw (>=0 or <1)

