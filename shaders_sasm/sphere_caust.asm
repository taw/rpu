;--------------------------------------------------------------------
;
; Compute faked caustic for sphere
;
; 14.01.2004, Sven Woop
;

	; compute intersection in world space
	;mad R7,R4.z,R1,R0
	
	; transform light position to object space
	ld4x OBJ,4
par(	dph3 R6.x,I0,C0
	dph3 R6.y,I1,C0
	dph3 R6.z,I2,C0)

	; transform direction to object space
par(	dp3 R3.x,I0,R1
	dp3 R3.y,I1,R1
	dp3 R3.z,I2,R1)

	dp3_rsq R3.w,R3,R3        ; normalize ray direction
	mul R3,S.w,R3

	dp3_rsq R6.w,R6,R6        ; normalize light vector
	mul R6,S.w,R6
	;mov R1.w,S.w

	; compute faked caustic
	dp3 R0.w,R3,R6

	mul R0.w,R0.w,R0.w
	mul R0.w,R0.w,R0.w
	mul R0.w,R0.w,R0.w
	mul R0.w,R0.w,R0.w

	add R0.w,R0.w,-C.y
	mul R0.w,R0.w,R0.w
	mul R0.w,R0.w,C1.z
	mul R0.w,R0.w,S.w
	add R0.w,R0.w,C.y
	dp4_rcp R0.w,R0.w,C.xxxy
	;mul R0.w,S.w,R1.w
	add R0.w,S.w,C0.w
	mul R5,R0.w,R5 + return or xyzw (>=0 or <1)


	
	


	

