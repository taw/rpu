;--------------------------------------------------------------------
;
; Object intersection shader
;
; 07.10.2004, Sven Woop
;

	; load inverse object transformation and root node
	ld4x PREOBJ,0
	
	; transform ray to object coordinate space
par    (dph3 R2.x,I0,R0
	dph3 R2.y,I1,R0
	dph3 R2.z,I2,R0
	dp3_rcp R3.x,I0,R1
	dp3_rcp R3.y,I1,R1
	dp3_rcp R3.z,I2,R1)

	; store ray
par    (mov E0,R2
	mov E1,S
	mov E2.w,I3 + trace push 0 rty 0)

	; exit
	return or xyzw (>=0 or <1)
	

