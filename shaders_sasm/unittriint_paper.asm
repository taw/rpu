;--------------------------------------------------------------------
;
; Unit triangle intersection shader
;
; 07.10.2004, Sven Woop
;
		
	; load inverse triangle transformation
	ld4x A1,0
	
	; transform ray to object coordinate space
par    (dp3_rcp R7.z,I2,R3
	dp3 R7.y,I1,R3
	dp3 R7.x,I0,R3
	dph3 R6.x,I0,R2
	dph3 R6.y,I1,R2
	dph3 R6.z,I2,R2)
	
	; compute hit distance d
	mul R8.z,-R6,S + jmp nohit,z <0

	; compute u and v
	mad R8.xy,R8.zzzz,R7,R6
	+jmp nohit,or xy (<0 or >=1) 

	; compute u+v
	add R9.z,R8.x,R8.y
	+jmp nohit,z >=1

	; terminate if d > hitdist 
	add R9.z,R8,-R4
	+jmp nohit,z >=0

	; terminate if d < 0
	;mov R9.z,R8

	
	; save hit data and exit with hit found
par    (
	mov R4.xyz,R8 + mov RC5.w,I3
	mov H.w,R8.z + exit 1
	;mov RC7.w,C0 
	)

nohit:	; exit with no hit found
	exit 0

