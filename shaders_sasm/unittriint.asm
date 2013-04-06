;--------------------------------------------------------------------
;
; Unit triangle intersection shader
;
; 07.10.2004, Sven Woop
;
		
	; load inverse triangle transformation
	ld4x PRETRI,0
	
	; transform ray to object coordinate space
par    (dp3_rcp R7.z,I2,R3
	dp3 R7.y,I1,R3
	dp3 R7.x,I0,R3
	dph3 R6.x,I0,R2
	dph3 R6.y,I1,R2
	dph3 R6.z,I2,R2)
	
	; compute hit distance d
	mul R6.z,-R6,S.z 
	
	; compute u and v
	mad R6.xy,R6.zzzz,R7,R6
	+ return or xy (<0 or >=1) 

	; compute u+v
	add R6.w,R6.x,R6.y
	+ return w >=1

	; terminate if d > hitdist 
	add R6.w,R6.z,-R4.z
	+ return w >=0

	; terminate if d < 0
	mov R6.w,R6.z
	+ return w <0
	
	; save hit data and exit with hit found
par    (mov E2.x,I3
	mov E2.z,R6.z
	mov R4.xyz,R6 + return or xyzw (>=0 or <1))

;nohit:	; exit with no hit found
;	return or xyzw (>=0 or <1)

