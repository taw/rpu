;--------------------------------------------------------------------
;
; Möller Trumbore intersection shader
;
; 05.09.2005, Sven Woop
;

	; load inverse triangle transformation
	ld4x PRETRI,0

	; compute E1 and E2 and T
	mov R8,I2
par    (add R6,-R8,I0
	add R7,-R8,I1
        add R8,-R8,R2)

	; compute P and Q
par    (mul R9,R3.yzxw,R7.zxyw
        mul R10,R8.yzxw,R6.zxyw)

par    (mad R9,-R3.zxyw,R7.yzxw,R9
        mad R10,-R8.zxyw,R6.yzxw,R10)

	; compute (u,v,d)
par    (dp3_rcp R6.w,R9,R6
	dp3 R6.x,R9,R8
	dp3 R6.y,R10,R3
	dp3 R6.z,R10,R7)

	mul R6,R6,S.w 
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
	mov A.zw,I3
	;mov R5,I3
	mov R4.w,I3.w
	mov R5.w,I3.z
	mov R4.xyz,R6 
        + return or xyzw (>=0 or <1))


