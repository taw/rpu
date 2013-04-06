;--------------------------------------------------------------------
;
; glass transparency shader
;
; 08.03.2005, Sven Woop
;


	; multiply transparency with light intensity
	mul R6,+0.8,R5

par    (add R4.w,+0.01,R4.z
	mov R2,R1)
	mad R1,R4.w,R1,R0
	mov R0,R4

	; shoot transparency ray  
par    (dp3_rcp E1.x,R2,C.yxxx   ; compute reciprocal direction
	dp3_rcp E1.y,R2,C.xyxx
	dp3_rcp E1.z,R2,C.xxyx)

par    (mov E0,R1                ; set origin
	mov E1,S                 ; set reciprocal direction
	mov R5,C.xxww            ; set hit info
	mov E2,C3                ; set rayloss,min,max,root
	+ trace push 1 rty 1)    ; trace reflection ray

	add R5.z,R0.z,R5.z

	mov E0,H + call push 1 rty 1 ; call material shader

	mov R5,R6 + return or xyzw (>=0 or <1)

