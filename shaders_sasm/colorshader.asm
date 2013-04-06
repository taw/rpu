;--------------------------------------------------------------------
;
; Color shader
;
; 30.11.2004, Sven Woop
;

	; store uv-coordinates and return
	;mov R0, R4.xyx + return or xyzw (>=0 or <1)

	; compute hit point
	mad R0,R4.z,R1,R0

	; load color
	;add R0,ONE,-ONE
	;ld I0,A1,10
        ;add R0,R0,I0
	;ld I1,A1,10
        ;add R0,-R0,I1 + exit 1
	
	; shoot light ray
        mov R2,C0
        add R3,-C0,R0

	; load color
	ld I0,TRI,10
	mov R0,I0
	;mov R0,I0 + return or xyzw (>=0 or <1)

	; interpolate normal
	ld4x TRI,7
	add R4.w,R4.x,R4.y
	add R4.w,C.y,-R4.w
	mul R5,R4.x,I0
	mad R5,R4.y,I1,R5
	mad R5,R4.w,I2,R5

	;mov R0,R0 + return or xyzw (>=0 or <1)

	dp3_rsq R3.w,R3,R3
	mul R4,S.w,R3
	mov R3.w,S.w
	mul R3.w,R3,R3

	dp3 R1.w,R4,R5 + jmp neg, w <0
	;mul R1.w,R1.w,R3.w
	
pos:	mul R1.w,R1.w,R3.w + jmp cont, or w (>=0 or <1)
neg:    mul R1.w,-R1.w,R3.w 

cont:

	mul R7,R1.w,C1          ; store light intensity
	dp3_rcp R4.x,R3,C.yxxx ; S.x = 1/R3 dot (1,0,0,0)
	dp3_rcp R4.y,R3,C.xyxx
	dp3_rcp R4.z,R3,C.xxyx
par	(mov E0,R2 
	mov E1,S
	mov R6,C3.yyzz
	mov E2,C3 + trace push 2 rty 1)

;mov R0,R7 + return or xyzw (>=0 or <1)

	; call material shader
	mov E0,H + call push 2 rty 1
	add R2,R7,C1.w	         ; add ambient part
	mul R1,R1.w,R2          ; store light intensity
	mul R0,R0,R1 + return or xyzw (>=0 or <1)    ; multiply light with color

	;mul R0,R0,R1.w + return or xyzw (>=0 or <1)	
	
	


	

