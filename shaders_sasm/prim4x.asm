;--------------------------------------------------------------------
;
; Oversampling shader
;
; 18.01.2004, Sven Woop
;

	; save position
	mov R1,I1

	; initialize color accumulator and counters
	mov R0,C3.x

loop0:  add R2.w,R0.w,-C2.x + jmp finished0, w >=0
	add R0.w,R0.w,C.y

	mov R1.w,C3.x
loop1:  add R2.w,R1.w,-C2.y + jmp loop0, w >=0
	add R1.w,R1.w,C.y

	; compute subpixel direction
	mad R4,R0.w,C0,R1
	mad R4,R1.w,C1,R4

	; load matrix from memory
	mov A.z,C0.w     ; set A2 to matrix address
	ld4x A2,0

	; transform preliminary ray
par    (mov R2.x,I0.w
	mov R2.y,I1.w
	mov R2.z,I2.w
	dp3_rcp R3.x,I0,R4
	dp3_rcp R3.y,I1,R4
	dp3_rcp R3.z,I2,R4)

	; store transformed ray and hit distance
par	(mov E0.xyz,R2
	mov E1.xyz,S

	; initialize hit data
	mov R6,C.xxww

	; store root node and start traversal
	mov E2,C3 + trace push 2 rty 0)

	; call material shader
	mov E0,H + call push 2 rty 0

	; accumulate color
	add R0.xyz,R0,R2
	
 	+ jmp loop1, or x (>=0 or <1)
	
finished0:
	
	; store color and return
	mul O, R0, C2.z + return or xyzw (>=0 or <1)
	


	

