;--------------------------------------------------------------------
;
; SPU Debug code
;
; 01.02.2005, Sven Woop
;

        mov R0,C0
	add R0,R0,C1
	add R0,R0,-C1
	mul R0,C1,R0
	mov_rcp R0,C0
	mov R0,S + return or xyzw (>=0 and <1)

	; transform preliminary ray
par    (mov R0.x,C0.w
	mov R0.y,C1.w
	mov R0.z,C2.w
	dp3_rcp R1.x,C0,I1
	dp3_rcp R1.y,C1,I1
	dp3_rcp R1.z,C2,I1)

	; store transformed ray and hit distance
par	(mov E0,R0   
	 mov E1,S
	 mov E2,C3)
	
	; initialize hit data
	mov R4,C3.xxww + trace push 0 rty 0

	; store color and return
	mov O, R0 + return or xyzw (>=0 and <1) 
	


	

