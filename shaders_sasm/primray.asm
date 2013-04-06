;--------------------------------------------------------------------
;
; Primary ray generation shader
;
; 07.10.2004, Sven Woop
;

	;add O, I1, -C.y + return or xyzw (>=0 or <1)

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
	 mov E2,C3
	 mov R4,C.xxww + trace push 0 rty 0)

	;mov O, R4.xyx + return or xyzw (>=0 or <1)
	
	; call material shader
	mov E0,H + call push 0 rty 0

	; store color and return
	mov O, R0 + return or xyzw (>=0 or <1)
	


	

