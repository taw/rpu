;--------------------------------------------------------------------
;
; Stops light rays
;
; 17.12.2004, Sven Woop
;

	;mov R5,C.x + return or xyzw (>=0 or <1)
	;return or xyzw (>=0 or <1)

	add R2.x,+0.98,-R4.z 
	mov R2.x,R2.x + jmp nolight,x >=0

	add R2.y,-1.02,R4.z
	mov R2.y,R2.y + jmp nolight,y >=0

	;mov R5,R5 + exit 0
	return or xyzw (>=0 or <1)
	
nolight: mov R5.xyz,C.x + return or xyzw (>=0 or <1)


	; store uv-coordinates and return
	;mov R0, R4 + exit 0


	
	


	

