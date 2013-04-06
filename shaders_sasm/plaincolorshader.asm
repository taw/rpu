;--------------------------------------------------------------------
;
; Color shader
;
; 30.11.2004, Sven Woop
;

	; store uv-coordinates and return
	;mov R0, R4.xyx + exit 0

	; compute hit point
	;mad R0,R4.z,R1,R0

	; load color
	;add R0,ONE,-ONE
	;ld I0,A1,10
        ;add R0,R0,I0
	;ld I1,A1,10
        ;add R0,-R0,I1 + exit 1
	
	ld I0,TRI,10
	mov R0,I0 + return or xyzw (>=0 or <1)

	
	
	


	

