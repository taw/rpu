;--------------------------------------------------------------------
;
; empty geometry intersection shader
;
; 08.03.2005, Sven Woop
;

	; we never hit empty geometry		
	return or xyzw (>=0 or <1)

