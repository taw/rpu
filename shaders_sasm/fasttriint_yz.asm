;--------------------------------------------------------------------
;
; Fast Triangle Intersection (yz-projection)
;
; 20.01.2005, Sven Woop
;
	ld4x A1,0              ; load triangle specification
   par (dph3 R6.x,I0,R2        ; transform origin
	dp3_rcp R6.y,I0,R3)    ; transform ray direction
	mul R7.zw,-R6.x,S.y    ; hit distance to R7.zw
	+ jmp nohit, z <0      ; no hit if hit lies behind us
	mad R6.xyz,R7.z,R3,R2  ; compute hit point
	+ mov RC5.w,I3         ; set shader id
	dph2 R7.x,I1,R6.yz     ; compute u coordinate
	+ jmp nohit, w <0      ; no hit if u < 0
	dph2 R7.y,I2,R6.yz     ; compute v coordinate
	+ jmp nohit, w <0      ; no hit if v < 0
	add R6.w,R7.z,-R4.z    ; terminate if d > hitdist 
	+jmp nohit,w >=0
	add R6.w,R7.x,R7.y     ; compute u+v
	+ jmp nohit, w >=1     ; no hit if u+v >= 1	
   par (mov R4.xyz,R7          ; remember hit info
	+ mov H.w,R7.w         ; set max value
        mov RC7.w,C0           ; set ishit
        + exit 1)	

nohit:	exit 0                 ; exit with no hit found
 
