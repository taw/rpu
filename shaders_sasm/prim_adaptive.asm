;--------------------------------------------------------------------
;
; Performs adaptive oversampling
;
; 22.06.2005, Sven Woop
;

;mov O,C0 + return or xyzw (>=0 or <1)

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

;--------------------------------------------------------------------
;
; Adaptive code at position 12
;


	;add R0,I1,-C.y
	;mov O, R0 + return or xyzw (>=0 or <1)
	
	; save position
	mov R1,I1

	; load old color
	dph2 R2.x,C0,I1
	dph2 R2.y,C1,I1

;texld_word_fp I0,C2.zw,R2.xy
;texld I0,C2.zw,R2.xy
;mov O, I0 + return or xyzw (>=0 or <1)

;mul R2.y,R2.y,+0.5

;add R0,R2.xy,-64.0
;mov R0,R0 + jmp notfound, and x <0
;add R0,R2.xy,-64.0
;mov R0,R0 + jmp notfound, and x >=1
;add R0,R2.xy,-96.0
;mov R0,R0 + jmp notfound, and y <0
;add R0,R2.xy,-96.0
;mov R0,R0 + jmp notfound, and y >=1

;mov O,+1.0
;return or xyzw (>=0 or <1)

;notfound: mov O,+0.0
;	  return or xyzw (>=0 or <1)

	texld_word_fp I0,C2.xy,R2.xy
;mul_sat O,R2.xy,+0.00390625 
;return or xyzw (>=0 or <1)
;mov O, I0 + return or xyzw (>=0 or <1)

;	mul R2,R2,+0.125
;	texld I0,C2.zw,R2.x
;mov O, I0 + return or xyzw (>=0 or <1)


	; initialize color accumulator and counters
	mov R0,I0

;mov O, R0 + return or xyzw (>=0 or <1)


	add R3,R2,C.yxxx
	texld_word_fp I0,C2.xy,R3
	add R3,R2,-C.yxxx
	texld_word_fp I1,C2.xy,R3
	add R3,R2,C.xyxx
	texld_word_fp I2,C2.xy,R3
	add R3,R2,-C.xyxx
	texld_word_fp I3,C2.xy,R3

	add R3,R0,-I0
	dp3 R5.x,R3,R3
	add R3,I1,-R0
	dp3 R5.y,R3,R3
	add R3,I2,-R0
	dp3 R5.z,R3,R3
	add R3,I3,-R0
	dp3 R5.w,R3,R3

	add R6.x,R5.x,-R5.y
        + jmp xgy, x >=0 

ygx:    add R6.x,R5.z,-R5.w
        + jmp ygxzgw, x >=0 

ygxwgz: add R6.x,R5.y,-R5.w
        + jmp my, x >=0 

	mov R5.x,R5.w
	+ jmp maxfin, or xyzw (>=0 or <1)

ygxzgw: add R6.x,R5.y,-R5.z
        + jmp my, x >=0 

	mov R5.x,R5.z
	+ jmp maxfin, or xyzw (>=0 or <1)

xgy:    add R6.x,R5.z,-R5.w
        + jmp xgyzgw, x >=0 

xgywgz: add R6.x,R5.y,-R5.w
        + jmp mx, x >=0 

	mov R5.x,R5.w
	+ jmp maxfin, or xyzw (>=0 or <1)

xgyzgw: add R6.x,R5.x,-R5.z
        + jmp mx, x >=0 

	mov R5.x,R5.z
	+ jmp maxfin, or xyzw (>=0 or <1)

mx:     mov R5.x,R5.x
	+ jmp maxfin, or xyzw (>=0 or <1)
my:     mov R5.x,R5.y
	+ jmp maxfin, or xyzw (>=0 or <1)
mz:     mov R5.x,R5.z
	+ jmp maxfin, or xyzw (>=0 or <1)
mw:     mov R5.x,R5.w
	+ jmp maxfin, or xyzw (>=0 or <1)

maxfin: mul_sat R5.x,+6.2,R5.x

;mov O,R5.x + return or xyzw (>=0 or <1)

	add R0.w,+0.0,R5.x
;mov R0.w,+1.0
	mov R1.w,C.x

loop:   add R0.w,R0.w,-C1.w
 	+ jmp finished, or w (<0 or >=1)

	; load jitter vector
	texld I0,C2.zw,R1.w
	add R4,R1,I0
	;mov R4,R1

;mov O,I0 + return or xyzw (>=0 or <1)

	add R1.w,R1.w,C.y	

	; set A2 to matrix address
	mov A.z,C0.w     
	ld4x A2,0

	; transform preliminary ray
par    (mov R2.x,I0.w
	mov R2.y,I1.w
	mov R2.z,I2.w
	dp3_rcp R3.x,I0,R4
	dp3_rcp R3.y,I1,R4
	dp3_rcp R3.z,I2,R4)

	; store transformed ray and hit distance
par	(mov E0,R2
	 mov E1,S
	 mov E2,C3
	 mov R6,C.xxww + trace push 2 rty 0)

	;mov O, R4.xyx + return or xyzw (>=0 or <1)
	
	; call material shader
	mov E0,H + call push 2 rty 0

;mov O,R2 + return or xyzw (>=0 or <1)

	; accumulate color
	add R0.xyz,R0,R2
	+ jmp loop, or xyzw (>=0 or <1)

finished:
	
	; normalize color and return
	add_rcp R1.w,R1.w,C.y
	mul O,R0,S.w + return or xyzw (>=0 or <1)

	;mov O, R0 + return or xyzw (>=0 or <1)


