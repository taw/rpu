;--------------------------------------------------------------------
;
; varnish shader
;
; 03.03.2005, Sven Woop
;

	
par    (mov R2,R4                   ; store hit info
	mad R0,R4.z,R1,R0)          ; compute hit position    
        mov R3,C0                   ; origin of light ray
        add R4,-C0,R0               ; direction of light ray
	mov R0.w,H.y                ; store address of triangle
	
	dp3_rsq R4.w,R4,R4          ; compute light fraction reaching hit point
	mov R4.w,S.w
	mul R4.w,R4,R4              ; compute 1/distance^2
	mul R8,C0.w,R4.w            ; set R7 to light intensity at hit point

par    (dp3_rcp E1.x,R4,C.yxxx      ; compute 1/dir.x
	dp3_rcp E1.y,R4,C.xyxx      ; compute 1/dir.y
	dp3_rcp E1.z,R4,C.xxyx)     ; compute 1/dir.z

par    (mov E0,R3                   ; store ray origin 
	mov E1,S                    ; store reciprocal direction
	mov R7,C.xxww               ; initialize hit info
	mov E2,C3                   ; set rayloss,min,max,root
	+ trace push 3 rty 1)       ; trace shadow ray
	mov E0,H + call push 3 rty 1; shade shadow

	mov A.x,R0.w                ; restore triangle address
	ld4x A0,7                   ; load triangle normal
	add R2.w,R2.x,R2.y          ; and interpolate it
	add R2.w,C.y,-R2.w
	mul R5,R2.x,I0
	mad R5,R2.y,I1,R5
	mad R5,R2.w,I2,R5

	dp3_rsq R5.w,R5,R5          ; normalize normal
	mul R5,S.w,R5

	dp3_rsq R4.w,R4,R4          ; normalize light direction
	mul R4,S.w,-R4

	dp3_rsq R1.w,R1,R1          ; normalize ray direction
	mul R1,S.w,R1

	; compute ambient light intensity
	dp3 R1.w,R4,R5 + jmp neg, w <0
pos:	mul R2,R1.w,R8 + jmp cont, or xyzw (>=0 or <1)
neg:    mul R2,-R1.w,R8 
cont:	

	ld I0,A0,10                ; load color
	mul R2,I0,R2               ; multiply incomming light with color

	; compute reflected direction
	dp3 R3.w,R5,R1           ; dot product between normal and direction
	add R3.w,R3.w,R3.w       ; 2* TODO: omtimize
	mad R3,-R3.w,R5,R1       ; d - 2 * (d*n) * n

	mad R6,0.01,R3,R0        ; go safety away
	
	; compute specular light intensity
	dp3_sat R1.w,R3,R4
	add R1.w,R1.w,-C.y
	mul R1.w,R1.w,R1.w
	mul R1.w,R1.w,C2.w
	add R1.w,R1.w,C.y
	dp4_rcp R1.w,R1.w,C.xxxy
	mul R1,S.w,R8          ; specular part multiplied with light intensity
	mad R0,12.0,R1,R2      ; add ambient and specular part together     

	; shoot reflection ray  
	mov R1,0.5               ; default color if we do not shoot reflection ray
	add E0,H.z,-0.063 + jmp norefl, z >=0  ; reflection depth of 2
par    (mov R1,R6
	mov R2,R3)
		
par    (dp3_rcp E1.x,R2,C.yxxx   ; compute reciprocal direction
	dp3_rcp E1.y,R2,C.xyxx
	dp3_rcp E1.z,R2,C.xxyx)

par    (mov E0,R1                ; set origin
	mov E1,S                 ; set reciprocal direction
	mov R5,C.xxww            ; set hit info
	mov E2,C3                ; set rayloss,min,max,root
	+ trace push 1 rty 0)    ; trace reflection ray

	mov E0,H + call push 1 rty 0 ; call material shader

norefl:
	mad R0,0.5,R1,R0               ; add reflection part to color
	+ return or xyzw (>=0 or <1)   ; terminate shader
	



