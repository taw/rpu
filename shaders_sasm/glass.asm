;--------------------------------------------------------------------
;
; glass shader
;
; 03.03.2005, Sven Woop
;

	
par    (mov R2,R4                   ; store hit info
	mad R0,R4.z,R1,R0)          ; compute hit position    
        mov R3,C0                   ; origin of light ray
        add R4,-C0,R0               ; direction of light ray

	mov R5,H                    ; store address of triangle
	mov R0.w,R5.y                
	
	dp3_rsq R4.w,R4,R4          ; compute light fraction reaching hit point
	mov R4.w,S.w
	mul R4.w,R4,R4              ; compute 1/distance^2
	mul R8,C0.w,R4.w            ; set R8 to light intensity at hit point

par    (dp3_rcp E1.x,R4,C.yxxx      ; compute 1/dir.x
	dp3_rcp E1.y,R4,C.xyxx      ; compute 1/dir.y
	dp3_rcp E1.z,R4,C.xxyx)     ; compute 1/dir.z

;par    (mov E0,R3                   ; store ray origin 
;	mov E1,S                    ; store reciprocal direction
;	mov R7,C.xxww               ; initialize hit info
;	mov E2,C3                   ; set rayloss,min,max,root
;	+ trace push 3 rty 1)       ; trace shadow ray
;	mov E0,H + call push 3 rty 1; shade shadow

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

	mad R6,+0.01,R3,R0        ; go safety distance to reflection direction
	mad R7,+0.01,R1,R0        ; go safety distance to transparency direction
	
	; compute specular light intensity
	dp3_sat R1.w,R3,R4

	add R1.w,R1.w,-C.y
	mul R1.w,R1.w,R1.w
	mul R1.w,R1.w,C2.w
	add R1.w,R1.w,C.y
	dp4_rcp R1.w,R1.w,C.xxxy
	mul R8,S.w,R8          ; specular part multiplied with light intensity
	mad R0,+0.7,R8,R2       ; add diffuse and specular part together     

	dp3 R1.w,-R1,R5 + jmp neg1, w <0    ; dot product between normal and direction

	; shoot reflection ray  
cont1:	mov R3.w,-0.063
	add E0,H,R3.w + jmp norays, z >=0  ; recursion depth of 1

par    (mov R2,R1
	mov R1.xyz,R7
	mov R4,R3
	mov R3,R6)
		
par    (dp3_rcp E1.x,R4,C.yxxx   ; compute reciprocal direction
	dp3_rcp E1.y,R4,C.xyxx
	dp3_rcp E1.z,R4,C.xxyx)

par    (mov E0,R3                ; set origin
	mov E1,S                 ; set reciprocal direction
	mov R7,C.xxww            ; set hit info
	mov E2,C3                ; set rayloss,min,max,root
	+ trace push 3 rty 0)    ; trace reflection ray

	mov E0,H + call push 3 rty 0 ; call material shader

	add R2.w,C.y,-R1.w       ; compute reflectivity
	mul R2.w,R2.w,R2.w
	mul R2.w,+0.95,R2.w
	add R2.w,R2.w,+0.05
	mad R0,R2.w,R3,R0        ; add reflection part to color
	add R0.w,C.y,-R2.w

	; shoot transparency ray  
par    (dp3_rcp E1.x,R2,C.yxxx   ; compute reciprocal direction
	dp3_rcp E1.y,R2,C.xyxx
	dp3_rcp E1.z,R2,C.xxyx)

par    (mov E0,R1                ; set origin
	mov E1,S                 ; set reciprocal direction
	mov R5,C.xxww            ; set hit info
	mov E2,C3                ; set rayloss,min,max,root
	+ trace push 1 rty 0)    ; trace reflection ray

	mov E0,H + call push 1 rty 0 ; call material shader
	
	mad R0,R0.w,R1,R0               ; add transparency part to color
	return or xyzw (>=0 or <1)     ; terminate shader

norays:
	return or xyzw (>=0 or <1)     ; terminate shader
	

neg1:   mov R1.w,-R1.w + jmp cont1, or xyzw (>=0 or <1)
