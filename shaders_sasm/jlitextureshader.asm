;--------------------------------------------------------------------
; Texture shader mit Diskokugellicht
;
; 22.01.2005, Jörg Schmittler (bases on textureshader by Sven Woop)
;

        ; input
        ;    R0 = org
        ;    R1 = dir
        ;    R2 = *
        ;    R3 = *
        ;    R4 = (u,v,d,*)
        ;    A0 = obj addr
        ;    A1 = tri addr
        ;    C0 = lightpos
        ;    C1 = lightcol.xyz, w=ambience-factor
        ;    C2 = xy=0.999,1.001, zw=<disco-texture>


        ; output
        ;    R0 = color


	; debug exit: return uv-coordinates
	;mov R0, R4.xyx + exit 0

	; input
	; R0 = org
	; R1 = dir
	; R2 = *
	; R3 = *
	; R4 = (u,v,d,*)
	; A0 = obj addr
	; A1 = tri addr

	; output
	; R0 = color

	; compute hit point
	mad R0,R4.z,R1,R0

	; load texture transformation
	ld4x TRI,4

	; compute texture coordinates
par    (dph2 R1.x,I1,R4
	dph2 R1.y,I2,R4
        mov R2.xy,I0)

	; sample nearest
	texld_word_fp I0,R2,R1
	mov R1,I0	

	; bilinear interpolation
	;texld4x R2,R1
	;frac R2.xy,R1
	;add R2.zw,ONE,-R2.xyxy
	;mul R2,R2.zxzx,R2.wwyy
	;mul R1,R2.x,I0
	;mad R1,R2.y,I1,R1
	;mad R1,R2.z,I2,R1
	;mad R1,R2.w,I3,R1

        ; debug exit: return flat shaded texture
        ;mov R0,R1 + exit 0

	; shoot light ray
	mov R2,C0
	add R3,-C0,R0

	; interpolate normal
	ld4x TRI,7
	add R4.w,R4.x,R4.y
	add R4.w,C.y,-R4.w
	mul R5,R4.x,I0
	mad R5,R4.y,I1,R5
	mad R5,R4.w,I2,R5

	; lichtabfall + cos
	dp3_rsq R3.w,R3,R3
	mul R4,S.w,R3
	mov R3.w,S.w
	mul R3.w,R3,R3 ; abfallintensitaet

	dp3 R1.w,R4,R5 + jmp neg, w <0 ; cos * abfall_int
	
pos:	mul R1.w,R1.w,R3.w + jmp cont, or w (>=0 or <1)
neg:    mul R1.w,-R1.w,R3.w 
cont:

	;dp3_rsq R3.w,R3,R3
	;mul R4,S.w,R3
	;ld I0,A1,7
	;dp3_sat R1.w,R4,I0
	;mov R3.w,S.w
	;mul R3.w,R3,R3
	;mul R1.w,R1.w,R3.w

	; mul R7,R1.w,C1        ; multiply lightcol with light-ray-intensity

        ; HIER INTERSECTION RAY-SPHERE BERECHNEN
        ; ERGEBNIS: UV-Koordinaten auf Kugel

	texld_word_fp I0,C2.zw,R3
	mul R7,R1.w,I0          ; multiply lightcol with light-ray-intensity

	dp3_rcp R4.x,R3,C.yxxx ; S.x = 1/R3 dot (1,0,0,0)
	dp3_rcp R4.y,R3,C.xyxx
	dp3_rcp R4.z,R3,C.xxyx
par	(mov E0,R2
	mov E1,S
	mov R6,C3.yyzz
	mov E2,C3 + trace push 2 rty 1)

	; call material shader
	mov E0,H + call push 2 rty 1

	;add R2.x,C2.x,-R2.z + jmp nolight,x >=0
	;add R2.y,-C2.y,R2.z + jmp nolight,y >=0
	
	add R0,R7,C1.w           ; add ambient part
	mul R0,R0,R1 + return or xyzw (>=0 or <1)    ; multiply light with color
	;mul R0,C1,R1 + exit 0

;nolight: mul R0,C1.w,R1 + exit 0
