;--------------------------------------------------------------------
; Envmap Texture Shader ( fuer RayLosses )
;
; 25.01.2005, Jörg Schmittler 
;

        ; input
        ;    R0 = org
        ;    R1 = dir
        ;    R2 = *
        ;    R3 = *
        ;    R4 = (u,v,d,*)
        ;    A0 = obj addr
        ;    A1 = tri addr
        ;    C0 = lightpos, w=0.5
        ;    C1 = lightcol.xyz, w=ambience-factor
        ;    C2 = xy=0.999,1.001, zw=<disco-texture>


        ; output
        ;    R0 = color


        ; texld I0, R2, R4    -->  target-reg, <u,v>, <tex-addr,tex-type>


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

        dp3_rsq R1.w,R1,R1        ; normalize ray direction
        mul R1,S.w,R1

 

        add R2.xy,R1.zy,C.y    ; dir.xy = 1.0 + dir.xy
        ; add R2.xy,ONE,-R1 ; dir.xy = 1.0 - dir.xy

        mul R2.xy,R2,C0.ww  ; uv = dir.xy * 0.5 * 128.0 


        ; mov R4.xyzw,C0.wwzz
        ; mul R2.xy,R2.xy,C0.ww
        ; mul R2.xy,R2,R4     ;  uv = dir.xy * 0.5
        ; mul R2.xy,R2,R4.zw  ;  tc = uv * 128.0


	; texld I0,C2.zw,R2.xy
	; texld I0,R1,C2.zw
        mov R4.xy, C2.zw   ; work-around fuer bug in texld

        ; sample nearest
	;texld I0,R4,R2
	;mov R0,I0 + return or xyzw (>=0 or <1)

	; bilinear interpolation
	texld4x_word_fp R4,R2
	frac R2.xy,R2
	add R2.zw,C.y,-R2.xyxy
	mul R2,R2.zxzx,R2.wwyy
	mul R1,R2.x,I0
	mad R1,R2.y,I1,R1
	mad R1,R2.z,I2,R1
	mad R1,R2.w,I3,R1
        mov R0,R1 + return or xyzw (>=0 or <1)

	;texld4x_word_fp R2,R1
	;frac R2.xy,R1
	;add R2.zw,C.y,-R2.xyxy
	;mul R2,R2.zxzx,R2.wwyy
	;mul R1,R2.x,I0
	;mad R1,R2.y,I1,R1
	;mad R1,R2.z,I2,R1
	;mad R1,R2.w,I3,R1
	;mov R0,R1 + return or xyzw (>=0 or <1)
