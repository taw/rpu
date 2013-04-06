; This is not true Phong shading, there's no reflection part
; Ia - ambient intensity - 0.4
; Id - diffuse intensity - 0.6
; Is - specular intensity = 0

; vector from hit point to the light source
add R9.xyz, R8.xyz, -R3.xyz

; normalize the light vector
dp3_rsq R9.w, R9, R9
mul R9.xyz, S.w, R9.xyz

; compute the normal vector
; B-A
add R10.xyz, R5.xyz, -R4.xyz
; C-A
add R11.xyz, R6.xyz, -R4.xyz
; R12 = (B-A) x (C-A)
mul R12.xyz, R10.yzx, R11.zxy
mad R12.xyz, R10.zxy, -R11.yzx, R12.xyz
; normalize
dp3_rsq R15.w, R12, R12
mul R12.xyz, S.w, R12.xyz

; standarize for normal * dir <= 0
dp3 R15.w, R2, R12
+ jmp sign_ok, or w (<0)

; inverse normal if necessary
mov R12, -R12

sign_ok:

; normal * ray_dir
; if negative, set to 0
dp3_sat R15.w, R12, R9

; I = Ia + Id*(L*N)
mad_sat R15.w, 0.6, R15.w, 0.4
; color = triangle_color * I
mul R15.xyz, R7.xyz, R15.w
+ return or xyzw (>=0 or <1)
