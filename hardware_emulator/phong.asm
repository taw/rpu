; This is not true Phong shading, there's no reflection part
; Ia - ambient intensity - 0.4
; Id - diffuse intensity - 0.6
; Is - specular intensity = 0

; Values in the registers:
; R0 - (P.x P.y P.z hit_dist)
; R1 - (dir.x, dir.y, dir.z, ?)
; R2 - (1-u-v, u, v, ?)
; A0 - tri_data
; C0 - light source

; Triangle layout:
; A* B* C* D*

; Vertex layout:
; x y z

; Triangle data layout:
; r g b

; vector from hit point to the light source
add R9.xyz, C0.xyz, -R0.xyz

; normalize the light vector
dp3_rsq R9.w, R9, R9
mul R9.xyz, S.w, R9.xyz

; Load A, B, C, color
load I0, A0, 0 ; A* B* C* D*
mov A, I0

load I0, A0, 0 ; A.x A.y A.z ?
load I1, A1, 0 ; B.x B.y B.z ?
load I2, A2, 0 ; C.x C.y C.z ?
load I3, A3, 0 ; r g b ?

; compute the normal vector
; B-A
add R10.xyz, I1.xyz, -I0.xyz
; C-A
add R11.xyz, I2.xyz, -I0.xyz

; R12 = (B-A) x (C-A)
mul R12.xyz, R10.yzx, R11.zxy
mad R12.xyz, R10.zxy, -R11.yzx, R12.xyz
; normalize
dp3_rsq R12.w, R12, R12
mul R12.xyz, S.w, R12.xyz

; standarize for normal * dir <= 0
dp3 R15.w, R1, R12
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
mul R0.xyz, I3.xyz, R15.w
+ return or xyzw (>=0 or <1)
