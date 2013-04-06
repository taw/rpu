; This is not true Phong shading, there's no reflection part
; Ia - ambient intensity - 0.4
; Id - diffuse intensity - 0.6
; Is - specular intensity = 0

; Values in the registers:
; P  - (x, y, 0, 0)
; C0 - ray orig
; C1 - ray dir x transform
; C2 - ray dir y transform
; C3 - ray dir z transform
; C4 - trace parameters
; C5 - light source
; C6 - bg color

; Triangle layout:
; A* B* C* D*

; Vertex layout:
; x y z

; Triangle data layout:
; r g b

; Compute dir
dp2h R0.x, C1, P
dp2h R0.y, C2, P
dp2h R0.z, C3, P

; Normalize dir
dp3_rsq R0.w, R0, R0
mul R0.xyz, R0.xyz, S.w

; run TRACE
trace C0, R0, C4

mov R1, HIT.z
+ jmp bg, and z (<0)

; Compute the hit point
mad R2.xyz, R0.xyz, R1.w, C0.xyz

; vector from hit point to the light source
add R9.xyz, C5.xyz, -R2.xyz

; normalize the light vector
dp3_rsq R9.w, R9, R9
mul R9.xyz, S.w, R9.xyz

; Load A, B, C, color
load I0, TRIADDR, 0 ; A* B* C* D*
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
dp3 R15.w, R0, R12
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

; No triangle hit, return
bg:

mov R0, C6
+ return or xyzw (>=0 or <1)
