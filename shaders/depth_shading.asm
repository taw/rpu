; <0.8     - green
; 0.8..1.8 - from green to pink
; >1.8     - pink
add_sat R15.y, 1.8, -R0.z
add_sat R15.xz, 1.0, -R15.y
; unconditional return
+ return or xyzw (>=0 or <1)
