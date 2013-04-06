; Sample Shader 

label0:	mov_sat	R0.x,R1.yx   
       +add	R0.zw,R1.wwww,R2.wwwz
	jmp	label0,and xy (<0 or >=1)
	exit	1
