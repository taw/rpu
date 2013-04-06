	.data
	.globl	camlArr__data_begin
camlArr__data_begin:
	.text
	.globl	camlArr__code_begin
camlArr__code_begin:
	.data
	.long	4096
	.globl	camlArr
camlArr:
	.space	16
	.data
	.long	3319
camlArr__1:
	.long	caml_curry2
	.long	5
	.long	camlArr__dotprod_64
	.data
	.long	2295
camlArr__2:
	.long	camlArr__get_z_62
	.long	3
	.data
	.long	2295
camlArr__3:
	.long	camlArr__get_y_60
	.long	3
	.data
	.long	2295
camlArr__4:
	.long	camlArr__get_x_58
	.long	3
	.text
	.align	16
	.globl	camlArr__get_x_58
	.type	camlArr__get_x_58,@function
camlArr__get_x_58:
	subl	$8, %esp
.L100:
	movl	%eax, %ecx
.L101:	movl	caml_young_ptr, %eax
	subl	$12, %eax
	movl	%eax, caml_young_ptr
	cmpl	caml_young_limit, %eax
	jb	.L102
	leal	4(%eax), %eax
	movl	$2301, -4(%eax)
	movl	-4(%ecx), %ebx
	shrl	$10, %ebx
	cmpl	$1, %ebx
	jbe	.L104
	fldl	(%ecx)
	fstpl	(%eax)
	addl	$8, %esp
	ret
.L102:	call	caml_call_gc
.L103:	jmp	.L101
.L104:	call	caml_ml_array_bound_error
	.text
	.align	16
	.globl	camlArr__get_y_60
	.type	camlArr__get_y_60,@function
camlArr__get_y_60:
	subl	$8, %esp
.L105:
	movl	%eax, %ecx
.L106:	movl	caml_young_ptr, %eax
	subl	$12, %eax
	movl	%eax, caml_young_ptr
	cmpl	caml_young_limit, %eax
	jb	.L107
	leal	4(%eax), %eax
	movl	$2301, -4(%eax)
	movl	-4(%ecx), %ebx
	shrl	$10, %ebx
	cmpl	$3, %ebx
	jbe	.L109
	fldl	8(%ecx)
	fstpl	(%eax)
	addl	$8, %esp
	ret
.L107:	call	caml_call_gc
.L108:	jmp	.L106
.L109:	call	caml_ml_array_bound_error
	.text
	.align	16
	.globl	camlArr__get_z_62
	.type	camlArr__get_z_62,@function
camlArr__get_z_62:
	subl	$8, %esp
.L110:
	movl	%eax, %ecx
.L111:	movl	caml_young_ptr, %eax
	subl	$12, %eax
	movl	%eax, caml_young_ptr
	cmpl	caml_young_limit, %eax
	jb	.L112
	leal	4(%eax), %eax
	movl	$2301, -4(%eax)
	movl	-4(%ecx), %ebx
	shrl	$10, %ebx
	cmpl	$5, %ebx
	jbe	.L114
	fldl	16(%ecx)
	fstpl	(%eax)
	addl	$8, %esp
	ret
.L112:	call	caml_call_gc
.L113:	jmp	.L111
.L114:	call	caml_ml_array_bound_error
	.text
	.align	16
	.globl	camlArr__dotprod_64
	.type	camlArr__dotprod_64,@function
camlArr__dotprod_64:
	subl	$8, %esp
.L115:
	movl	%eax, %ecx
.L116:	movl	caml_young_ptr, %eax
	subl	$12, %eax
	movl	%eax, caml_young_ptr
	cmpl	caml_young_limit, %eax
	jb	.L117
	leal	4(%eax), %eax
	movl	$2301, -4(%eax)
	movl	-4(%ebx), %edx
	shrl	$10, %edx
	cmpl	$3, %edx
	jbe	.L119
	fldl	8(%ebx)
	movl	-4(%ecx), %edx
	shrl	$10, %edx
	cmpl	$3, %edx
	jbe	.L119
	fldl	8(%ecx)
	fmulp	%st, %st(1)
	movl	-4(%ebx), %edx
	shrl	$10, %edx
	cmpl	$1, %edx
	jbe	.L119
	fldl	(%ebx)
	movl	-4(%ecx), %edx
	shrl	$10, %edx
	cmpl	$1, %edx
	jbe	.L119
	fldl	(%ecx)
	fmulp	%st, %st(1)
	faddp	%st, %st(1)
	movl	-4(%ebx), %edx
	shrl	$10, %edx
	cmpl	$5, %edx
	jbe	.L119
	fldl	16(%ebx)
	movl	-4(%ecx), %ebx
	shrl	$10, %ebx
	cmpl	$5, %ebx
	jbe	.L119
	fldl	16(%ecx)
	fmulp	%st, %st(1)
	faddp	%st, %st(1)
	fstpl	(%eax)
	addl	$8, %esp
	ret
.L117:	call	caml_call_gc
.L118:	jmp	.L116
.L119:	call	caml_ml_array_bound_error
	.text
	.align	16
	.globl	camlArr__entry
	.type	camlArr__entry,@function
camlArr__entry:
.L120:
	movl	$camlArr__4, %eax
	movl	%eax, camlArr
	movl	$camlArr__3, %eax
	movl	%eax, camlArr + 4
	movl	$camlArr__2, %eax
	movl	%eax, camlArr + 8
	movl	$camlArr__1, %eax
	movl	%eax, camlArr + 12
	movl	$1, %eax
	ret
	.text
	.globl	camlArr__code_end
camlArr__code_end:
	.data
	.globl	camlArr__data_end
camlArr__data_end:
	.long	0
	.globl	camlArr__frametable
camlArr__frametable:
	.long	4
	.long	.L118
	.word	12
	.word	2
	.word	3
	.word	5
	.align	4
	.long	.L113
	.word	12
	.word	1
	.word	5
	.align	4
	.long	.L108
	.word	12
	.word	1
	.word	5
	.align	4
	.long	.L103
	.word	12
	.word	1
	.word	5
	.align	4
