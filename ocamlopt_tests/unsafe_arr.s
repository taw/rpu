	.data
	.globl	camlUnsafe_arr__data_begin
camlUnsafe_arr__data_begin:
	.text
	.globl	camlUnsafe_arr__code_begin
camlUnsafe_arr__code_begin:
	.data
	.long	4096
	.globl	camlUnsafe_arr
camlUnsafe_arr:
	.space	16
	.data
	.long	3319
camlUnsafe_arr__1:
	.long	caml_curry2
	.long	5
	.long	camlUnsafe_arr__dotprod_64
	.data
	.long	2295
camlUnsafe_arr__2:
	.long	camlUnsafe_arr__get_z_62
	.long	3
	.data
	.long	2295
camlUnsafe_arr__3:
	.long	camlUnsafe_arr__get_y_60
	.long	3
	.data
	.long	2295
camlUnsafe_arr__4:
	.long	camlUnsafe_arr__get_x_58
	.long	3
	.text
	.align	16
	.globl	camlUnsafe_arr__get_x_58
	.type	camlUnsafe_arr__get_x_58,@function
camlUnsafe_arr__get_x_58:
	subl	$8, %esp
.L100:
	movl	%eax, %ebx
.L101:	movl	caml_young_ptr, %eax
	subl	$12, %eax
	movl	%eax, caml_young_ptr
	cmpl	caml_young_limit, %eax
	jb	.L102
	leal	4(%eax), %eax
	movl	$2301, -4(%eax)
	fldl	(%ebx)
	fstpl	(%eax)
	addl	$8, %esp
	ret
.L102:	call	caml_call_gc
.L103:	jmp	.L101
	.text
	.align	16
	.globl	camlUnsafe_arr__get_y_60
	.type	camlUnsafe_arr__get_y_60,@function
camlUnsafe_arr__get_y_60:
	subl	$8, %esp
.L104:
	movl	%eax, %ebx
.L105:	movl	caml_young_ptr, %eax
	subl	$12, %eax
	movl	%eax, caml_young_ptr
	cmpl	caml_young_limit, %eax
	jb	.L106
	leal	4(%eax), %eax
	movl	$2301, -4(%eax)
	fldl	8(%ebx)
	fstpl	(%eax)
	addl	$8, %esp
	ret
.L106:	call	caml_call_gc
.L107:	jmp	.L105
	.text
	.align	16
	.globl	camlUnsafe_arr__get_z_62
	.type	camlUnsafe_arr__get_z_62,@function
camlUnsafe_arr__get_z_62:
	subl	$8, %esp
.L108:
	movl	%eax, %ebx
.L109:	movl	caml_young_ptr, %eax
	subl	$12, %eax
	movl	%eax, caml_young_ptr
	cmpl	caml_young_limit, %eax
	jb	.L110
	leal	4(%eax), %eax
	movl	$2301, -4(%eax)
	fldl	16(%ebx)
	fstpl	(%eax)
	addl	$8, %esp
	ret
.L110:	call	caml_call_gc
.L111:	jmp	.L109
	.text
	.align	16
	.globl	camlUnsafe_arr__dotprod_64
	.type	camlUnsafe_arr__dotprod_64,@function
camlUnsafe_arr__dotprod_64:
	subl	$8, %esp
.L112:
	movl	%eax, %ecx
.L113:	movl	caml_young_ptr, %eax
	subl	$12, %eax
	movl	%eax, caml_young_ptr
	cmpl	caml_young_limit, %eax
	jb	.L114
	leal	4(%eax), %eax
	movl	$2301, -4(%eax)
	fldl	8(%ecx)
	fmull	8(%ebx)
	fldl	(%ecx)
	fmull	(%ebx)
	faddp	%st, %st(1)
	fldl	16(%ecx)
	fmull	16(%ebx)
	faddp	%st, %st(1)
	fstpl	(%eax)
	addl	$8, %esp
	ret
.L114:	call	caml_call_gc
.L115:	jmp	.L113
	.text
	.align	16
	.globl	camlUnsafe_arr__entry
	.type	camlUnsafe_arr__entry,@function
camlUnsafe_arr__entry:
.L116:
	movl	$camlUnsafe_arr__4, %eax
	movl	%eax, camlUnsafe_arr
	movl	$camlUnsafe_arr__3, %eax
	movl	%eax, camlUnsafe_arr + 4
	movl	$camlUnsafe_arr__2, %eax
	movl	%eax, camlUnsafe_arr + 8
	movl	$camlUnsafe_arr__1, %eax
	movl	%eax, camlUnsafe_arr + 12
	movl	$1, %eax
	ret
	.text
	.globl	camlUnsafe_arr__code_end
camlUnsafe_arr__code_end:
	.data
	.globl	camlUnsafe_arr__data_end
camlUnsafe_arr__data_end:
	.long	0
	.globl	camlUnsafe_arr__frametable
camlUnsafe_arr__frametable:
	.long	4
	.long	.L115
	.word	12
	.word	2
	.word	3
	.word	5
	.align	4
	.long	.L111
	.word	12
	.word	1
	.word	3
	.align	4
	.long	.L107
	.word	12
	.word	1
	.word	3
	.align	4
	.long	.L103
	.word	12
	.word	1
	.word	3
	.align	4
