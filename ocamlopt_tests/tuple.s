	.data
	.globl	camlTuple__data_begin
camlTuple__data_begin:
	.text
	.globl	camlTuple__code_begin
camlTuple__code_begin:
	.data
	.long	4096
	.globl	camlTuple
camlTuple:
	.space	16
	.data
	.long	3319
camlTuple__1:
	.long	caml_curry2
	.long	5
	.long	camlTuple__dotprod_64
	.data
	.long	3319
camlTuple__2:
	.long	caml_tuplify3
	.long	-5
	.long	camlTuple__get_z_62
	.data
	.long	3319
camlTuple__3:
	.long	caml_tuplify3
	.long	-5
	.long	camlTuple__get_y_60
	.data
	.long	3319
camlTuple__4:
	.long	caml_tuplify3
	.long	-5
	.long	camlTuple__get_x_58
	.text
	.align	16
	.globl	camlTuple__get_x_58
	.type	camlTuple__get_x_58,@function
camlTuple__get_x_58:
.L100:
	ret
	.text
	.align	16
	.globl	camlTuple__get_y_60
	.type	camlTuple__get_y_60,@function
camlTuple__get_y_60:
.L101:
	movl	%ebx, %eax
	ret
	.text
	.align	16
	.globl	camlTuple__get_z_62
	.type	camlTuple__get_z_62,@function
camlTuple__get_z_62:
.L102:
	movl	%ecx, %eax
	ret
	.text
	.align	16
	.globl	camlTuple__dotprod_64
	.type	camlTuple__dotprod_64,@function
camlTuple__dotprod_64:
	subl	$40, %esp
.L103:
	movl	%eax, 4(%esp)
	movl	%ebx, %eax
	movl	%eax, 0(%esp)
	movl	camlTuple + 8, %ebx
	movl	(%ebx), %ecx
	call	*%ecx
.L104:
	fldl	(%eax)
	fstpl	8(%esp)
	movl	camlTuple + 8, %ebx
	movl	(%ebx), %ecx
	movl	4(%esp), %eax
	call	*%ecx
.L105:
	fldl	(%eax)
	fstpl	16(%esp)
	fldl	16(%esp)
	fmull	8(%esp)
	fstpl	24(%esp)
	movl	camlTuple + 4, %ebx
	movl	(%ebx), %ecx
	movl	0(%esp), %eax
	call	*%ecx
.L106:
	fldl	(%eax)
	fstpl	8(%esp)
	movl	camlTuple + 4, %ebx
	movl	(%ebx), %ecx
	movl	4(%esp), %eax
	call	*%ecx
.L107:
	fldl	(%eax)
	fstpl	16(%esp)
	fldl	16(%esp)
	fmull	8(%esp)
	fstpl	16(%esp)
	movl	camlTuple, %ebx
	movl	(%ebx), %ecx
	movl	0(%esp), %eax
	call	*%ecx
.L108:
	fldl	(%eax)
	fstpl	8(%esp)
	movl	camlTuple, %ebx
	movl	(%ebx), %ecx
	movl	4(%esp), %eax
	call	*%ecx
.L109:
	fldl	(%eax)
	fstpl	32(%esp)
	fldl	32(%esp)
	fmull	8(%esp)
	fstpl	8(%esp)
	fldl	8(%esp)
	faddl	16(%esp)
	fstpl	8(%esp)
	fldl	8(%esp)
	faddl	24(%esp)
	fstpl	8(%esp)
.L110:	movl	caml_young_ptr, %eax
	subl	$12, %eax
	movl	%eax, caml_young_ptr
	cmpl	caml_young_limit, %eax
	jb	.L111
	leal	4(%eax), %eax
	movl	$2301, -4(%eax)
	fldl	8(%esp)
	fstpl	(%eax)
	addl	$40, %esp
	ret
.L111:	call	caml_call_gc
.L112:	jmp	.L110
	.text
	.align	16
	.globl	camlTuple__entry
	.type	camlTuple__entry,@function
camlTuple__entry:
.L113:
	movl	$camlTuple__4, %eax
	movl	%eax, camlTuple
	movl	$camlTuple__3, %eax
	movl	%eax, camlTuple + 4
	movl	$camlTuple__2, %eax
	movl	%eax, camlTuple + 8
	movl	$camlTuple__1, %eax
	movl	%eax, camlTuple + 12
	movl	$1, %eax
	ret
	.text
	.globl	camlTuple__code_end
camlTuple__code_end:
	.data
	.globl	camlTuple__data_end
camlTuple__data_end:
	.long	0
	.globl	camlTuple__frametable
camlTuple__frametable:
	.long	7
	.long	.L112
	.word	44
	.word	0
	.align	4
	.long	.L109
	.word	44
	.word	0
	.align	4
	.long	.L108
	.word	44
	.word	1
	.word	4
	.align	4
	.long	.L107
	.word	44
	.word	2
	.word	0
	.word	4
	.align	4
	.long	.L106
	.word	44
	.word	2
	.word	0
	.word	4
	.align	4
	.long	.L105
	.word	44
	.word	2
	.word	0
	.word	4
	.align	4
	.long	.L104
	.word	44
	.word	2
	.word	0
	.word	4
	.align	4
