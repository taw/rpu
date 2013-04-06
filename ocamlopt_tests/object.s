	.data
	.globl	camlObject__data_begin
camlObject__data_begin:
	.text
	.globl	camlObject__code_begin
camlObject__code_begin:
	.data
	.long	2048
	.globl	camlObject
camlObject:
	.space	8
	.data
	.long	2295
camlObject__2:
	.long	camlObject__vec_init_109
	.long	3
	.data
	.long	4096
camlObject__1:
	.long	.L100010
	.long	.L100009
	.long	.L100008
	.long	.L100011
	.data
	.long	4096
camlObject__3:
	.long	.L100008
	.long	.L100009
	.long	.L100010
	.long	.L100011
	.long	2300
.L100011:
	.ascii	"dotprod"
	.byte	0
	.long	2300
.L100010:
	.ascii	"get_x"
	.space	2
	.byte	2
	.long	2300
.L100009:
	.ascii	"get_y"
	.space	2
	.byte	2
	.long	2300
.L100008:
	.ascii	"get_z"
	.space	2
	.byte	2
	.data
	.long	3072
camlObject__4:
	.long	.L100005
	.long	.L100006
	.long	.L100007
	.long	1276
.L100007:
	.ascii	"z"
	.space	2
	.byte	2
	.long	1276
.L100006:
	.ascii	"y"
	.space	2
	.byte	2
	.long	1276
.L100005:
	.ascii	"x"
	.space	2
	.byte	2
	.text
	.align	16
	.globl	camlObject__method_dotprod_101
	.type	camlObject__method_dotprod_101,@function
camlObject__method_dotprod_101:
	subl	$44, %esp
.L100:
	movl	%eax, 8(%esp)
	movl	%ebx, %eax
	movl	%eax, 0(%esp)
	movl	%ecx, 4(%esp)
	movl	camlObject + 4, %ecx
	addl	$8, %ecx
	movl	$583092899, %ebx
	call	caml_send0
.L101:
	fldl	(%eax)
	fstpl	12(%esp)
	movl	4(%esp), %eax
	movl	20(%eax), %ecx
	movl	8(%esp), %eax
	movl	(%eax), %ebx
	movl	-2(%ebx, %ecx, 2), %ebx
	movl	(%ebx), %ecx
	call	*%ecx
.L102:
	fldl	(%eax)
	fstpl	20(%esp)
	fldl	20(%esp)
	fmull	12(%esp)
	fstpl	28(%esp)
	movl	camlObject + 4, %ecx
	addl	$4, %ecx
	movl	$583092897, %ebx
	movl	0(%esp), %eax
	call	caml_send0
.L103:
	fldl	(%eax)
	fstpl	12(%esp)
	movl	4(%esp), %eax
	movl	16(%eax), %ecx
	movl	8(%esp), %eax
	movl	(%eax), %ebx
	movl	-2(%ebx, %ecx, 2), %ebx
	movl	(%ebx), %ecx
	call	*%ecx
.L104:
	fldl	(%eax)
	fstpl	20(%esp)
	fldl	20(%esp)
	fmull	12(%esp)
	fstpl	20(%esp)
	movl	camlObject + 4, %ecx
	movl	$583092895, %ebx
	movl	0(%esp), %eax
	call	caml_send0
.L105:
	fldl	(%eax)
	fstpl	12(%esp)
	movl	4(%esp), %eax
	movl	12(%eax), %ecx
	movl	8(%esp), %eax
	movl	(%eax), %ebx
	movl	-2(%ebx, %ecx, 2), %ebx
	movl	(%ebx), %ecx
	call	*%ecx
.L106:
	fldl	(%eax)
	fstpl	36(%esp)
	fldl	36(%esp)
	fmull	12(%esp)
	fstpl	12(%esp)
	fldl	12(%esp)
	faddl	20(%esp)
	fstpl	12(%esp)
	fldl	12(%esp)
	faddl	28(%esp)
	fstpl	12(%esp)
.L107:	movl	caml_young_ptr, %eax
	subl	$12, %eax
	movl	%eax, caml_young_ptr
	cmpl	caml_young_limit, %eax
	jb	.L108
	leal	4(%eax), %eax
	movl	$2301, -4(%eax)
	fldl	12(%esp)
	fstpl	(%eax)
	addl	$44, %esp
	ret
.L108:	call	caml_call_gc
.L109:	jmp	.L107
	.text
	.align	16
	.globl	camlObject__fun_116
	.type	camlObject__fun_116,@function
camlObject__fun_116:
	subl	$16, %esp
.L110:
	movl	%ebx, %eax
	movl	%ecx, 0(%esp)
	movl	%edx, 8(%esp)
	movl	%esi, 12(%esp)
	movl	%edi, 4(%esp)
	movl	24(%edi), %ebx
	call	camlCamlinternalOO__create_object_opt_357
.L111:
	movl	%eax, %esi
	movl	0(%esp), %eax
	pushl	%eax
	movl	8(%esp), %ebx
	movl	12(%ebx), %eax
	lea	-2(%esi, %eax, 2), %eax
	pushl	%eax
	call	caml_modify
	addl	$8, %esp
	movl	8(%esp), %eax
	pushl	%eax
	movl	16(%ebx), %eax
	lea	-2(%esi, %eax, 2), %eax
	pushl	%eax
	call	caml_modify
	addl	$8, %esp
	movl	12(%esp), %eax
	pushl	%eax
	movl	20(%ebx), %eax
	lea	-2(%esi, %eax, 2), %eax
	pushl	%eax
	call	caml_modify
	addl	$8, %esp
	movl	%esi, %eax
	addl	$16, %esp
	ret
	.text
	.align	16
	.globl	camlObject__vec_init_109
	.type	camlObject__vec_init_109,@function
camlObject__vec_init_109:
	subl	$28, %esp
.L112:
	movl	%eax, 24(%esp)
	movl	$camlObject__4, %ecx
	movl	$camlObject__3, %ebx
	call	camlCamlinternalOO__new_methods_variables_296
.L113:
	movl	4(%eax), %ebx
	movl	%ebx, 4(%esp)
	movl	8(%eax), %ebx
	movl	%ebx, 0(%esp)
	movl	12(%eax), %edi
	movl	16(%eax), %ebx
	movl	%ebx, 8(%esp)
	movl	(%eax), %edx
	movl	%edx, 12(%esp)
	movl	%edx, %eax
	addl	$2, %eax
	movl	%eax, 16(%esp)
	movl	%edx, %ecx
	addl	$4, %ecx
	movl	%ecx, 20(%esp)
.L114:	movl	caml_young_ptr, %eax
	subl	$76, %eax
	movl	%eax, caml_young_ptr
	cmpl	caml_young_limit, %eax
	jb	.L115
	leal	4(%eax), %ebp
	movl	$6391, -4(%ebp)
	movl	$caml_curry2, (%ebp)
	movl	$5, 4(%ebp)
	movl	$camlObject__method_dotprod_101, 8(%ebp)
	movl	%edi, 12(%ebp)
	movl	0(%esp), %eax
	movl	%eax, 16(%ebp)
	movl	4(%esp), %esi
	movl	%esi, 20(%ebp)
	leal	28(%ebp), %ebx
	movl	$11264, -4(%ebx)
	movl	%edi, (%ebx)
	movl	$3, 4(%ebx)
	movl	%edx, 8(%ebx)
	movl	%eax, 12(%ebx)
	movl	$3, 16(%ebx)
	movl	16(%esp), %eax
	movl	%eax, 20(%ebx)
	movl	%esi, 24(%ebx)
	movl	$3, 28(%ebx)
	movl	%ecx, 32(%ebx)
	movl	8(%esp), %eax
	movl	%eax, 36(%ebx)
	movl	%ebp, 40(%ebx)
	movl	24(%esp), %eax
	call	camlCamlinternalOO__set_methods_640
.L117:
.L118:	movl	caml_young_ptr, %eax
	subl	$32, %eax
	movl	%eax, caml_young_ptr
	cmpl	caml_young_limit, %eax
	jb	.L119
	leal	4(%eax), %eax
	movl	$7415, -4(%eax)
	movl	$caml_curry5, (%eax)
	movl	$11, 4(%eax)
	movl	$camlObject__fun_116, 8(%eax)
	movl	12(%esp), %ebx
	movl	%ebx, 12(%eax)
	movl	16(%esp), %ebx
	movl	%ebx, 16(%eax)
	movl	20(%esp), %ebx
	movl	%ebx, 20(%eax)
	movl	24(%esp), %ebx
	movl	%ebx, 24(%eax)
	addl	$28, %esp
	ret
.L119:	call	caml_call_gc
.L120:	jmp	.L118
.L115:	call	caml_call_gc
.L116:	jmp	.L114
	.text
	.align	16
	.globl	camlObject__entry
	.type	camlObject__entry,@function
camlObject__entry:
.L121:
	pushl	$1
	pushl	$7
	movl	$caml_make_vect, %eax
	call	caml_c_call
.L122:
	addl	$8, %esp
	movl	%eax, camlObject + 4
	xorl	%eax, %eax
	movl	$camlObject__2, %ebx
	movl	$camlObject__1, %eax
	call	camlCamlinternalOO__make_class_335
.L123:
	movl	%eax, camlObject
	movl	$1, %eax
	ret
	.text
	.globl	camlObject__code_end
camlObject__code_end:
	.data
	.globl	camlObject__data_end
camlObject__data_end:
	.long	0
	.globl	camlObject__frametable
camlObject__frametable:
	.long	14
	.long	.L123
	.word	4
	.word	0
	.align	4
	.long	.L122
	.word	12
	.word	0
	.align	4
	.long	.L120
	.word	32
	.word	2
	.word	12
	.word	24
	.align	4
	.long	.L117
	.word	32
	.word	2
	.word	12
	.word	24
	.align	4
	.long	.L116
	.word	32
	.word	7
	.word	0
	.word	4
	.word	8
	.word	12
	.word	24
	.word	7
	.word	11
	.align	4
	.long	.L113
	.word	32
	.word	1
	.word	24
	.align	4
	.long	.L111
	.word	20
	.word	4
	.word	0
	.word	4
	.word	8
	.word	12
	.align	4
	.long	.L109
	.word	48
	.word	0
	.align	4
	.long	.L106
	.word	48
	.word	0
	.align	4
	.long	.L105
	.word	48
	.word	2
	.word	4
	.word	8
	.align	4
	.long	.L104
	.word	48
	.word	3
	.word	0
	.word	4
	.word	8
	.align	4
	.long	.L103
	.word	48
	.word	3
	.word	0
	.word	4
	.word	8
	.align	4
	.long	.L102
	.word	48
	.word	3
	.word	0
	.word	4
	.word	8
	.align	4
	.long	.L101
	.word	48
	.word	3
	.word	0
	.word	4
	.word	8
	.align	4
