	.data
	.globl	camlRec__data_begin
camlRec__data_begin:
	.text
	.globl	camlRec__code_begin
camlRec__code_begin:
	.data
	.long	7168
	.globl	camlRec
camlRec:
	.space	28
	.data
	.long	3319
camlRec__1:
	.long	caml_curry2
	.long	5
	.long	camlRec__dotprod2_78
	.data
	.long	3319
camlRec__2:
	.long	caml_curry2
	.long	5
	.long	camlRec__dotprod_75
	.data
	.long	3319
camlRec__3:
	.long	caml_curry2
	.long	5
	.long	camlRec__unsafe_geti_72
	.data
	.long	3319
camlRec__4:
	.long	caml_curry2
	.long	5
	.long	camlRec__geti_70
	.data
	.long	2295
camlRec__5:
	.long	camlRec__get_z_68
	.long	3
	.data
	.long	2295
camlRec__6:
	.long	camlRec__get_y_66
	.long	3
	.data
	.long	2295
camlRec__7:
	.long	camlRec__get_x_64
	.long	3
	.data
	.long	5372
camlRec__8:
	.ascii	"Index out of range"
	.space	1
	.byte	1
	.text
	.align	16
	.globl	camlRec__get_x_64
	.type	camlRec__get_x_64,@function
camlRec__get_x_64:
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
	.globl	camlRec__get_y_66
	.type	camlRec__get_y_66,@function
camlRec__get_y_66:
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
	.globl	camlRec__get_z_68
	.type	camlRec__get_z_68,@function
camlRec__get_z_68:
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
	.globl	camlRec__geti_70
	.type	camlRec__geti_70,@function
camlRec__geti_70:
	subl	$8, %esp
.L116:
	movl	%eax, %ecx
	cmpl	$5, %ebx
	jbe	.L115
	movl	$camlRec__8, %eax
	addl	$8, %esp
	jmp	camlPervasives__failwith_37
	.align	16
.L115:
	sarl	$1, %ebx
	cmpl	$1, %ebx
	je	.L113
	jg	.L112
.L114:
.L117:	movl	caml_young_ptr, %eax
	subl	$12, %eax
	movl	%eax, caml_young_ptr
	cmpl	caml_young_limit, %eax
	jb	.L118
	leal	4(%eax), %eax
	movl	$2301, -4(%eax)
	fldl	(%ecx)
	fstpl	(%eax)
	addl	$8, %esp
	ret
	.align	16
.L113:
.L120:	movl	caml_young_ptr, %eax
	subl	$12, %eax
	movl	%eax, caml_young_ptr
	cmpl	caml_young_limit, %eax
	jb	.L121
	leal	4(%eax), %eax
	movl	$2301, -4(%eax)
	fldl	8(%ecx)
	fstpl	(%eax)
	addl	$8, %esp
	ret
	.align	16
.L112:
.L123:	movl	caml_young_ptr, %eax
	subl	$12, %eax
	movl	%eax, caml_young_ptr
	cmpl	caml_young_limit, %eax
	jb	.L124
	leal	4(%eax), %eax
	movl	$2301, -4(%eax)
	fldl	16(%ecx)
	fstpl	(%eax)
	addl	$8, %esp
	ret
.L124:	call	caml_call_gc
.L125:	jmp	.L123
.L121:	call	caml_call_gc
.L122:	jmp	.L120
.L118:	call	caml_call_gc
.L119:	jmp	.L117
	.text
	.align	16
	.globl	camlRec__unsafe_geti_72
	.type	camlRec__unsafe_geti_72,@function
camlRec__unsafe_geti_72:
	subl	$8, %esp
.L127:
	movl	%eax, %ecx
	movzbl	-4(%ecx), %eax
	cmpl	$254, %eax
	je	.L126
	movl	-2(%ecx, %ebx, 2), %eax
	addl	$8, %esp
	ret
	.align	16
.L126:
.L128:	movl	caml_young_ptr, %eax
	subl	$12, %eax
	movl	%eax, caml_young_ptr
	cmpl	caml_young_limit, %eax
	jb	.L129
	leal	4(%eax), %eax
	movl	$2301, -4(%eax)
	fldl	-4(%ecx, %ebx, 4)
	fstpl	(%eax)
	addl	$8, %esp
	ret
.L129:	call	caml_call_gc
.L130:	jmp	.L128
	.text
	.align	16
	.globl	camlRec__dotprod_75
	.type	camlRec__dotprod_75,@function
camlRec__dotprod_75:
	subl	$8, %esp
.L131:
	movl	%eax, %ecx
.L132:	movl	caml_young_ptr, %eax
	subl	$12, %eax
	movl	%eax, caml_young_ptr
	cmpl	caml_young_limit, %eax
	jb	.L133
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
.L133:	call	caml_call_gc
.L134:	jmp	.L132
	.text
	.align	16
	.globl	camlRec__dotprod2_78
	.type	camlRec__dotprod2_78,@function
camlRec__dotprod2_78:
	subl	$32, %esp
.L147:
	movl	%eax, %ecx
	movzbl	-4(%ebx), %eax
	cmpl	$254, %eax
	je	.L146
	movl	(%ebx), %eax
	jmp	.L145
	.align	16
.L146:
.L148:	movl	caml_young_ptr, %eax
	subl	$12, %eax
	movl	%eax, caml_young_ptr
	cmpl	caml_young_limit, %eax
	jb	.L149
	leal	4(%eax), %eax
	movl	$2301, -4(%eax)
	fldl	(%ebx)
	fstpl	(%eax)
.L145:
	fldl	(%eax)
	fstpl	8(%esp)
	movzbl	-4(%ecx), %eax
	cmpl	$254, %eax
	je	.L144
	movl	8(%ecx), %eax
	jmp	.L143
	.align	16
.L144:
.L151:	movl	caml_young_ptr, %eax
	subl	$12, %eax
	movl	%eax, caml_young_ptr
	cmpl	caml_young_limit, %eax
	jb	.L152
	leal	4(%eax), %eax
	movl	$2301, -4(%eax)
	fldl	16(%ecx)
	fstpl	(%eax)
.L143:
	fldl	(%eax)
	fstpl	0(%esp)
	fldl	0(%esp)
	fmull	8(%esp)
	fstpl	24(%esp)
	movzbl	-4(%ebx), %eax
	cmpl	$254, %eax
	je	.L142
	movl	4(%ebx), %eax
	jmp	.L141
	.align	16
.L142:
.L154:	movl	caml_young_ptr, %eax
	subl	$12, %eax
	movl	%eax, caml_young_ptr
	cmpl	caml_young_limit, %eax
	jb	.L155
	leal	4(%eax), %eax
	movl	$2301, -4(%eax)
	fldl	8(%ebx)
	fstpl	(%eax)
.L141:
	fldl	(%eax)
	fstpl	8(%esp)
	movzbl	-4(%ecx), %eax
	cmpl	$254, %eax
	je	.L140
	movl	4(%ecx), %eax
	jmp	.L139
	.align	16
.L140:
.L157:	movl	caml_young_ptr, %eax
	subl	$12, %eax
	movl	%eax, caml_young_ptr
	cmpl	caml_young_limit, %eax
	jb	.L158
	leal	4(%eax), %eax
	movl	$2301, -4(%eax)
	fldl	8(%ecx)
	fstpl	(%eax)
.L139:
	fldl	(%eax)
	fstpl	0(%esp)
	fldl	0(%esp)
	fmull	8(%esp)
	fstpl	16(%esp)
	movzbl	-4(%ebx), %eax
	cmpl	$254, %eax
	je	.L138
	movl	(%ebx), %eax
	jmp	.L137
	.align	16
.L138:
.L160:	movl	caml_young_ptr, %eax
	subl	$12, %eax
	movl	%eax, caml_young_ptr
	cmpl	caml_young_limit, %eax
	jb	.L161
	leal	4(%eax), %eax
	movl	$2301, -4(%eax)
	fldl	(%ebx)
	fstpl	(%eax)
.L137:
	fldl	(%eax)
	fstpl	8(%esp)
	movzbl	-4(%ecx), %eax
	cmpl	$254, %eax
	je	.L136
	movl	(%ecx), %eax
	jmp	.L135
	.align	16
.L136:
.L163:	movl	caml_young_ptr, %eax
	subl	$12, %eax
	movl	%eax, caml_young_ptr
	cmpl	caml_young_limit, %eax
	jb	.L164
	leal	4(%eax), %eax
	movl	$2301, -4(%eax)
	fldl	(%ecx)
	fstpl	(%eax)
.L135:
	fldl	(%eax)
	fstpl	0(%esp)
	fldl	0(%esp)
	fmull	8(%esp)
	fstpl	0(%esp)
	fldl	0(%esp)
	faddl	16(%esp)
	fstpl	0(%esp)
	fldl	0(%esp)
	faddl	24(%esp)
	fstpl	0(%esp)
.L166:	movl	caml_young_ptr, %eax
	subl	$12, %eax
	movl	%eax, caml_young_ptr
	cmpl	caml_young_limit, %eax
	jb	.L167
	leal	4(%eax), %eax
	movl	$2301, -4(%eax)
	fldl	0(%esp)
	fstpl	(%eax)
	addl	$32, %esp
	ret
.L167:	call	caml_call_gc
.L168:	jmp	.L166
.L164:	call	caml_call_gc
.L165:	jmp	.L163
.L161:	call	caml_call_gc
.L162:	jmp	.L160
.L158:	call	caml_call_gc
.L159:	jmp	.L157
.L155:	call	caml_call_gc
.L156:	jmp	.L154
.L152:	call	caml_call_gc
.L153:	jmp	.L151
.L149:	call	caml_call_gc
.L150:	jmp	.L148
	.text
	.align	16
	.globl	camlRec__entry
	.type	camlRec__entry,@function
camlRec__entry:
.L169:
	movl	$camlRec__7, %eax
	movl	%eax, camlRec
	movl	$camlRec__6, %eax
	movl	%eax, camlRec + 4
	movl	$camlRec__5, %eax
	movl	%eax, camlRec + 8
	movl	$camlRec__4, %eax
	movl	%eax, camlRec + 12
	movl	$camlRec__3, %eax
	movl	%eax, camlRec + 16
	movl	$camlRec__2, %eax
	movl	%eax, camlRec + 20
	movl	$camlRec__1, %eax
	movl	%eax, camlRec + 24
	movl	$1, %eax
	ret
	.text
	.globl	camlRec__code_end
camlRec__code_end:
	.data
	.globl	camlRec__data_end
camlRec__data_end:
	.long	0
	.globl	camlRec__frametable
camlRec__frametable:
	.long	15
	.long	.L168
	.word	36
	.word	0
	.align	4
	.long	.L165
	.word	36
	.word	1
	.word	5
	.align	4
	.long	.L162
	.word	36
	.word	2
	.word	3
	.word	5
	.align	4
	.long	.L159
	.word	36
	.word	2
	.word	3
	.word	5
	.align	4
	.long	.L156
	.word	36
	.word	2
	.word	3
	.word	5
	.align	4
	.long	.L153
	.word	36
	.word	2
	.word	3
	.word	5
	.align	4
	.long	.L150
	.word	36
	.word	2
	.word	3
	.word	5
	.align	4
	.long	.L134
	.word	12
	.word	2
	.word	3
	.word	5
	.align	4
	.long	.L130
	.word	12
	.word	2
	.word	3
	.word	5
	.align	4
	.long	.L125
	.word	12
	.word	1
	.word	5
	.align	4
	.long	.L122
	.word	12
	.word	1
	.word	5
	.align	4
	.long	.L119
	.word	12
	.word	1
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
