.cstring
.text
.globl _main
_main:
	pushq	%rbp
	movq	%rsp, %rbp
subq $16, %rsp
movl $3, %eax
movl %eax, -4(%rbp)
movl $2, %eax
movl %eax, -8(%rbp)
movl $1, %eax
movl %eax, -12(%rbp)
movl -4(%rbp), %edi
movl -8(%rbp), %esi
movl -12(%rbp), %edx
call _add
movl %eax, %edi
call _printInt
	leave
	ret
.cstring
.text
.globl _add
_add:
	pushq	%rbp
	movq	%rsp, %rbp
subq $32, %rsp
movl %edx, %eax
movl %eax, -4(%rbp)
movl %esi, %eax
movl %eax, -8(%rbp)
movl %edi, %eax
movl %eax, -12(%rbp)
movl -8(%rbp), %eax
addl -4(%rbp), %eax
movl %eax, -16(%rbp)
movl -12(%rbp), %eax
addl -16(%rbp), %eax
movl %eax, -20(%rbp)
	leave
	ret
