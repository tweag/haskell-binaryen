	.text
	.file	"/b/build/slave/linux/build/src/src/work/gcc/gcc/testsuite/gcc.c-torture/execute/pr33382.c"
	.section	.text.foo,"ax",@progbits
	.hidden	foo
	.globl	foo
	.type	foo,@function
foo:                                    # @foo
	.result 	i32
	.local  	i32
# BB#0:                                 # %entry
	i32.const	$push0=, 0
	i32.load	$0=, x+8($pop0)
	i32.const	$push2=, 0
	i32.const	$push1=, 1
	i32.store	$discard=, x+4($pop2), $pop1
	return  	$0
	.endfunc
.Lfunc_end0:
	.size	foo, .Lfunc_end0-foo

	.section	.text.main,"ax",@progbits
	.hidden	main
	.globl	main
	.type	main,@function
main:                                   # @main
	.result 	i32
	.local  	i32
# BB#0:                                 # %entry
	i32.const	$push2=, 0
	i32.load	$0=, x+8($pop2)
	i32.const	$push1=, 0
	i32.const	$push0=, 1
	i32.store	$discard=, x+4($pop1), $pop0
	block
	br_if   	$0, 0           # 0: down to label0
# BB#1:                                 # %if.end
	i32.const	$push3=, 0
	return  	$pop3
.LBB1_2:                                # %if.then
	end_block                       # label0:
	call    	abort@FUNCTION
	unreachable
	.endfunc
.Lfunc_end1:
	.size	main, .Lfunc_end1-main

	.hidden	x                       # @x
	.type	x,@object
	.section	.data.x,"aw",@progbits
	.globl	x
	.p2align	2
x:
	.int32	1                       # 0x1
	.int32	2                       # 0x2
	.int32	0                       # 0x0
	.int32	2                       # 0x2
	.int32	3                       # 0x3
	.size	x, 20


	.ident	"clang version 3.9.0 "
