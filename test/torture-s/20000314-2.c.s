	.text
	.file	"/b/build/slave/linux/build/src/src/work/gcc/gcc/testsuite/gcc.c-torture/execute/20000314-2.c"
	.section	.text.main,"ax",@progbits
	.hidden	main
	.globl	main
	.type	main,@function
main:                                   # @main
	.result 	i32
# BB#0:                                 # %entry
	block
	i32.const	$push1=, 0
	i32.load	$push0=, a($pop1)
	br_if   	$pop0, 0        # 0: down to label0
# BB#1:                                 # %if.then
	call    	abort@FUNCTION
	unreachable
.LBB0_2:                                # %if.end
	end_block                       # label0:
	i32.const	$push2=, 0
	call    	exit@FUNCTION, $pop2
	unreachable
	.endfunc
.Lfunc_end0:
	.size	main, .Lfunc_end0-main

	.hidden	bigconst                # @bigconst
	.type	bigconst,@object
	.section	.rodata.bigconst,"a",@progbits
	.globl	bigconst
	.p2align	3
bigconst:
	.int64	17179869184             # 0x400000000
	.size	bigconst, 8

	.hidden	a                       # @a
	.type	a,@object
	.section	.data.a,"aw",@progbits
	.globl	a
	.p2align	2
a:
	.int32	1                       # 0x1
	.size	a, 4


	.ident	"clang version 3.9.0 "
