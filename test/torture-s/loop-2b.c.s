	.text
	.file	"/b/build/slave/linux/build/src/src/work/gcc/gcc/testsuite/gcc.c-torture/execute/loop-2b.c"
	.section	.text.f,"ax",@progbits
	.hidden	f
	.globl	f
	.type	f,@function
f:                                      # @f
	.param  	i32
	.result 	i32
	.local  	i32, i32
# BB#0:                                 # %entry
	block
	i32.const	$push1=, 2147483647
	i32.eq  	$push2=, $0, $pop1
	br_if   	$pop2, 0        # 0: down to label0
# BB#1:                                 # %for.body.preheader
	i32.const	$push3=, 2
	i32.shl 	$push4=, $0, $pop3
	i32.const	$push5=, a
	i32.add 	$2=, $pop4, $pop5
	i32.const	$push6=, 2147483646
	i32.sub 	$1=, $pop6, $0
.LBB0_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	loop                            # label1:
	i32.const	$push10=, -2
	i32.store	$discard=, 0($2), $pop10
	copy_local	$push0=, $1
	tee_local	$push9=, $0=, $pop0
	i32.const	$push8=, 2147483645
	i32.eq  	$push7=, $pop9, $pop8
	br_if   	$pop7, 1        # 1: down to label2
# BB#3:                                 # %for.body
                                        #   in Loop: Header=BB0_2 Depth=1
	i32.const	$push12=, 4
	i32.add 	$2=, $2, $pop12
	i32.const	$push11=, -1
	i32.add 	$1=, $0, $pop11
	br_if   	$0, 0           # 0: up to label1
.LBB0_4:                                # %for.end
	end_loop                        # label2:
	end_block                       # label0:
	return  	$2
	.endfunc
.Lfunc_end0:
	.size	f, .Lfunc_end0-f

	.section	.text.main,"ax",@progbits
	.hidden	main
	.globl	main
	.type	main,@function
main:                                   # @main
	.result 	i32
# BB#0:                                 # %if.end
	i32.const	$push1=, 0
	i64.const	$push0=, -4294967298
	i64.store	$discard=, a($pop1):p2align=2, $pop0
	i32.const	$push2=, 0
	call    	exit@FUNCTION, $pop2
	unreachable
	.endfunc
.Lfunc_end1:
	.size	main, .Lfunc_end1-main

	.hidden	a                       # @a
	.type	a,@object
	.section	.bss.a,"aw",@nobits
	.globl	a
	.p2align	2
a:
	.skip	8
	.size	a, 8


	.ident	"clang version 3.9.0 "
