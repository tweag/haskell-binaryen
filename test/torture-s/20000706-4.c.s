	.text
	.file	"/b/build/slave/linux/build/src/src/work/gcc/gcc/testsuite/gcc.c-torture/execute/20000706-4.c"
	.section	.text.bar,"ax",@progbits
	.hidden	bar
	.globl	bar
	.type	bar,@function
bar:                                    # @bar
	.param  	i32
# BB#0:                                 # %entry
	block
	i32.const	$push3=, 2
	i32.ne  	$push4=, $0, $pop3
	br_if   	$pop4, 0        # 0: down to label0
# BB#1:                                 # %entry
	i32.const	$push1=, 0
	i32.load	$push2=, c($pop1)
	i32.load	$push0=, 0($pop2)
	i32.const	$push5=, 1
	i32.ne  	$push6=, $pop0, $pop5
	br_if   	$pop6, 0        # 0: down to label0
# BB#2:                                 # %if.end
	return
.LBB0_3:                                # %if.then
	end_block                       # label0:
	call    	abort@FUNCTION
	unreachable
	.endfunc
.Lfunc_end0:
	.size	bar, .Lfunc_end0-bar

	.section	.text.foo,"ax",@progbits
	.hidden	foo
	.globl	foo
	.type	foo,@function
foo:                                    # @foo
	.param  	i32, i32
	.local  	i32, i32, i32, i32, i32
# BB#0:                                 # %entry
	i32.const	$2=, __stack_pointer
	i32.load	$2=, 0($2)
	i32.const	$3=, 16
	i32.sub 	$6=, $2, $3
	i32.const	$3=, __stack_pointer
	i32.store	$6=, 0($3), $6
	i32.const	$push1=, 0
	i32.const	$5=, 12
	i32.add 	$5=, $6, $5
	i32.store	$discard=, c($pop1), $5
	block
	i32.store	$push0=, 12($6), $0
	i32.const	$push2=, 1
	i32.ne  	$push3=, $pop0, $pop2
	br_if   	$pop3, 0        # 0: down to label1
# BB#1:                                 # %entry
	i32.const	$push4=, 2
	i32.ne  	$push5=, $1, $pop4
	br_if   	$pop5, 0        # 0: down to label1
# BB#2:                                 # %bar.exit
	i32.const	$4=, 16
	i32.add 	$6=, $6, $4
	i32.const	$4=, __stack_pointer
	i32.store	$6=, 0($4), $6
	return
.LBB1_3:                                # %if.then.i
	end_block                       # label1:
	call    	abort@FUNCTION
	unreachable
	.endfunc
.Lfunc_end1:
	.size	foo, .Lfunc_end1-foo

	.section	.text.main,"ax",@progbits
	.hidden	main
	.globl	main
	.type	main,@function
main:                                   # @main
	.result 	i32
# BB#0:                                 # %entry
	i32.const	$push1=, 1
	i32.const	$push0=, 2
	call    	foo@FUNCTION, $pop1, $pop0
	i32.const	$push2=, 0
	call    	exit@FUNCTION, $pop2
	unreachable
	.endfunc
.Lfunc_end2:
	.size	main, .Lfunc_end2-main

	.hidden	c                       # @c
	.type	c,@object
	.section	.bss.c,"aw",@nobits
	.globl	c
	.p2align	2
c:
	.int32	0
	.size	c, 4


	.ident	"clang version 3.9.0 "
