	.text
	.file	"/b/build/slave/linux/build/src/src/work/gcc/gcc/testsuite/gcc.c-torture/execute/pr36765.c"
	.section	.text.foo,"ax",@progbits
	.hidden	foo
	.globl	foo
	.type	foo,@function
foo:                                    # @foo
	.param  	i32
	.result 	i32
	.local  	i32
# BB#0:                                 # %entry
	i32.const	$push0=, 16
	i32.call	$push1=, __builtin_malloc@FUNCTION, $pop0
	tee_local	$push8=, $1=, $pop1
	i32.const	$push2=, 0
	i32.store	$discard=, 0($pop8), $pop2
	i32.const	$push3=, 2
	i32.shl 	$push4=, $0, $pop3
	i32.add 	$push5=, $1, $pop4
	i32.const	$push6=, 1
	i32.store	$discard=, 0($pop5), $pop6
	i32.load	$push7=, 0($1)
	return  	$pop7
	.endfunc
.Lfunc_end0:
	.size	foo, .Lfunc_end0-foo

	.section	.text.main,"ax",@progbits
	.hidden	main
	.globl	main
	.type	main,@function
main:                                   # @main
	.result 	i32
# BB#0:                                 # %entry
	block
	i32.const	$push3=, 0
	i32.call	$push0=, foo@FUNCTION, $pop3
	i32.const	$push1=, 1
	i32.ne  	$push2=, $pop0, $pop1
	br_if   	$pop2, 0        # 0: down to label0
# BB#1:                                 # %if.end
	i32.const	$push4=, 0
	return  	$pop4
.LBB1_2:                                # %if.then
	end_block                       # label0:
	call    	abort@FUNCTION
	unreachable
	.endfunc
.Lfunc_end1:
	.size	main, .Lfunc_end1-main


	.ident	"clang version 3.9.0 "
