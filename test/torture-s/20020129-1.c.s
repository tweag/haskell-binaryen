	.text
	.file	"/b/build/slave/linux/build/src/src/work/gcc/gcc/testsuite/gcc.c-torture/execute/20020129-1.c"
	.section	.text.foo,"ax",@progbits
	.hidden	foo
	.globl	foo
	.type	foo,@function
foo:                                    # @foo
	.param  	i32, i32
	.local  	i32
# BB#0:                                 # %entry
	i32.load	$2=, 28($1)
	block
	block
	i32.load	$push0=, 28($0)
	i32.const	$push18=, 0
	i32.eq  	$push19=, $pop0, $pop18
	br_if   	$pop19, 0       # 0: down to label1
# BB#1:                                 # %if.end
	i32.const	$push20=, 0
	i32.eq  	$push21=, $2, $pop20
	br_if   	$pop21, 1       # 1: down to label0
# BB#2:                                 # %if.then6
	call    	abort@FUNCTION
	unreachable
.LBB0_3:                                # %if.then
	end_block                       # label1:
	i32.const	$push1=, 28
	i32.add 	$push2=, $0, $pop1
	i32.store	$discard=, 0($pop2), $2
	i32.const	$push13=, 28
	i32.add 	$push3=, $1, $pop13
	i32.const	$push4=, 0
	i32.store	$discard=, 0($pop3), $pop4
	i32.const	$push22=, 0
	i32.eq  	$push23=, $2, $pop22
	br_if   	$pop23, 0       # 0: down to label0
.LBB0_4:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	loop                            # label2:
	i32.store	$discard=, 4($2), $0
	i32.load	$2=, 0($2)
	br_if   	$2, 0           # 0: up to label2
.LBB0_5:                                # %if.end7
	end_loop                        # label3:
	end_block                       # label0:
	i32.load	$2=, 12($1)
	block
	block
	i32.load	$push5=, 12($0)
	i32.const	$push14=, -1
	i32.eq  	$push6=, $pop5, $pop14
	br_if   	$pop6, 0        # 0: down to label5
# BB#6:                                 # %if.end22
	i32.const	$push17=, -1
	i32.eq  	$push7=, $2, $pop17
	br_if   	$pop7, 1        # 1: down to label4
# BB#7:                                 # %if.then26
	call    	abort@FUNCTION
	unreachable
.LBB0_8:                                # %if.end22.thread
	end_block                       # label5:
	i32.const	$push8=, 12
	i32.add 	$push9=, $0, $pop8
	i32.store	$discard=, 0($pop9), $2
	i32.load	$push10=, 16($1)
	i32.store	$discard=, 16($0), $pop10
	i32.const	$push16=, 12
	i32.add 	$push11=, $1, $pop16
	i32.const	$push15=, -1
	i32.store	$discard=, 0($pop11), $pop15
	i32.const	$push12=, 0
	i32.store	$discard=, 16($1), $pop12
.LBB0_9:                                # %if.end27
	end_block                       # label4:
	return
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
	i32.const	$push1=, 0
	i32.const	$push0=, 6
	i32.store	$discard=, y($pop1), $pop0
	i32.const	$push10=, 0
	i32.const	$push3=, 145
	i32.store	$discard=, y+12($pop10), $pop3
	i32.const	$push9=, 0
	i32.const	$push4=, 2448
	i32.store	$discard=, y+16($pop9), $pop4
	i32.const	$push8=, 0
	i32.const	$push5=, -1
	i32.store	$discard=, x+12($pop8), $pop5
	i32.const	$push6=, x
	i32.const	$push2=, y
	call    	foo@FUNCTION, $pop6, $pop2
	i32.const	$push7=, 0
	call    	exit@FUNCTION, $pop7
	unreachable
	.endfunc
.Lfunc_end1:
	.size	main, .Lfunc_end1-main

	.hidden	y                       # @y
	.type	y,@object
	.section	.bss.y,"aw",@nobits
	.globl	y
	.p2align	2
y:
	.skip	32
	.size	y, 32

	.hidden	x                       # @x
	.type	x,@object
	.section	.bss.x,"aw",@nobits
	.globl	x
	.p2align	2
x:
	.skip	32
	.size	x, 32


	.ident	"clang version 3.9.0 "
