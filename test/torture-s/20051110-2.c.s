	.text
	.file	"/b/build/slave/linux/build/src/src/work/gcc/gcc/testsuite/gcc.c-torture/execute/20051110-2.c"
	.section	.text.add_unwind_adjustsp,"ax",@progbits
	.hidden	add_unwind_adjustsp
	.globl	add_unwind_adjustsp
	.type	add_unwind_adjustsp,@function
add_unwind_adjustsp:                    # @add_unwind_adjustsp
	.param  	i32
	.local  	i32, i32, i32
# BB#0:                                 # %entry
	i32.const	$push14=, 0
	i32.const	$push1=, -516
	i32.add 	$push2=, $0, $pop1
	i32.const	$push3=, 2
	i32.shr_s	$push9=, $pop2, $pop3
	tee_local	$push13=, $3=, $pop9
	i32.const	$push12=, 127
	i32.and 	$push4=, $pop13, $pop12
	i32.store8	$discard=, bytes($pop14), $pop4
	block
	i32.const	$push11=, 7
	i32.shr_u	$push8=, $3, $pop11
	tee_local	$push10=, $2=, $pop8
	i32.const	$push21=, 0
	i32.eq  	$push22=, $pop10, $pop21
	br_if   	$pop22, 0       # 0: down to label0
# BB#1:                                 # %if.then.lr.ph
	i32.const	$push16=, 0
	i32.load	$push5=, flag($pop16)
	i32.const	$push15=, 0
	i32.eq  	$1=, $pop5, $pop15
	i32.const	$0=, bytes
.LBB0_2:                                # %if.then
                                        # =>This Inner Loop Header: Depth=1
	loop                            # label1:
	i32.const	$push20=, 128
	i32.or  	$push6=, $3, $pop20
	i32.store8	$discard=, 0($0), $pop6
	i32.add 	$0=, $0, $1
	copy_local	$push0=, $2
	tee_local	$push19=, $2=, $pop0
	i32.const	$push18=, 127
	i32.and 	$push7=, $pop19, $pop18
	i32.store8	$discard=, 0($0), $pop7
	copy_local	$3=, $2
	i32.const	$push17=, 7
	i32.shr_u	$2=, $2, $pop17
	br_if   	$2, 0           # 0: up to label1
.LBB0_3:                                # %do.end
	end_loop                        # label2:
	end_block                       # label0:
	return
	.endfunc
.Lfunc_end0:
	.size	add_unwind_adjustsp, .Lfunc_end0-add_unwind_adjustsp

	.section	.text.main,"ax",@progbits
	.hidden	main
	.globl	main
	.type	main,@function
main:                                   # @main
	.result 	i32
	.local  	i32, i32
# BB#0:                                 # %add_unwind_adjustsp.exit
	i32.const	$push13=, 0
	i32.load	$0=, flag($pop13)
	i32.const	$push12=, 0
	i32.const	$push2=, 136
	i32.store8	$1=, bytes($pop12), $pop2
	i32.const	$push11=, 0
	i32.eq  	$push1=, $0, $pop11
	i32.const	$push3=, 7
	i32.store8	$0=, bytes($pop1), $pop3
	block
	i32.const	$push10=, 0
	i32.load8_u	$push4=, bytes($pop10)
	i32.ne  	$push5=, $1, $pop4
	br_if   	$pop5, 0        # 0: down to label3
# BB#1:                                 # %add_unwind_adjustsp.exit
	i32.const	$push14=, 0
	i32.load8_u	$push0=, bytes+1($pop14)
	i32.const	$push6=, 255
	i32.and 	$push7=, $pop0, $pop6
	i32.ne  	$push8=, $pop7, $0
	br_if   	$pop8, 0        # 0: down to label3
# BB#2:                                 # %if.end
	i32.const	$push9=, 0
	return  	$pop9
.LBB1_3:                                # %if.then
	end_block                       # label3:
	call    	abort@FUNCTION
	unreachable
	.endfunc
.Lfunc_end1:
	.size	main, .Lfunc_end1-main

	.hidden	bytes                   # @bytes
	.type	bytes,@object
	.section	.bss.bytes,"aw",@nobits
	.globl	bytes
bytes:
	.skip	5
	.size	bytes, 5

	.hidden	flag                    # @flag
	.type	flag,@object
	.section	.bss.flag,"aw",@nobits
	.globl	flag
	.p2align	2
flag:
	.int32	0                       # 0x0
	.size	flag, 4


	.ident	"clang version 3.9.0 "
