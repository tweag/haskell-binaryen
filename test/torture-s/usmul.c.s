	.text
	.file	"/b/build/slave/linux/build/src/src/work/gcc/gcc/testsuite/gcc.c-torture/execute/usmul.c"
	.section	.text.foo,"ax",@progbits
	.hidden	foo
	.globl	foo
	.type	foo,@function
foo:                                    # @foo
	.param  	i32, i32
	.result 	i32
# BB#0:                                 # %entry
	i32.mul 	$push0=, $1, $0
	return  	$pop0
	.endfunc
.Lfunc_end0:
	.size	foo, .Lfunc_end0-foo

	.section	.text.bar,"ax",@progbits
	.hidden	bar
	.globl	bar
	.type	bar,@function
bar:                                    # @bar
	.param  	i32, i32
	.result 	i32
# BB#0:                                 # %entry
	i32.mul 	$push0=, $1, $0
	return  	$pop0
	.endfunc
.Lfunc_end1:
	.size	bar, .Lfunc_end1-bar

	.section	.text.main,"ax",@progbits
	.hidden	main
	.globl	main
	.type	main,@function
main:                                   # @main
	.result 	i32
# BB#0:                                 # %entry
	block
	i32.const	$push0=, -2
	i32.const	$push33=, 65535
	i32.call	$push1=, foo@FUNCTION, $pop0, $pop33
	i32.const	$push2=, -131070
	i32.ne  	$push3=, $pop1, $pop2
	br_if   	$pop3, 0        # 0: down to label0
# BB#1:                                 # %if.end
	block
	i32.const	$push4=, 2
	i32.const	$push34=, 65535
	i32.call	$push5=, foo@FUNCTION, $pop4, $pop34
	i32.const	$push6=, 131070
	i32.ne  	$push7=, $pop5, $pop6
	br_if   	$pop7, 0        # 0: down to label1
# BB#2:                                 # %if.end4
	block
	i32.const	$push8=, -32768
	i32.const	$push35=, 32768
	i32.call	$push9=, foo@FUNCTION, $pop8, $pop35
	i32.const	$push10=, -1073741824
	i32.ne  	$push11=, $pop9, $pop10
	br_if   	$pop11, 0       # 0: down to label2
# BB#3:                                 # %if.end8
	block
	i32.const	$push12=, 32767
	i32.const	$push36=, 32768
	i32.call	$push13=, foo@FUNCTION, $pop12, $pop36
	i32.const	$push14=, 1073709056
	i32.ne  	$push15=, $pop13, $pop14
	br_if   	$pop15, 0       # 0: down to label3
# BB#4:                                 # %if.end12
	block
	i32.const	$push37=, 65535
	i32.const	$push16=, -2
	i32.call	$push17=, bar@FUNCTION, $pop37, $pop16
	i32.const	$push18=, -131070
	i32.ne  	$push19=, $pop17, $pop18
	br_if   	$pop19, 0       # 0: down to label4
# BB#5:                                 # %if.end16
	block
	i32.const	$push38=, 65535
	i32.const	$push20=, 2
	i32.call	$push21=, bar@FUNCTION, $pop38, $pop20
	i32.const	$push22=, 131070
	i32.ne  	$push23=, $pop21, $pop22
	br_if   	$pop23, 0       # 0: down to label5
# BB#6:                                 # %if.end20
	block
	i32.const	$push39=, 32768
	i32.const	$push24=, -32768
	i32.call	$push25=, bar@FUNCTION, $pop39, $pop24
	i32.const	$push26=, -1073741824
	i32.ne  	$push27=, $pop25, $pop26
	br_if   	$pop27, 0       # 0: down to label6
# BB#7:                                 # %if.end24
	block
	i32.const	$push40=, 32768
	i32.const	$push28=, 32767
	i32.call	$push29=, bar@FUNCTION, $pop40, $pop28
	i32.const	$push30=, 1073709056
	i32.ne  	$push31=, $pop29, $pop30
	br_if   	$pop31, 0       # 0: down to label7
# BB#8:                                 # %if.end28
	i32.const	$push32=, 0
	call    	exit@FUNCTION, $pop32
	unreachable
.LBB2_9:                                # %if.then27
	end_block                       # label7:
	call    	abort@FUNCTION
	unreachable
.LBB2_10:                               # %if.then23
	end_block                       # label6:
	call    	abort@FUNCTION
	unreachable
.LBB2_11:                               # %if.then19
	end_block                       # label5:
	call    	abort@FUNCTION
	unreachable
.LBB2_12:                               # %if.then15
	end_block                       # label4:
	call    	abort@FUNCTION
	unreachable
.LBB2_13:                               # %if.then11
	end_block                       # label3:
	call    	abort@FUNCTION
	unreachable
.LBB2_14:                               # %if.then7
	end_block                       # label2:
	call    	abort@FUNCTION
	unreachable
.LBB2_15:                               # %if.then3
	end_block                       # label1:
	call    	abort@FUNCTION
	unreachable
.LBB2_16:                               # %if.then
	end_block                       # label0:
	call    	abort@FUNCTION
	unreachable
	.endfunc
.Lfunc_end2:
	.size	main, .Lfunc_end2-main


	.ident	"clang version 3.9.0 "
