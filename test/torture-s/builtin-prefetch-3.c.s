	.text
	.file	"/b/build/slave/linux/build/src/src/work/gcc/gcc/testsuite/gcc.c-torture/execute/builtin-prefetch-3.c"
	.section	.text.simple_vol_global,"ax",@progbits
	.hidden	simple_vol_global
	.globl	simple_vol_global
	.type	simple_vol_global,@function
simple_vol_global:                      # @simple_vol_global
# BB#0:                                 # %entry
	i32.const	$push0=, 0
	i32.load	$discard=, glob_vol_ptr_int($pop0)
	i32.const	$push1=, 0
	i32.load	$discard=, glob_vol_ptr_vol_int($pop1)
	return
	.endfunc
.Lfunc_end0:
	.size	simple_vol_global, .Lfunc_end0-simple_vol_global

	.section	.text.simple_vol_file,"ax",@progbits
	.hidden	simple_vol_file
	.globl	simple_vol_file
	.type	simple_vol_file,@function
simple_vol_file:                        # @simple_vol_file
# BB#0:                                 # %entry
	i32.const	$push0=, 0
	i32.load	$discard=, stat_vol_ptr_int($pop0)
	i32.const	$push1=, 0
	i32.load	$discard=, stat_vol_ptr_vol_int($pop1)
	return
	.endfunc
.Lfunc_end1:
	.size	simple_vol_file, .Lfunc_end1-simple_vol_file

	.section	.text.expr_vol_global,"ax",@progbits
	.hidden	expr_vol_global
	.globl	expr_vol_global
	.type	expr_vol_global,@function
expr_vol_global:                        # @expr_vol_global
# BB#0:                                 # %entry
	i32.const	$push0=, 0
	i32.load	$discard=, vol_ptr_str($pop0)
	i32.const	$push29=, 0
	i32.load	$discard=, vol_ptr_vol_str($pop29)
	i32.const	$push28=, 0
	i32.load	$discard=, vol_ptr_str($pop28)
	i32.const	$push27=, 0
	i32.load	$discard=, vol_ptr_vol_str($pop27)
	i32.const	$push26=, 0
	i32.load	$discard=, vol_ptr_str($pop26)
	i32.const	$push25=, 0
	i32.load	$discard=, vol_ptr_vol_str($pop25)
	i32.const	$push24=, 0
	i32.load	$discard=, vol_str+16($pop24)
	i32.const	$push23=, 0
	i32.load	$discard=, vol_ptr_str($pop23)
	i32.const	$push22=, 0
	i32.load	$push1=, ptr_vol_str($pop22)
	i32.load	$discard=, 16($pop1)
	i32.const	$push21=, 0
	i32.load	$push2=, vol_ptr_vol_str($pop21)
	i32.load	$discard=, 16($pop2)
	i32.const	$push20=, 0
	i32.load	$discard=, vol_str+16($pop20)
	i32.const	$push19=, 0
	i32.load	$discard=, vol_ptr_str($pop19)
	i32.const	$push18=, 0
	i32.load	$push3=, ptr_vol_str($pop18)
	i32.load	$discard=, 16($pop3)
	i32.const	$push17=, 0
	i32.load	$push4=, vol_ptr_vol_str($pop17)
	i32.load	$discard=, 16($pop4)
	i32.const	$push16=, 0
	i32.load	$discard=, glob_vol_ptr_int($pop16)
	i32.const	$push15=, 0
	i32.load	$discard=, glob_vol_ptr_vol_int($pop15)
	i32.const	$push14=, 0
	i32.load	$discard=, glob_vol_ptr_int($pop14)
	i32.const	$push13=, 0
	i32.load	$discard=, glob_vol_ptr_vol_int($pop13)
	i32.const	$push12=, 0
	i32.load	$discard=, glob_vol_int($pop12)
	i32.const	$push11=, 0
	i32.load	$discard=, glob_vol_ptr_int($pop11)
	i32.const	$push10=, 0
	i32.load	$discard=, glob_vol_ptr_vol_int($pop10)
	i32.const	$push9=, 0
	i32.load	$discard=, glob_vol_ptr_int($pop9)
	i32.const	$push8=, 0
	i32.load	$discard=, glob_vol_int($pop8)
	i32.const	$push7=, 0
	i32.load	$discard=, glob_vol_int($pop7)
	i32.const	$push6=, 0
	i32.load	$discard=, glob_vol_ptr_vol_int($pop6)
	i32.const	$push5=, 0
	i32.load	$discard=, glob_vol_int($pop5)
	return
	.endfunc
.Lfunc_end2:
	.size	expr_vol_global, .Lfunc_end2-expr_vol_global

	.section	.text.main,"ax",@progbits
	.hidden	main
	.globl	main
	.type	main,@function
main:                                   # @main
	.result 	i32
# BB#0:                                 # %entry
	call    	simple_vol_global@FUNCTION
	call    	simple_vol_file@FUNCTION
	i32.const	$push1=, 0
	i32.const	$push4=, 0
	i32.const	$push0=, str
	i32.store	$push2=, str+16($pop4), $pop0
	i32.store	$discard=, vol_str+16($pop1), $pop2
	call    	expr_vol_global@FUNCTION
	i32.const	$push3=, 0
	call    	exit@FUNCTION, $pop3
	unreachable
	.endfunc
.Lfunc_end3:
	.size	main, .Lfunc_end3-main

	.hidden	glob_int                # @glob_int
	.type	glob_int,@object
	.section	.data.glob_int,"aw",@progbits
	.globl	glob_int
	.p2align	2
glob_int:
	.int32	4                       # 0x4
	.size	glob_int, 4

	.hidden	glob_int_arr            # @glob_int_arr
	.type	glob_int_arr,@object
	.section	.bss.glob_int_arr,"aw",@nobits
	.globl	glob_int_arr
	.p2align	4
glob_int_arr:
	.skip	400
	.size	glob_int_arr, 400

	.hidden	glob_vol_ptr_int        # @glob_vol_ptr_int
	.type	glob_vol_ptr_int,@object
	.section	.data.glob_vol_ptr_int,"aw",@progbits
	.globl	glob_vol_ptr_int
	.p2align	2
glob_vol_ptr_int:
	.int32	glob_int_arr
	.size	glob_vol_ptr_int, 4

	.hidden	glob_vol_int_arr        # @glob_vol_int_arr
	.type	glob_vol_int_arr,@object
	.section	.bss.glob_vol_int_arr,"aw",@nobits
	.globl	glob_vol_int_arr
	.p2align	4
glob_vol_int_arr:
	.skip	400
	.size	glob_vol_int_arr, 400

	.hidden	glob_ptr_vol_int        # @glob_ptr_vol_int
	.type	glob_ptr_vol_int,@object
	.section	.data.glob_ptr_vol_int,"aw",@progbits
	.globl	glob_ptr_vol_int
	.p2align	2
glob_ptr_vol_int:
	.int32	glob_vol_int_arr
	.size	glob_ptr_vol_int, 4

	.hidden	glob_vol_ptr_vol_int    # @glob_vol_ptr_vol_int
	.type	glob_vol_ptr_vol_int,@object
	.section	.data.glob_vol_ptr_vol_int,"aw",@progbits
	.globl	glob_vol_ptr_vol_int
	.p2align	2
glob_vol_ptr_vol_int:
	.int32	glob_vol_int_arr
	.size	glob_vol_ptr_vol_int, 4

	.hidden	str                     # @str
	.type	str,@object
	.section	.bss.str,"aw",@nobits
	.globl	str
	.p2align	2
str:
	.skip	20
	.size	str, 20

	.hidden	vol_ptr_str             # @vol_ptr_str
	.type	vol_ptr_str,@object
	.section	.data.vol_ptr_str,"aw",@progbits
	.globl	vol_ptr_str
	.p2align	2
vol_ptr_str:
	.int32	str
	.size	vol_ptr_str, 4

	.hidden	vol_str                 # @vol_str
	.type	vol_str,@object
	.section	.bss.vol_str,"aw",@nobits
	.globl	vol_str
	.p2align	2
vol_str:
	.skip	20
	.size	vol_str, 20

	.hidden	ptr_vol_str             # @ptr_vol_str
	.type	ptr_vol_str,@object
	.section	.data.ptr_vol_str,"aw",@progbits
	.globl	ptr_vol_str
	.p2align	2
ptr_vol_str:
	.int32	vol_str
	.size	ptr_vol_str, 4

	.hidden	vol_ptr_vol_str         # @vol_ptr_vol_str
	.type	vol_ptr_vol_str,@object
	.section	.data.vol_ptr_vol_str,"aw",@progbits
	.globl	vol_ptr_vol_str
	.p2align	2
vol_ptr_vol_str:
	.int32	vol_str
	.size	vol_ptr_vol_str, 4

	.hidden	glob_vol_int            # @glob_vol_int
	.type	glob_vol_int,@object
	.section	.bss.glob_vol_int,"aw",@nobits
	.globl	glob_vol_int
	.p2align	2
glob_vol_int:
	.int32	0                       # 0x0
	.size	glob_vol_int, 4

	.type	stat_vol_int_arr,@object # @stat_vol_int_arr
	.lcomm	stat_vol_int_arr,400,4
	.type	stat_vol_ptr_int,@object # @stat_vol_ptr_int
	.section	.data.stat_vol_ptr_int,"aw",@progbits
	.p2align	2
stat_vol_ptr_int:
	.int32	stat_int_arr
	.size	stat_vol_ptr_int, 4

	.type	stat_vol_ptr_vol_int,@object # @stat_vol_ptr_vol_int
	.section	.data.stat_vol_ptr_vol_int,"aw",@progbits
	.p2align	2
stat_vol_ptr_vol_int:
	.int32	stat_vol_int_arr
	.size	stat_vol_ptr_vol_int, 4

	.type	stat_vol_int,@object    # @stat_vol_int
	.lcomm	stat_vol_int,4,2
	.type	stat_int_arr,@object    # @stat_int_arr
	.lcomm	stat_int_arr,400,4

	.ident	"clang version 3.9.0 "
