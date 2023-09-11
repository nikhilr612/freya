# Test file
.cpool 3
	f64 3.141592653598, $PI
	i64 -1756
	char 槑

.extern 0

.fdecl 2
	.df main, 0, 1, @main_func
	.df x2p1, 1, 1, @sqr_func

.code
main_func:
	ldc $PI, %0x00
	print %0
	ldc 1, %0x00
	print %0
	ldc 2, %0x00
	print %0
	ret
sqr_func:
	# x <- x * x
	mul %0, %0, %0
	# x <- x +1
	inc %0, 1
	ret %0
.clr