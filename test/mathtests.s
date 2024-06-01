.cpool 0

# Integrate 2^x/x from 1 to 2 -> Ei(ln(4)) - Ei(ln(2))
# Compare simpson and rectangular integration.
# Simpson wins!

.extern 2
	.ef test/mathutil:integral_on, @rect_integral_fn
	.ef test/mathutil:simps_integral, @simpson_integ_fn

.fdecl 2
	.df target_fn, 1, 2, @target_fn
	.df main, 0, 5, @main_fn

.code

# %0 - x, %1 - r
target_fn:
	ldi 2, %1
	exp %1, %0, %1
	div %1, %0, %1
	ret %1

# %0 - a, %1 - b, %2 - FRef1, %3 - FRef2, %4 - return val
main_fn:
	ldi 1, %0
	ldi 2, %1
	ldf @target_fn, %2
	ldx @rect_integral_fn, %3
	ldi 100, %4
	stdcall %3, %5, %2, %0, %1, %4
	print %4
	ldx @simpson_integ_fn, %3
	ldi 100, %4
	stdcall %3, %5, %2, %0, %1, %4
	print %4
	ret