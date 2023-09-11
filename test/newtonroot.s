.cpool 1
	f64 1e-3, $Delta

.extern 0

# Apply newton's root approximation method to solve numerically, f(x) = 0
.fdecl 3
	.df diff_fn, 2, 4, @diff_fn
	.df target_fn, 1, 2, @target_fn
	.df main, 0, 3, @main

.code

# %0 - FRef object, %1 - location, %2 & %3 - arithmetic registers
# Use symmetric form: diff(f) = [f(x+h) - f(x-h)]/2h
diff_fn:
	ldc $Delta, %2
	add %1, %2, %2
	# r2 = x+h, r2 <- f(r2) = f(x+h)
	stdcall %3, %0, %2
	ldc $Delta, %3
	sub %1, %3, %3
	# r3 = x-h, r3 <- f(r2) = f(x-h)
	stdcall %4, %0, %3
	# r2 = f(x+h), r3 = f(x-h), r2 <- r2 - r3 = f(x+h) - f(x-h)
	sub %2, %3, %2
	ldc $Delta, %3
	add %3, %3, %3
	# r2 = f(x+h)-f(x-h), r3 = 2*h, r2 <- r2 / r3 = [f(x+h)-f(x-h)]/2h
	div %2, %3, %2
	ret %2

# Target function: x*2^x -1
# %0 - x, %1 - arithmetic register
target_fn:
	ldi 2, %1
	exp %1, %0, %1
	# r0 = x, r1 = 2^x, r0 <- r0 * r1  = x*2^x
	mul %0, %1, %0
	ldi 1, %1
	sub %0, %1, %0
	ret %0

main:
	ldi 0, %0
	ldf @target_fn, %1
	ldf @diff_fn, %2
	stdcall %1, %2, %1, %0
	print %0
	ret