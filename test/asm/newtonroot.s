.cpool 1
	f64 0.6, $StartVal

.extern 1
	.ef test/mathutil:derivative_at, @diff_fn

# Apply newton's root approximation method to solve numerically, f(x) = 0
.fdecl 3
	.df target_fn, 1, 2, @target_fn
	.df main, 0, 4, @main
	.df findroot, 3, 5, @findroot
.code

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

# %0 - Fref, %1 - n, %2 - x_n, %3 & %4 - temporary registers
findroot:
	# If the required number of iterations are complete, then return.
	branch %1, @end_algo
		ldx @diff_fn, %3
		# r4 <- r3 (r0, r2) = diff_fn(f, x_n) = f'(x_n)
		stdcall %3, %5, %0, %2
		# if f'(x_n) == 0, then we cannot apply algorithm.
		assert %4, %0xff
		# r3 = diff_fn, r3 <- r0 (r2) = f(x_n)
		stdcall %0, %4, %2
		# if f(x_n) == 0, then return
		branch %3, @end_algo
		div %3, %4, %3
		sub %2, %3, %2
		inc %1, -1
		ldf @findroot, %3
		# r2 <- r3 (r0, r1, r2) = findroot(f, n-1, x_{n+1})
		stdcall %3, %3, %0, %1, %2
	end_algo:
		ret %2
.clr

main:
	ldi 10, %0
	ldc $StartVal, %3
	ldf @target_fn, %1
	ldf @findroot, %2
	stdcall %2, %1, %1, %0, %3
	print %0
	ret
.clr