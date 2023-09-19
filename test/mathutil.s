.cpool 1
	f64 1e-3, $Delta

.extern 0

.fdecl 2
	.df diff_fn, 2, 4, @diff_fn
	.df intg_fn, 3, 6, @intg_fn 

.code

# %0 - FRef object, %1 - location, %2 & %3 - arithmetic registers
# Use symmetric form: diff(f) = [f(x+h) - f(x-h)]/2h
diff_fn:
	ldc $Delta, %2
	add %1, %2, %2
	# r2 = x+h, r2 <- f(r2) = f(x+h)
	stdcall %0, %3, %2
	ldc $Delta, %3
	sub %1, %3, %3
	# r3 = x-h, r3 <- f(r2) = f(x-h)
	stdcall %0, %4, %3
	# r2 = f(x+h), r3 = f(x-h), r2 <- r2 - r3 = f(x+h) - f(x-h)
	sub %2, %3, %2
	ldc $Delta, %3
	add %3, %3, %3
	# r2 = f(x+h)-f(x-h), r3 = 2*h, r2 <- r2 / r3 = [f(x+h)-f(x-h)]/2h
	div %2, %3, %2
	ret %2

# %0 - FRef object, %1 - a, %2 - b, %3 - n, %4 - Delta, %5 - sum
intg_fn:
	assert %3, %0xff
	# r4 <- r0(r1) = f(a)
	stdcall %0, %5, %1
	# r5 <- r0(r2) = f(b)
	stdcall %0, %6, %2
	# r5 = f(b) + f(a)
	add %5, %4, %5
	ldi 2, %4
	# r5 = [f(a) + f(b)] / 2
	div %5, %4, %5
	sub %2, %1, %4
	# r4 = (b-a)/n
	div %4, %3, %4
	# r3 = n-1
	inc %3, -2
	loop_start:
		# r2 = 0
		ldi 0, %2
		rgt %3, %2, %2
		branch %2, @loop_end
		# r1 <- r1 + r4 = a + dx
		add %1, %4, %1
		# r2 <- r0(r1) = f(a + dx)
		stdcall %0, %3, %1
		# r5 <- r2 + r5, sum += f(a + dx)
		add %2, %5, %5
		inc %3, -1
		jmp @loop_start
	loop_end:
		mul %5, %4, %5
		ret %5
.clr