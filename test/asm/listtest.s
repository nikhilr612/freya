# Create a list of partial sums to ln(2).
# Perform tests on them.
.cpool 1
	i64 1000, $N

.extern 0
.fdecl 3
	.df populate_list, 1, 6, @func1_def
	.df main, 0, 3, @main
	.df calc_err, 1, 3, @calc_err

.code
# %0 - N, %1 - FRef
main:
	ldc $N, %0
	ldf @func1_def, %1
	stdcall %1, %1, %0
	ldf @calc_err, %1
	# Borrow, to prevent premature dealloc
	&imt %0, %2
	stdcall %1, %0, %2
	# -----------> accidental tail-call happens here, avoid with nop
	nop
	ret

# %0 - ConstRef / last value, %1 - slice, %2 - temp register
calc_err:
	fslice %0, %1
	print %0
	pop %1, %0
	revrs %1
	loop_start:
		branch %1, @loop_end
		pop %1, %2
		sub %2, %0, %2
		print %2
		jmp @loop_start
	loop_end:
		ret
.clr

# %0 - N, %1 - List, %2 - accumulator, %3 - sign, %4 - counter, %5 - temp
func1_def:
	newls %1
	ldi 0, %2
	ldi 1, %3
	ldi 1, %4
	loop_start:
		rle %4, %0, %5
		branch %5, @loop_end
		div %3, %4, %5
		add %2, %5, %2
		push %1, %2
		ldi -1, %5
		mul %3, %5, %3
		inc %4, 1
		jmp @loop_start
	loop_end:
		ret %1
.clr