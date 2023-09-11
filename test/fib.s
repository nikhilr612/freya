# Calculate nth fibonacci number
.cpool 1
	i64 10, $N

.extern 0

.fdecl 2
	# Find the nth fibonacci number
	.df fibn, 1, 3, @fibn_def
	.df main, 0, 2, @main_def

.code

# %0 - n, %1 - f_cur, %2 - f_prev
fibn_def:
	ldi 0, %1
	ldi 1, %2
	loop_start:
		branch %0, @end_loop
		swap %1, %2
		add %1, %2, %1
		inc %0, -1
		jmp @loop_start
	end_loop:
	ret %1
.clr

main_def:
	ldc $N, %0
	ldf @fibn_def, %1
	stdcall %1, %1, %0
	print %0
	ret
.clr