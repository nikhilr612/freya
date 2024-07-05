# Naively find collatz sequences for first N natural numbers
.cpool 1
	i64 1000, $N

.extern 0

.fdecl 2
	.df collatz, 2, 3, @collatz_def
	.df main, 0, 5, @main_def

.code

# %0 - n, %1 - list, %2 - temp
collatz_def:
	push %1, %0
	ldi 1, %2
	# r2 <- (r0 == r2) :: n == 1
	req %0, %2, %2
	branch %2, @if_else
	# just return when 1 is reached
		ret
	if_else:
		ldi 2, %2
		rem %0, %2, %2
		branch %2, @even_case
			ldi 3, %2
			mul %0, %2, %2
			inc %2, 1
			ldf @collatz_def, %0
			stdcall %0, %0, %2, %1
			ret
		even_case:
			ldi 2, %2
			idv %0, %2, %2
			ldf @collatz_def, %0
			stdcall %0, %0, %2, %1
			ret
.clr

main_def:
	ldi 1, %0
	ldc $N, %1
	start_loop:
		rle %0, %1, %2
		branch %2, @end_loop
		newls %2
		&mut %2, %3
		ldf @collatz_def, %4
		stdcall %4, %0, %0, %3
		print %2
		# drop is needed, since %2 is written to before %3
		drop %3
		inc %0, 1
		jmp @start_loop
	end_loop:
		ret
.clr