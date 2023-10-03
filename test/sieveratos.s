# Find all primes under 8 million using Eratosthenes' Sieve
.cpool 1
	i64 8000000, $N
.extern 0
.fdecl 2
	.df main, 0, 4, @main_def
	.df sieve, 2, 6, @sieve_def

.code

main_def:
	ldc $N, %0
	newbs %0, %0
	newls %1
	&mut %1, %2
	ldf @sieve_def, %3
	stdcall %3, %0, %0, %2
	print %1
	ret
.clr

# %0 - bitset, %1 - list, %2 - index, %3 - length, %4 - bool, %5 - other index
sieve_def:
	ldi 2, %2
	len %0, %3
	start_loop:
		rlt %2, %3, %4
		branch %4, @end_loop
		get %0, %2, %4
		lnot %4
		branch %4, @skip
			push %1, %2
			move %2, %5
			mul %5, %5, %5
			inner_loop:
				rlt %5, %3, %4
				branch %4, @skip
				put %0, %5, %4
				add %5, %2, %5
				jmp @inner_loop
		skip:
			inc %2, 1
		jmp @start_loop
	end_loop:
		ret
.clr