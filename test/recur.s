.cpool 0
.extern 0

.fdecl 3
	.df main, 0, 2, @main_def
	.df fact, 1, 3, @fact_def
	.df _fact_rec, 2, 3, @fact_rec_def

.code
# %0 - N (counter), %1 - product
# If N=0, return product
fact_rec_def:
	branch %0, @term_pt
		# P <- N*P
		mul %0, %1, %1
		inc %0, -1
		ldf @fact_rec_def, %2
		stdcall %2, %3, %0, %1
		ret %2
	term_pt:
	ret %1

fact_def:
	ldi 1, %1
	ldf @fact_rec_def, %2
	stdcall %2, %3, %0, %1
	ret %2

main_def:
	ldi 6, %0
	ldf @fact_def, %1
	stdcall %1, %2, %0
	print %1
	ret
.clr