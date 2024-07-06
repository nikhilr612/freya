.cpool 1
	str "Greetings! It seems I've got some arguments.", $MSG

.extern 0
.fdecl 1
	.df main, 1, 2, @main_def

.code
main_def:
	ldc $MSG, %1
	print %1
	print %0
	ret
.clr