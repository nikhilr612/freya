.cpool 1
	str "Hello, World!", $MSG
	
.extern 0
.fdecl 1
	.df main, 0, 1, @main_def
	
.code
main_def:
	ldc $MSG, %0x00
	print %0x00
	ret
.clr