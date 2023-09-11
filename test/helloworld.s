.cpool 2
str "Hello, World!", $MSG
f64 8.24621125, $F1

.extern 2
	.ef test/piout:main
	.ef test/piout:x2p1

.fdecl 1
	.df main, 0, 2, @main_def

main_def:
	ldc $MSG, %0x00
	print %0x00
	ldx 0, %0x00
	# Call without return
	stdcall %0x00, %0
	ldx 1, %0x00
	ldc $F1, %0x01
	stdcall %0x00, %2, %0x01
	print %0x01
	ldx 1, %0x00
	ldc $F1, %0x01
	stdcall %0x00, %2, %0x01
	print %0x01
	ret
.clr