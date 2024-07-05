.cpool 2
	str "Hello World!", $STRING1
	str "こんにちは、みんな", $STRING2

.extern 0
.fdecl 1
	.df main, 0, 4, @main

.code

main:
	ldc $STRING1, %0
	# r0 = ConstRef("Hello World!"), print("Hello World!")
	print %0
	ldi -1, %1
	ldi -6, %2
	# r0 = ConstRef("Hello World!"), r1 = -1, r2 = -6, r3 <- r0[r1..r2] = r0[-1..-6]
	slice %3, %0, %1, %2
	print %3
	# Drop the slice, then overwrite reference.
	drop %3
	ldc $STRING2, %0
	len %0, %1
	ldi 0, %2
	slice %3, %0, %1, %2
	# Show unicode support.
	loop_start:
		len %3, %1
		branch %1, @loop_end
		pop %3, %2
		print %2
		jmp @loop_start
	loop_end:
		ret
.clr
