.cpool 1
	str "collection parameter is empty", $ERRMSG1

.extern 0

.fdecl 4
	.df fold,3, 4, @fold_defn
	.df sum, 1, 3, @sum_defn
	.df max, 1, 4, @max_defn
	.df min, 1, 4, @min_defn
	# impl any, all
.code

# %0 - Const Ref / Slice, %1 - FRef, %2 - accumulator, %3 - temp
fold_defn:
	fslice %0, %0
	revrs %0
	loop_start:
		branch %0, @loop_end
		pop %0, %3
		stdcall %1, %3, %2, %3
		jmp @loop_start
	loop_end:
		ret %2
.clr

# %0 - Const Ref / Slice
sum_defn:
	ldi 0, %1
	fslice %0, %0
	revrs %0
	loop_start:
		branch %0, @loop_end
		pop %0, %2
		add %1, %2, %1
		jmp @loop_start
	loop_end:
		ret %2
.clr

# %0 - Const Ref/ Slice, %1 - elm, %2 - max, %3 - temp
max_defn:
	fslice %0, %0
	revrs %0
	ldc $ERRMSG1, %1
	assert %0, %1
	pop %0, %2
	loop_start:
		branch %0, @loop_end
		pop %0, %1
		rgt %1, %2, %3
			branch %3, @jmp_stmt
			move %1, %2
		jmp_stmt:
			jmp @loop_start
	loop_end:
		ret %2
.clr

# %0 - Const Ref/ Slice, %1 - elm, %2 - max, %3 - temp
min_defn:
	fslice %0, %0
	revrs %0
	ldc $ERRMSG1, %1
	assert %0, %1
	pop %0, %2
	loop_start:
		branch %0, @loop_end
		pop %0, %1
		rlt %1, %2, %3
			branch %3, @jmp_stmt
			move %1, %2
		jmp_stmt:
			jmp @loop_start
	loop_end:
		ret %2
.clr