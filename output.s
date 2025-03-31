	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	_main                           ; -- Begin function main
	.p2align	2
_main:                                  ; @main
	.cfi_startproc
; %bb.0:                                ; %entry
	sub	sp, sp, #32
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	.cfi_def_cfa_offset 32
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	mov	w8, #5                          ; =0x5
	mov	w9, #10                         ; =0xa
	mov	w0, #53                         ; =0x35
	stp	w8, w9, [sp, #8]
	bl	_putchar
	mov	w0, #10                         ; =0xa
	bl	_putchar
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	mov	w0, wzr
	add	sp, sp, #32
	ret
	.cfi_endproc
                                        ; -- End function
.subsections_via_symbols
