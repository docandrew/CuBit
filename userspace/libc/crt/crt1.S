/*
 * CuBit libc program entry. The loader starts _start with the stack pointer
 * at the top of an empty stack and RDI = the launch block's length; the
 * start itself is Ada (CuBit.Libc_Start, userspace/libc/ada).
 */
	.bss
	.balign 8
	.global __cubit_launch_block_length
__cubit_launch_block_length: .quad 0
	.text
	.global _start
	.type _start,@function
_start:
	xor %ebp,%ebp
	movq    %rdi, __cubit_launch_block_length(%rip)
	/* CAP-002: the launch block may end with the startup table and a
	 * trailer (u64 table bytes, u64 "CUBITLB1"; CuBit.Launch_Envelopes).
	 * Its payload readers see only the payload's length. */
	movq	%rdi, %rax			/* payload length, unless a trailer says less */
	cmpq	$16, %rdi
	jb	  .launch_split_done
	movabsq $0x00005A0000000000, %rcx  /* Launch_Envelopes block address */
	addq	%rdi, %rcx			/* the block's end */
	movabsq $0x31424C5449425543, %rdx
	cmpq	-8(%rcx), %rdx
	jne	 .launch_split_done
	movq	-16(%rcx), %rdx		 /* table bytes */
	cmpq	$3216, %rdx			 /* Startup_Tables.Maximum_Table_Bytes */
	ja	  .launch_split_done
	addq	$7, %rdx
	andq	$-8, %rdx			   /* padded to a word */
	addq	$16, %rdx			   /* and the trailer */
	cmpq	%rdi, %rdx
	ja	  .launch_split_done
	movq	%rdi, %rax
	subq	%rdx, %rax
.launch_split_done:
	mov %rax,%rsi            /* the launch block's payload length */
	mov %rsp,%rdi            /* the initial stack pointer */
	and $-16,%rsp
	call __cubit_start
	hlt
	.section .note.GNU-stack,"",@progbits
