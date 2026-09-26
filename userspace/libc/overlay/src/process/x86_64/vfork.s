/* CuBit: no vfork; defer to fork, which fails with ENOSYS (programs are
 * started by procmgr, not by forking). */
.text
.global vfork
.weak vfork
.type vfork,@function
vfork:
	jmp fork
