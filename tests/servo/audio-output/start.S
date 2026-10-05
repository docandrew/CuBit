.global _start
.text
_start:
 andq $-16,%rsp
 call main
 ud2
.section .note.GNU-stack,"",@progbits
