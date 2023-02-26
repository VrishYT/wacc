

.global main
main:
	mov r0, #16
	mov r0, r0
	mov r1, #0
	cmp r1, #0
	bleq _errDivZero
	bl __aeabi_idivmod
	mov r1, r0
	push {r0, r1}
	mov r1, r1
	bl _printi
	pop {r0, r1}
	bl exit


.data
	.word 5
.L._prints_str:
	.asciz "%.*s"
.text
_prints:
	push {lr}
	ldr r1, [r0,#-4]
	ldr r0, =.L._prints_str
	bl printf
	mov r0, #0
	bl fflush
	pop {pc}
.data
	.word 40
.L._errDivZero_str0:
	.asciz "fatal error: division or modulo by zero"
.text
_errDivZero:
	ldr r0, =.L._errDivZero_str0
	bl _prints
	mov r0, #255
	bl exit
.data
	.word 3
.L._printi_int:
	.asciz "%d"
.text
_printi:
	push {lr}
	ldr r0, =.L._printi_int
	bl printf
	mov r0, #0
	bl fflush
	pop {pc}