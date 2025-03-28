.text
main:
    jal input
    move $a1, $t0

    jal input
    move $a2, $t0
    
    # jal multi
    # jal plus

    jal andFunc

    li $v0, 10
    syscall

input:
    # Prompt user
    li $v0, 4
    la $a0, prompt
    syscall

    # Read integer input
    li $v0, 5
    syscall

    # Move to t0
    move $t0, $v0

    jr $ra

plus:
    li $v0, 4
    la $a0, s1
    syscall
    
    li $v0, 1
    add $a0, $a1, $a2
    syscall

    li $v0, 4
    la $a0, newline
    syscall

    jr $ra

multi:
    li $v0, 4
    la $a0, s2
    syscall
    
    li $v0, 1
    mul $a0, $a1, $a2
    syscall

    li $v0, 4
    la $a0, newline
    syscall

    jr $ra

andFunc:
    and $a0, $a1, $a2

    bne $a0, 0, printTrue
    li $v0, 4
    la $a0, false
    syscall

    li $v0, 4
    la $a0, newline
    syscall

    jr $ra

printTrue:
    li $v0, 4
    la $a0, true
    syscall

    li $v0, 4
    la $a0, newline
    syscall

    jr $ra


.data

s1:       .asciiz "1: "
s2:       .asciiz "2: "
false:    .asciiz "FALSE"
true:    .asciiz "TRUE"
prompt:   .asciiz "Enter an integer: "
newline:  .asciiz "\n"