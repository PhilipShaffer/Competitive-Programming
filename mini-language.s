.text
main:
    jal input
    move $a1, $t0

    # jal input
    # move $a2, $t0
    
    # jal multi
    # jal plus
    # jal andFunc
    # jal ifStmt
    # jal xPlusY
    jal local
    
    # Exit program
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

ifStmt:
    bne $a1, $a2, then
    j else

then:
    li $t1, 10
    li $t2, 2
    mul $a0, $t1, $t2

    li $v0, 1
    syscall

    li $v0, 4
    la $a0, newline
    syscall

    jr $ra

else:
    li $a0, 14

    li $v0, 1
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

xPlusY:
    sw $a1, x
    lw $a0, x
    # li $v0, 1
    # syscall
    
    # li $v0, 4
    # la $a0, newline
    # syscall

    lw $a1, x
    mul $a0, $a1, $a1
    sw $a0, y
    # li $v0, 1
    # syscall

    # li $v0, 4
    # la $a0, newline
    # syscall

    # print (x+y)
    lw $t0, y
    lw $t1, x
    add $a0, $t0, $t1
    li $v0, 1
    syscall

    li $v0, 4
    la $a0, newline
    syscall

    jr $ra

local:           ##### print (let x = 3 in x * x)
    sw $ra, -8($sp)  # store return address 2 stack frames from the stack pointer
    sw $a1, 0($sp)   # store user-input a1, into stack
    lw $a1, 0($sp)
    mul $t0, $a1, $a1
    sw $t0, -4($sp)  # save t0 into next stack frame

    lw $a0, 0($sp)   # prints stored local variable to ensure corretly saved 
    li $v0, 1
    syscall

    li $v0, 4
    la $a0, newline
    syscall

    lw $a0, -4($sp)  # print result for mul
    li $v0, 1
    syscall

    li $v0, 4
    la $a0, newline
    syscall

    sw $ra, 0($sp)
    jr $ra

.data

s1:       .asciiz "1: "
s2:       .asciiz "2: "
false:    .asciiz "FALSE"
true:     .asciiz "TRUE"
prompt:   .asciiz "Enter an integer: "
newline:  .asciiz "\n"
x:        .word 0
y:        .word 0