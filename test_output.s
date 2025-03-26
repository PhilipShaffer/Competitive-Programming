.data
.text
.globl main
main:
    move $fp, $sp
    subu $sp, $sp, 128
    li $v0, 5
    sw $v0, -4($fp)
    li $v0, 10
    sw $v0, -8($fp)
    lw $v0, -4($fp)
    sw $v0, -4($sp)
    addiu $sp, $sp, -4
    lw $v0, -8($fp)
    lw $t1, 0($sp)
    addiu $sp, $sp, 4
    addu $v0, $t1, $v0
    sw $v0, -12($fp)
    lw $v0, -12($fp)
    move $a0, $v0
    li $v0, 1
    syscall
    li $a0, '\n'
    li $v0, 11
    syscall
    lw $v0, -4($fp)
    sw $v0, -4($sp)
    addiu $sp, $sp, -4
    lw $v0, -8($fp)
    lw $t1, 0($sp)
    addiu $sp, $sp, 4
    mul $v0, $t1, $v0
    sw $v0, -16($fp)
    lw $v0, -16($fp)
    move $a0, $v0
    li $v0, 1
    syscall
    li $a0, '\n'
    li $v0, 11
    syscall
    lw $v0, -12($fp)
    sw $v0, -4($sp)
    addiu $sp, $sp, -4
    lw $v0, -16($fp)
    lw $t1, 0($sp)
    addiu $sp, $sp, 4
    sgt $v0, $t1, $v0
    beq $v0, $zero, else_0
    li $v0, 1
    move $a0, $v0
    li $v0, 1
    syscall
    li $a0, '\n'
    li $v0, 11
    syscall
    j endif_1
else_0:
    li $v0, 0
    move $a0, $v0
    li $v0, 1
    syscall
    li $a0, '\n'
    li $v0, 11
    syscall
endif_1:
    li $v0, 10
    syscall
