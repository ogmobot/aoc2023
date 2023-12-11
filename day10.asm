    # A note on mnemonics:
    # - `li reg val` expands to
    #       `lui reg (upper short of val)
    #        ori reg reg (lower short of val)`
    # - `la label` expands to
    #       `li (address of label)`
    # - `move r1 r2` expands to
    #       `addu r1 $0 r2`
    # - `not r1 r2` expands to
    #       `nor r1 r2 $0`
    .globl main
    .text
load_input_data:
    # syscall 13 - open file
    la $a0 input_file_name
    # access flags
    li $a1 0
    # mode (0 == O_RDONLY)
    li $a2 0
    li $v0 13
    syscall
    sw $v0 file_descriptor

    # syscall 14 - read
    move $a0 $v0
    la $a1 grid_data
    li $a2 0x8000
    li $v0 14
    syscall
    sw $v0 grid_size

    # syscall 16 - close file
    lw $a0 file_descriptor
    li $v0 16
    syscall

    jr $ra

print_int_nl:
    # prints (and mangles) the integer value in $a0
    # syscall 1 - output decimal integer
    li $v0 1
    syscall

    # syscall 11 - putchar
    li $a0 10
    li $v0 11
    syscall

    jr $ra

replace_S_with_pipe:
    # FIXME
    li $t0 '|'
    sb $t0 grid_data($s0)
    lw $t0 go_south
    sw $t0 start_facing
    jr $ra

find_S_and_row_length:
    # t0 character from grid
    # t1 comparison characters 'S' and '\n'
    # t2 index into grid
    li $t2 0
fsarl_loop:
    lbu $t0 grid_data($t2)
    li $t1 'S'
    bne $t0 $t1 fsarl_skip_S
    sw $t2 start_location
    # Must wait until row_length exists before replace_S... can be called.
    b fsarl_skip_NL
fsarl_skip_S:
    # It's guaranteed t0 hasn't changed yet
    #lbu $t0 grid_data($t2)
    li $t1 10
    bne $t0 $t1 fsarl_skip_NL
    lw $t0 row_length
    bne $0 $t0 fsarl_skip_NL
    sw $t2 row_length
fsarl_skip_NL:
    addiu $t2 $t2 1
    # Technically unnecessary to continue through the file after finding
    # both S and row_length, but it's easier to write this way.
    lw $t1 grid_size
    bne $t2 $t1 fsarl_loop
fsarl_done:
    jr $ra

trace_pipe:
    addiu $sp $sp -4
    sw $ra 4 ($sp)

    # a0 total length
    # t0 character from grid
    # t1 comparison character
    # t2 index into grid
    # t3 facing
    li $a0 0
    lw $t2 start_location
    lw $t3 start_facing

    lbu $t0 grid_data($t2)
tp_loop:
    andi $t1 $t0 0x80
    bne $0 $t1 tp_done

    ori $t1 $t0 0x80
    sw $t1 grid_data($t2)

    addu $t2 $t2 $t3
    addiu $a0 1
    lbu $t0 grid_data($t2)
    # TODO update facing based on $t0 and $t3
    # e.g.
    #       li $t1 'L'
    # (and then, if equal to $t0 ...)
    #       lw $t1 go_south
    # (and then, if equal to $t3 ...)
    #       lw $t3 go_east
    # Would it be easier to load one into high bits
    # and the other into the low bits?
    # (If none are found, we're at the start of the loop.)
go_south:
    lw $t3 row_length
    b tp_loop
go_north:
    lw $t3 row_length
    subu $t3 $0 $t3
    b tp_loop
go_east:
    li $t3 1
    b tp_loop
go_west:
    li $t3 -1
    b tp_loop

tp_done:
    sra $a0 $a0 1
    jal print_int_nl

    lw $ra 4 ($sp)
    addiu $sp $sp 4
    jr $ra

tally_interior_area:
    addiu $sp $sp -4
    sw $ra 4 ($sp)

    # a0 total area
    # t0 character from grid
    # t1 comparison characters '|'|80, 'L'|80 and 'J'|80
    # t2 index into grid
    # t3 flag for outside=0 inside=1
    li $a0 0
    li $t2 0
    li $t3 0
tia_loop:
    lbu $t0 grid_data($t2)
    # 'J'|80 == 202
    li $t1 202
    beq $t0 $t1 tia_toggle
    # 'L'|80 == 204
    li $t1 204
    beq $t0 $t1 tia_toggle
    # '|'|80 == 252
    li $t1 252
    bne $t0 $t1 tia_skip
tia_toggle:
    xori $t3 $t3 1
tia_skip:
    add $a0 $a0 $t3
    addiu $t2 $t2 1
    lw $t1 grid_size
    bne $t2 $t1 tia_loop
tia_done:
    jal print_int_nl

    lw $ra 4 ($sp)
    addiu $sp $sp 4
    jr $ra

function_that_calls_a_function:
    # push return value onto stack
    addiu $sp $sp -4
    sw $ra 4 ($sp)

    li $a0 123
    jal  print_int_nl

    # pop return value from stack
    lw $ra 4 ($sp)
    addiu $sp $sp 4
    jr $ra

main:
    jal load_input_data
    jal find_S_and_row_length
    jal replace_S_with_pipe

    jal trace_pipe
    jal tally_interior_area

    # syscall 10 - exit with 0
    li $v0 10
    syscall

    .data
hello_world:
    .asciiz "hello, world!\n"
input_file_name:
    .asciiz "input10.txt"

    .align 2
file_descriptor:
    .word 0
grid_size:
    .word 0
start_location:
    # this get overwritten by find_S_and_row_length
    .word 0
start_facing:
    # this get overwritten by replace_S_with_pipe
    .word 0

row_length:
    # this gets overwritten by find_S_and_row_length
    .word 0
grid_data:
    # need at least 19740 bytes
    .space 0x8000
