    # Note:
    # - `la label` expands to
    #       `li (address of label)`

    # s0: grid_size
    # s1: file_descriptor, then row_length

    .globl main
    .text
load_input_data:
    # syscall 13 - open file
    la $a0 input_file_name
    # access flags
    xor $a1 $a1 $a1
    # mode (0 == O_RDONLY)
    xor $a2 $a2 $a2
    addiu $v0 $0 13
    syscall
    sw $v0 file_descriptor

    # syscall 14 - read
    addu $a0 $0 $v0
    la $a1 grid_data
    addiu $a2 $0 0x6000
    addiu $v0 $0 14
    syscall
    sw $v0 grid_size

    # syscall 16 - close file
    lw $a0 file_descriptor
    addiu $v0 $0 16
    syscall

    jr $ra

print_int_nl:
    # prints (and mangles) the integer value in $a0
    # syscall 1 - output decimal integer
    addiu $v0 $0 1
    syscall

    # syscall 11 - putchar
    addiu $a0 $0 10
    addiu $v0 $0 11
    syscall

    jr $ra

replace_S_with_pipe:
    # J=74 L=76 7=55 F=70 -=45 |=124
    # t0 character from grid or to store
    # t1 comparison character and facing to store
    # t2 index into grid
    # t3 table lookup
    xor $t3 $t3 $t3

    lw $t2 start_location
    addiu $t2 $t2 1
    lb $t0 grid_data($t2)
    addiu $t1 $t0 -45
    beq $0 $t1 rswp_connects_east
    addiu $t1 $t0 -55
    beq $0 $t1 rswp_connects_east
    addiu $t1 $t0 -74
    beq $0 $t1 rswp_connects_east
    b rswp_check_south
rswp_connects_east:
    addiu $t3 $t3 1
rswp_check_south:
    lw $t2 start_location
    lw $t1 row_length
    addu $t2 $t2 $t1
    lb $t0 grid_data($t2)
    addiu $t1 $t0 -124
    beq $0 $t1 rswp_connects_south
    addiu $t1 $t0 -76
    beq $0 $t1 rswp_connects_south
    addiu $t1 $t0 -74
    beq $0 $t1 rswp_connects_south
    b rswp_check_west
rswp_connects_south:
    addiu $t3 $t3 2
rswp_check_west:
    # J=74 L=76 7=55 F=70 -=45 |=124
    lw $t2 start_location
    addiu $t2 $t2 -1
    lb $t0 grid_data($t2)
    addiu $t1 $t0 -45
    beq $0 $t1 rswp_connects_west
    addiu $t1 $t0 -70
    beq $0 $t1 rswp_connects_west
    addiu $t1 $t0 -76
    beq $0 $t1 rswp_connects_west
    b rswp_check_north
rswp_connects_west:
    addiu $t3 $t3 4
rswp_check_north:
    lw $t2 start_location
    lw $t1 row_length
    subu $t2 $t2 $t1
    lb $t0 grid_data($t2)
    addiu $t1 $t0 -124
    beq $0 $t1 rswp_connects_north
    addiu $t1 $t0 -70
    beq $0 $t1 rswp_connects_north
    addiu $t1 $t0 -55
    beq $0 $t1 rswp_connects_north
    b rswp_connects_done
rswp_connects_north:
    addiu $t3 $t3 8

rswp_connects_done:
    lbu $t0 S_type_table($t3)

    # If S is |     : face south
    # If S is 7 J   : face west
    # If S is F L - : face east
    addiu $t1 $t0 -124
    beq $0 $t1 rswp_face_south
    addiu $t1 $t0 -55
    beq $0 $t1 rswp_face_west
    addiu $t1 $t0 -74
    beq $0 $t1 rswp_face_west
    addiu $t1 $0 1
    b rswp_done
rswp_face_south:
    lw $t1 row_length
    b rswp_done
rswp_face_west:
    addiu $t1 $0 -1
rswp_done:
    sw $t1 start_facing
    lw $t2 start_location
    sb $t0 grid_data($t2)
    jr $ra

find_S_and_row_length:
    # t0 character from grid
    # t1 comparison characters 'S' and '\n'
    # t2 index into grid
    xor $t2 $t2 $t2
fsarl_loop:
    lbu $t0 grid_data($t2)
    addiu $t1 $0 'S'
    bne $t0 $t1 fsarl_skip_S
    sw $t2 start_location
    # Must wait until row_length exists before replace_S... can be called.
    b fsarl_skip_NL
fsarl_skip_S:
    # It's guaranteed t0 hasn't changed yet
    #lbu $t0 grid_data($t2)
    addiu $t1 $0 10
    bne $t0 $t1 fsarl_skip_NL
    lw $t0 row_length
    bne $0 $t0 fsarl_skip_NL
    addiu $t1 $t2 1
    sw $t1 row_length
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
    xor $a0 $a0 $a0
    lw $t2 start_location
    lw $t3 start_facing

    lbu $t0 grid_data($t2)
tp_loop:
    andi $t1 $t0 0x80
    bne $0 $t1 tp_done

    ori $t1 $t0 0x80
    sb $t1 grid_data($t2)

    addu $t2 $t2 $t3
    addiu $a0 $a0 1
    lbu $t0 grid_data($t2)

    addiu $t1 $t3 1
    beq $t1 $0 facing_west
    addiu $t1 $t3 -1
    beq $t1 $0 facing_east
    lw $t1 row_length
    beq $t3 $t1 facing_south
    # b facing_north

    # J=74 L=76 7=55 F=70
facing_north:
    addiu $t1 $t0 -55
    beq $0 $t1 go_west
    addiu $t1 $t0 -70
    beq $0 $t1 go_east
    b tp_loop
facing_west:
    addiu $t1 $t0 -76
    beq $0 $t1 go_north
    addiu $t1 $t0 -70
    beq $0 $t1 go_south
    b tp_loop
facing_east:
    addiu $t1 $t0 -74
    beq $0 $t1 go_north
    addiu $t1 $t0 -55
    beq $0 $t1 go_south
    b tp_loop
facing_south:
    addiu $t1 $t0 -74
    beq $0 $t1 go_west
    addiu $t1 $t0 -76
    beq $0 $t1 go_east
    b tp_loop

go_south:
    lw $t3 row_length
    b tp_loop
go_north:
    lw $t3 row_length
    subu $t3 $0 $t3
    b tp_loop
go_east:
    addiu $t3 $0 1
    b tp_loop
go_west:
    addiu $t3 $0 -1
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
    # t1 comparison characters
    # t2 index into grid
    # t3 flag for outside=0 inside=1
    xor $a0 $a0 $a0
    xor $t2 $t2 $t2
    xor $t3 $t3 $t3
tia_loop:
    lbu $t0 grid_data($t2)
    # 'J'|80 == 202
    addiu $t1 $t0 -202
    beq $0 $t1 tia_toggle
    # 'L'|80 == 204
    addiu $t1 $t0 -204
    beq $0 $t1 tia_toggle
    # '|'|80 == 252
    addiu $t1 $t0 -252
    bne $0 $t1 tia_skip_toggle
tia_toggle:
    xori $t3 $t3 1
tia_skip_toggle:
    addiu $t1 $t0 -0x80
    bgez $t1 tia_skip_increment_total
    addu $a0 $a0 $t3
tia_skip_increment_total:
    addiu $t2 $t2 1
    lw $t1 grid_size
    bne $t2 $t1 tia_loop
tia_done:
    jal print_int_nl

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
    addiu $v0 $0 10
    syscall

    .data
input_file_name:
    .asciiz "input10.txt"

S_type_table:
    # lookup with bitmask of NWSE
    .ascii "xxxFx-7xxL|xJxxx"

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
    .space 0x6000
