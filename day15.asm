; Compile and link with
;   nasm <this>.asm -f elf64 -o <object>.o ; ld <object>.o -o <binary>

%define SYSCALL_READ       0
%define SYSCALL_WRITE      1
%define SYSCALL_OPEN       2
%define SYSCALL_CLOSE      3
%define SYSCALL_EXIT      60
%define MAX_FILE_SIZE 0x6000    ; (in bytes)
%define TABLE_SIZE     0x100    ; (in cells)
%define MAX_CELLS     0x1000
%define CELL_SIZE         16    ; (in bytes)
%define CELL_IN_USE     0x80    ; (bit flag)

    global _start

    section .text

printd_newline:
    ; Prints the value in rdi.
    ; Mangles rax, rcx, rdx, r10, output_buffer.
    mov rax, 0x0a30202020202020 ; "      0\n", no null
    mov [output_buffer], rax
    mov rcx, output_buffer + 6 ; write bytes to here
    mov r10, 10 ; divisor
    mov rax, rdi
    mov rdx, 0
    ; div divides rdx:rax by divisor
    ; quotient=rax, remainder=rdx
pdn_next_digit:
    div r10
    add rdx, 0x30
    mov byte [rcx], dl
    dec rcx
    mov rdx, 0
    cmp rax, 0
    je pdn_done
    jmp pdn_next_digit
pdn_done:
    mov rax, SYSCALL_WRITE
    mov rdi, 1  ; file descriptor (stdout)
    mov rsi, output_buffer ; pointer to string
    mov rdx, 8 ; message length
    syscall
    ret

load_file:
    ; Uses the open, read and close syscalls to load a file into
    ; memory location file_contents. rdi points to filename.
    mov rax, SYSCALL_OPEN
    ; rdi already points to filename
    mov rsi, 0 ; flags (0 = read-only)
    mov rdx, 0 ; mode
    syscall
    ; fd in rax

    mov rdi, rax
    ; save rdi to the stack, just in case READ modifies it
    sub rsp, 8
    push rdi

    mov rax, SYSCALL_READ
    mov rsi, file_contents
    mov rdx, MAX_FILE_SIZE
    syscall

    pop rdi
    add rsp, 8

    ; fd already in rdi
    mov rax, SYSCALL_CLOSE
    syscall
    ret

get_hash:
    ; Hashes the string pointed to by rdi, and puts result in rax.
    ; Mangles rax, rcx, rdx
    mov rax, 0
    mov rdx, 0
gh_next_letter:
    mov byte cl, [rdi + rdx]
    cmp byte cl, 0
    je gh_done
    add word ax, cx
    imul ax, 17
    mov byte ah, 0
    inc rdx
    jmp gh_next_letter
gh_done:
    ret

key_match:
    ; Test whether the strings at rdi and rsi are equal.
    ; Mangles rdi, rsi, rcx and rdx.
    ; Sets rax to 1 if the strings match and 0 otherwise.
    dec rdi
    dec rsi
km_next:
    inc rdi
    inc rsi
    mov cl, [rsi]
    mov dl, [rdi]
    cmp byte cl, dl
    jne km_fail
    cmp byte cl, 0
    jne km_next
km_succeed:
    mov rax, 1
    ret
km_fail:
    mov rax, 0
    ret

create_cell:
    ; Given KEY and VAL, inserts those into a free cell
    ; and returns a pointer to it
    ; rdi is pointer to key
    ; sil is a byte containing value

    ; Find first empty cell
    mov rax, [first_empty_cell]
    sub rax, CELL_SIZE
cc_find_unused:
    add rax, CELL_SIZE
    cmp rax, ht_cell_limit
    jne cc_dont_reset_rax
    mov rax, ht_cells
cc_dont_reset_rax:
    mov cl, [rax+7]
    test byte cl, CELL_IN_USE
    jnz cc_find_unused

    ; rax points to an empty cell
    add rax, CELL_SIZE
    mov [first_empty_cell], rax
    sub rax, CELL_SIZE

    ; cell->next = NULL;
    mov qword [rax + 8], 0
    ; cell->in_use = 1;
    ; cell->val = sil;
    or byte sil, CELL_IN_USE
    mov byte [rax + 7], sil

    ; Now we can use rsi for copying the string into bytes 0..7
    mov rsi, rax
cc_strcpy:
    mov byte cl, [rdi]
    cmp byte cl, 0
    je cc_strcpy_done
    mov byte [rsi], cl
    inc rsi
    inc rdi
    jmp cc_strcpy
cc_strcpy_done:
    mov byte [rsi], 0
    ret

; FIXME
; rough sketches for required functions

insert_into_table:
    ; Given KEY and VAL, updates the hash table.
    ; If KEY doesn't yet exist, append new cell.
    ; hash := get_hash(KEY)
    ; list_ptr := hash_table + (8 * hash)
    ; loop:
    ;   if key_match(KEY, list_ptr[0..7])
    ;     list_ptr[7] = VAL | 0x80
    ;     return
    ;   if list_ptr[8..16] == NULL:
    ;     new_node = create_cell(KEY, VAL)
    ;     list_ptr[8..16] = new_node
    ;     return
    ;   list_ptr = list_ptr[8..16]
    ;   goto loop
delete_from_table:
    ; Given KEY, removes it from the hash table
    ; hash := get_hash(KEY)
    ; list_ptr := hash_table + (8 * hash)
    ; loop
    ;   next_ptr = list_ptr[8..16]
    ;   if next_ptr == NULL
    ;     return
    ;   if key_match(KEY, next_ptr[0..7])
    ;     next_ptr[7] = 0 ; marks cell as free
    ;     list_ptr[8..16] = next_ptr[8..16]
    ;     return
    ;   goto loop

evaluate_table:
    ; determines score of the table for part 2

solve_p1:
    ; rax = pointer to hashkeybuffer
    ;*rbx = pointer to file
    ;  cl = character at pointer
    ;*r12 = problem solution
    ; (* callee responsibility - save on stack)
    sub rsp, 16
    push rbx
    push r12

    mov rbx, file_contents
    mov rax, hashkeybuffer
    mov r12, 0
p1_next_char:
    mov byte cl, [rbx]
    cmp byte cl, 0x0a ; newline
    je p1_done
    cmp byte cl, 0x00 ; NULL (should never happen)
    je p1_done
    cmp byte cl, 0x2c ; comma
    je p1_end_word
    mov byte [rax], cl
    inc rax
    inc rbx
    jmp p1_next_char
p1_end_word:
    mov byte [rax], 0
    mov rdi, hashkeybuffer
    call get_hash
    add r12, rax

    mov rax, hashkeybuffer
    inc rbx
    jmp p1_next_char

p1_done:
    mov byte [rax], 0
    mov rdi, hashkeybuffer
    call get_hash
    add r12, rax

    mov rdi, r12
    call printd_newline

    pop r12
    pop rbx
    add rsp, 16
    ret

solve_p2:
    ; fptr := file_contents
    ; i := 0
    ; loop:
    ;   if *fptr == =
    ;       fptr++
    ;       val := *fptr - '0'
    ;       insert_into_table(hashkeybuffer, val)
    ;       goto next_command
    ;   if *fptr == -
    ;       delete_from_table(hashkeybuffer)
    ;       goto next_command
    ;   hashkeybuffer[i] = *fptr
    ;   i++
    ;   goto loop
    ; next_command
    ;   i := 0
    ;   fptr++ ; advance to newline/comma
    ;   if *fptr == \n
    ;       evaluate_table
    ;       return
    ;   fptr++
    ;   goto loop

_start:
    mov rdi, input_file_name
    call load_file

    call solve_p1

    mov rax, SYSCALL_EXIT
    mov rdi, 0 ; exit code
    syscall

    section .data

first_empty_cell:
    dq ht_cells
output_buffer:
    times 8 db 0
hashkeybuffer:
    times 16 db 0

input_file_name:
    db "input15.txt", 0
file_contents:
    times MAX_FILE_SIZE db 0

hash_table:
    ; Each element of the table is a dummy cell with a raw pointer to
    ; a linked list
    times (CELL_SIZE * TABLE_SIZE) db 0
ht_cells:
    ; This is a pool of "ht" cells, each consisting of
    ; - 1 bit flag for whether this cell is in use (1=yes, 0=no)
    ; - 7-byte string identifier (\0 must appear at or before 7th byte)
    ; - 7-bit unsigned numerical value (0 - 127) (actually 0-9)
    ; - 8-byte pointer to next cell (raw pointer)
    ;   ** OFFSET OF 0 IS ALWAYS INVALID **
    ; These are laid out thus:
    ;   [<-- 7 bytes -->][<-1 bit->][<-7 bits->][<-- 8 bytes -->]
    ;   [   String \0   ][ in_use? ][   value  ][  Raw pointer  ]
    ; If (r) points to the cell:
    ;   - (r) is the first byte of the string identifier
    ;   - (r + 7) & 0x80 equals zero iff the cell is not in use
    ;   - (r + 7) & 0x7F is the numerical value of the cell
    ;   - (r + 8) is location of tail (raw pointer)
    ; When a cell is deleted,
    ;   set its 8th byte (r + 7) to zero.
    ; To create a new cell,
    ;   iterate through memory_pool (16 bytes at a time)
    ;   until a cell with non-zero 8th byte is found and
    ;   set that one up.
    times (CELL_SIZE * MAX_CELLS) db 0
ht_cell_limit:
