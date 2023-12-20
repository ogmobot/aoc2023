; Compile and link with
;   nasm <this>.asm -f elf64 -o <object>.o ; ld <object>.o -o <binary>

%define SYSCALL_WRITE      1
%define SYSCALL_EXIT      60
%define TABLE_SIZE       256    ; (in cells)
%define MAX_CELLS       4096
%define CELL_SIZE         16    ; (in bytes)

    global _start
    section .text
_start:
    mov rax, SYSCALL_WRITE
    mov rdi, 1  ; file descriptor (stdout)
    mov rsi, message ; pointer to string
    mov rdx, 14 ; message length
    syscall
    mov rax, SYSCALL_EXIT
    xor rdi, rdi ; exit code (0)
    syscall

; FIXME
; rough sketches for required functions
get_hash:
    ; given STR
    ; acc := 0
    ; i := 0
    ; while STR[i] != \0
    ;   acc += STR[i]
    ;   acc *= 17
    ;   acc &= 0xFF
    ;   i++
    ; return acc
key_match:
    ; A, B are pointers to strings
    ; (e.g. ptr to cell[0..7] and ptr to hashkeybuffer)
    ; caller must ensure they're null-terminated
    ; i := 0
    ; loop
    ;   if A[i] != B[i]
    ;     return false
    ;   if A[i] == \0
    ;     return true
    ;   i++
    ;   goto loop
create_cell:
    ; Given KEY and VAL, inserts those into a free cell
    ; and returns a pointer to it
    ; ptr := ht_cells
    ; while (ptr[7] != 0)
    ;    ptr += CELL_SIZE
    ; strcopy ptr[0..7], KEY
    ; ptr[7] = 0x80 | VAL
    ; ptr[8..16] = NULL
    ; return ptr

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

load_file_into_memory:
    ; TODO figure out syscalls
solve_p1:
    ; fptr := file_contents
    ; i := 0
    ; total := 0
    ; loop
    ;   if *fptr == \n or *fptr == ,
    ;       hashkeybuffer[i] = \0
    ;       total += get_hash(hashkeybuffer)
    ;       if  *fptr == \n
    ;           print total
    ;           return
    ;       i := 0
    ;   hashkeybuffer[i] = *fptr
    ;   fptr++
    ;   i++
    ;   goto loop
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

    section .data
message:
    db "Hello, world!", 10
hashkeybuffer:
    times 16 db 0

file_contents:
    times 0x6000 db 0

hash_table:
    ; Each element of the table is a dummy cell with a raw pointer to
    ; a linked list
    times (CELL_SIZE * TABLE_SIZE) db 0
ht_cells:
    ; This is a pool of "ht" cells, each consisting of
    ; - 1 bit flag for whether this cell is in use (1=yes, 0=no)
    ; - 7-byte string identifier (\0 must appear at or before 7th byte)
    ; - 7-bit numerical value
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
