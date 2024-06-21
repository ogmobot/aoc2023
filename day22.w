;; Run with `guile -x .w --language=wisp <this>`

import : ice-9 rdelim .
import : ice-9 regex .

define : range x-from x-to ;; inclusive
   cond
        {x-from > x-to}
            list
        else
            cons x-from : range {x-from + 1} x-to 

define : get-smallest-z brick
    apply min : map (lambda (tup) (list-ref tup 2)) brick

define : text->nums text
    ;; convert text to (x0 y0 z0 x1 y1 z1)
    map : compose string->number match:substring
        list-matches "[0-9]+" text

define : nums->brick nums
    ;; convert (x0 y0 z0 x1 y1 z1) to (xyz0 xyz1 xyz2 ...)
    ;; assume x0 <= x1 && y0 <= y1 && z0 <= z1
    ; ( yes, this is horrible )
    apply append
        apply append
            map
                lambda : x
                    map
                        lambda : y
                            map
                                lambda : z
                                    list x y z
                                range (list-ref nums 2) (list-ref nums 5)
                        range (list-ref nums 1) (list-ref nums 4)
                range (list-ref nums 0) (list-ref nums 3)

define : decrement-z brick
    map
        lambda (b) : list (car b) (cadr b) {(caddr b) - 1}
        . brick

define : collision? coords-a coords-b
    cond
        : null? coords-a
            . #f
        : member (car coords-a) coords-b
            . #t
        else
            collision? (cdr coords-a) coords-b

define : drop-til-you-stop brick locked-in
    cond
        {(get-smallest-z brick) = 1}
            cons brick locked-in
        : collision? (decrement-z brick) (apply append locked-in)
            cons brick locked-in
        else
            drop-til-you-stop (decrement-z brick) locked-in

define : main
    define brk
        nums->brick : text->nums "0,10,20~0,10,22"
    display brk
    newline
    display : drop-til-you-stop brk : list
    newline
    display : collision? brk (decrement-z brk)
    newline

main
