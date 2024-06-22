;; Run with `guile -x .w --language=wisp <this>`

import : ice-9 format .
import : ice-9 rdelim .
import : ice-9 regex .
import : ice-9 textual-ports .
import : statprof .

define : any? proc vals
    cond
        : null? vals
            . #f
        : proc (car vals)
            . #t
        else
            any? proc (cdr vals)

define : enumify vals start-index
    cond
        : null? vals
            list
        else
            cons
                cons start-index (car vals)
                enumify (cdr vals) {start-index + 1}

define : range x-from x-to ;; inclusive
    cond
        {x-from > x-to}
            list
        else
            cons x-from : range {x-from + 1} x-to 

define : hash-keys ht
    hash-fold
        lambda : key _ keys
            cons key keys
        list
        . ht

define : text->nums text
    . "Converts text to (x0 y0 z0 x1 y1 z1)"
    map : compose string->number match:substring
        list-matches "[0-9]+" text

define : nums->brick nums
    . "Converts (x0 y0 z0 x1 y1 z1) to (xyz0 xyz1 xyz2 ...)"
    . "Assuming x0 <= x1 && y0 <= y1 && z0 <= z1"
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

define : get-smallest-z brick-xs
    apply min : map (lambda (tup) (list-ref tup 2)) brick-xs

define : decrement-z brick-xs
    map
        lambda (b) : list (car b) (cadr b) {(caddr b) - 1}
        . brick-xs

define : collisions coords terrain
    cond
        : null? coords
            list
        : hash-ref terrain (car coords)
            cons (car coords) : collisions (cdr coords) terrain
        else
            collisions (cdr coords) terrain

define : drop-til-you-stop! brick locked-in supports
    let
        : brick-id (car brick)
          brick-xs (cdr brick)
          collisions-beneath
                collisions (decrement-z (cdr brick)) locked-in
        cond
            {(get-smallest-z brick-xs) = 1}
                for-each
                    lambda : x
                        hash-set! locked-in x brick-id
                    . brick-xs
            : not (null? collisions-beneath)
                for-each
                    lambda : x
                        hash-set! locked-in x brick-id
                    . brick-xs
                for-each
                    lambda : x
                        hash-set! supports
                            cons
                                hash-ref locked-in x
                                . brick-id
                            . #t
                    . collisions-beneath
            else
                drop-til-you-stop!
                    cons brick-id (decrement-z brick-xs)
                    . locked-in
                    . supports

define : find-supporters brick-id supports
    . "Find the bricks that support this one"
    map
        lambda : pair
            car pair
        filter
            lambda : pair
                . {(cdr pair) = brick-id}
            hash-keys supports

define : find-supportees brick-id supports
    . "Find the bricks that this one supports"
    map
        lambda : pair
            cdr pair
        filter
            lambda : pair
                . {(car pair) = brick-id}
            hash-keys supports

define : count-safe-pulls supports id-range
    . "Counts the number of bricks that can be removed safely"
    cond
        : null? id-range
            . 0
        else
            + : count-safe-pulls supports (cdr id-range)
                let
                    : supportees : find-supportees (car id-range) supports
                    cond
                        : any? (lambda (x) (= 1 (length (find-supporters x supports)))) supportees
                            . 0
                        else
                            . 1

define : main
    define *supports* : make-hash-table
    define *text-in* : call-with-input-file "input22.txt" get-string-all
    define *bricks*
        enumify
            map : lambda (x) (nums->brick (text->nums x))
                string-split
                    string-trim-right *text-in* #\Newline
                    . #\Newline
            . 1
    ;; Set up the tower...
    let
        : locked-in : make-hash-table (length *bricks*) ;; size of input
        for-each
            lambda : brk
                drop-til-you-stop! brk locked-in *supports*
            sort
                . *bricks*
                lambda : a b
                    . {(get-smallest-z (cdr a)) < (get-smallest-z (cdr b))}
    format #t "~a~%" : count-safe-pulls *supports* : range 1 (length *bricks*)

;statprof main #:count-calls? #t
main
