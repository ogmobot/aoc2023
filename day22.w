import : ice-9 format .
import : ice-9 rdelim .
import : ice-9 regex .
import : ice-9 textual-ports .

define : any? proc vals
    cond
        : null? vals
            . #f
        : proc (car vals)
            . #t
        else
            any? proc (cdr vals)

define : all? proc vals
    cond
        : null? vals
            . #t
        : proc (car vals)
            all? proc (cdr vals)
        else
            . #f

define : subset? a b
    all?
        lambda : x
            member x b
        . a

define : uniq xs
    . "Removes duplicates from a sorted list"
    cond
        {(length xs) < 2}
            . xs
        : = (car xs) (cadr xs)
            uniq (cdr xs)
        else
            cons (car xs) (uniq (cdr xs))

define : cachify-2 proc
    let
        : cache (make-hash-table)
        lambda : a b
            cond
                : hash-ref cache (cons a b)
                    hash-ref cache (cons a b)
                else
                    let
                        : res : proc a b
                        hash-set! cache (cons a b) res
                        . res

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

define : make-support-lookup supports id-range
    . "Creates a function that returns supporters or supportees"
    let
        : supporter-network (make-hash-table)
          supportee-network (make-hash-table)
        for-each
            lambda : x
                hash-set! supporter-network x (find-supporters x supports)
                hash-set! supportee-network x (find-supportees x supports)
            . id-range
        lambda : x which
            cond
                : equal? which 'supporters
                    hash-ref supporter-network x
                : equal? which 'supportees
                    hash-ref supportee-network x
                else
                    . #f

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

define : count-falls-no-cache support-lookup removed
    . "How many other bricks will fall when everything in `removed` is gone?"
    let*
        : find-supportees-1 (lambda (x) (support-lookup x 'supportees))
          gone?
            lambda (xs) (subset? xs removed)
          might-fall
            apply append : map find-supportees-1 removed
          can-fall
            filter
                lambda (x) (not (member x removed))
                . might-fall
          will-fall
            filter
                lambda (x) (gone? (support-lookup x 'supporters))
                . can-fall
        cond
            {(length will-fall) = 0}
                . {(length removed) - 1}
            else
                count-falls
                    . support-lookup
                    uniq
                        sort
                            append will-fall removed
                            . <

define count-falls : cachify-2 count-falls-no-cache

define : count-safe-pulls support-lookup id-range
    . "Counts the number of bricks that can be removed safely"
    length
        filter
            lambda (x) {(count-falls support-lookup (list x)) = 0}
            . id-range

define : main
    let*
        : text-in : call-with-input-file "input22.txt" get-string-all
          bricks
            enumify
                map : lambda (x) (nums->brick (text->nums x))
                    string-split
                        string-trim-right text-in #\Newline
                        . #\Newline
                . 1
          id-range : range 1 (length bricks)
          supports : make-hash-table (length bricks)
        ;; Set up the tower...
        let
            : locked-in : make-hash-table (length bricks)
            for-each
                lambda : brk
                    drop-til-you-stop! brk locked-in supports
                sort
                    . bricks
                    lambda : a b
                        . {(get-smallest-z (cdr a)) < (get-smallest-z (cdr b))}
        let
            : support-lookup : make-support-lookup supports id-range
            format #t "~a~%"
                count-safe-pulls support-lookup id-range
            format #t "~a~%"
                apply +
                    map
                        lambda : x
                            count-falls support-lookup : list x
                        . id-range

main
