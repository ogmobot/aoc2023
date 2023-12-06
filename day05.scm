(load-from-library "collect")
(load-from-library "filter")
(load-from-library "group")
(load-from-library "read-file")
(load-from-library "string-split")

(define (collect-sections all-lines)
    (let ((non-empty (lambda (x) (> (string-length x) 0))))
        (map
            (lambda (line) (filter non-empty line))
            (collect (lambda (x _) (non-empty x)) all-lines))))

(define (line->minimapper line)
    ; There's almost certainly a neater way to do this
    (let* ((words (string-split #\Space line))
           (dst-lower (string->number (car words)))
           (src-lower (string->number (cadr words)))
           (len (string->number (caddr words)))
           (dst-upper (+ dst-lower len))
           (src-upper (+ src-lower len))
           (offset (- dst-lower src-lower)))
        ; returns a function
        ; interval => (processed-intervals . unaffected-intervals)
        (lambda (interval)
            (let ((i-lower (car interval)) (i-upper (cdr interval)))
                (cond
                    ((and (>= i-lower src-lower)
                          (<= i-upper src-upper))
                        ; map entire interval
                        (cons
                            (list (cons (+ i-lower offset)
                                        (+ i-upper offset)))
                            '()))
                    ((and (< i-lower src-lower)
                          (> i-upper src-upper))
                        ; map middle (overlapping) part
                        (cons
                            (list (cons dst-lower dst-upper))
                            (list (cons i-lower src-lower)
                                  (cons src-upper i-upper))))
                    ((and (< i-lower src-lower)
                          (> i-upper src-lower))
                        ; map upper (overlapping) part
                        (cons
                            (list (cons dst-lower (+ i-upper offset)))
                            (list (cons i-lower src-lower))))
                    ((and (< i-lower src-upper)
                          (> i-upper src-upper))
                        ; map lower (overlapping) part
                        (cons
                            (list (cons (+ i-lower offset) dst-upper))
                            (list (cons src-upper i-upper))))
                    (#t
                        ; no match
                        (cons '() (list interval))))))))

(define (apply-mappers mappers xs results)
    ; push a list of intervals through every mapper in this section
    (cond
        ((= (length mappers) 0) (append xs results))
        (#t
            (let ((mapper-results (map (car mappers) xs)))
                (apply-mappers
                    (cdr mappers)
                    (apply append (map cdr mapper-results))
                    (append
                        results
                        (apply append (map car mapper-results))))))))

(define (section->mapper section)
    ; discard section name and assume everything's in order
    (let ((minimappers (map line->minimapper (cdr section))))
        (lambda (xs)
            (apply-mappers minimappers xs '()))))

(define (chain fs)
    ; ((chain f g h) x) => (h (g (f x)))
    (cond ((= (length fs) 0) (lambda (x) x))
          (#t (lambda (x) ((chain (cdr fs)) ((car fs) x))))))

(define *input-text* (with-input-from-file "input05.txt" read-file))
(let* ((sections (collect-sections *input-text*))
       (seed-nums (map string->number
                       (cdr (string-split #\Space (caar sections)))))
       (seeds-p1 (map (lambda (x) (cons x (+ x 1))) seed-nums))
       (seeds-p2 (map
            (lambda (pair) (cons (car pair) (+ (car pair) (cadr pair))))
            (group seed-nums 2)))
       (mappers (map section->mapper (cdr sections)))
       (megamapper (chain mappers)))
    (print (apply min (map car (megamapper seeds-p1))))
    (print (apply min (map car (megamapper seeds-p2)))))
