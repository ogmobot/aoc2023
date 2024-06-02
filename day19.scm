(define (split-parts line-list)
    ;; returns (rule-lines . part-lines)
    (if (= 0 (string-length (car line-list)))
        (cons '() (cdr line-list))
        (let ((res (split-parts (cdr line-list))))
            (cons (cons (car line-list) (car res)) (cdr res)))))

(define (split-once-by s ends)
    ;; returns (s[:ending] . s[ending+1:])
    (if (member (string-ref s 0) ends)
        (cons "" (substring s 1 (string-length s)))
        (let ((res (split-once-by (substring s 1 (string-length s)) ends)))
            (cons (string-append (substring s 0 1) (car res)) (cdr res)))))

;; Creating rules from text

(define (cutoff-and-dests rule name index)
    ;; converts "x>50:abc" to ('x 51 `(,name . ,index+1) '("abc" . 0))
    ;;      and "m<50:def" to ('m 50 '("def" . 0) `(,name . ,index+1))
    (let* ((first-rest (split-once-by rule '(#\:)))
           (condition (car first-rest))
           (destination (cdr first-rest))
           (value (string->number
            (substring condition 2 (string-length condition)))))
        (cons
            (string->symbol (substring rule 0 1))
            (if (equal? (string-ref rule 1) #\<)
                (list
                    value (cons destination 0) (cons name (+ index 1)))
                (list
                    (+ 1 value) (cons name (+ index 1)) (cons destination 0))))))

(define (process-rules text name index)
    ;; text looks like "a<2294:vbf,x>2212:xhh,ch}"
    (let* ((first-rest (split-once-by text '(#\, #\})))
           (rule (car first-rest))
           (rules (cdr first-rest)))
        (if (= 0 (string-length rules)) ; last rule?
            (list (list 'x 0 (cons rule 0) (cons rule 0)))
            (cons
                (cutoff-and-dests rule name index)
                (process-rules rules name (+ 1 index))))))

(define (line->rule rule-line)
    ;; key{(filter,)*filter}
    ;; returns (key . list-of-filters)
    ;; where filter is
    ;; '( symbol cutoff dest-if-lower dest-else )
    (let ((key-rulestext (split-once-by rule-line '(#\{))))
        (cons
            (car key-rulestext)
            (process-rules (cdr key-rulestext) (car key-rulestext) 0))))

;; Applying rules

(define (valid-part? multipart)
    (and
        (< (cdr (assq 'xlo multipart)) (cdr (assq 'xhi multipart)))
        (< (cdr (assq 'mlo multipart)) (cdr (assq 'mhi multipart)))
        (< (cdr (assq 'alo multipart)) (cdr (assq 'ahi multipart)))
        (< (cdr (assq 'slo multipart)) (cdr (assq 'shi multipart)))))

(define (rule-split rule multipart)
    (let ((attribute (car rule))
          (cutoff (cadr rule))
          (dest-lt (caddr rule)) (dest-geq (cadddr rule)))
        (let ((lo (string->symbol (string-append (symbol->string attribute) "lo")))
              (hi (string->symbol (string-append (symbol->string attribute) "hi"))))
            (list
                (cons dest-lt
                    ;; new segment for [xlo -- cutoff]
                    (cons
                        (cons hi (min cutoff (cdr (assq hi multipart))))
                        multipart))
                (cons dest-geq
                    ;; new segment for [cutoff -- xhi]
                    (cons
                        (cons lo (max cutoff (cdr (assq lo multipart))))
                        multipart))))))

(define (apply-rules rules packets accepted)
    ;; packets are (dest . multipart)
    ;; accepted is a list of multiparts
    (if (= 0 (length packets))
        accepted
        (let ((dest (caar packets))
              (multipart (cdar packets)))
            (cond
                ((or (not (valid-part? multipart)) (string=? "R" (car dest)))
                    (apply-rules rules (cdr packets) accepted))
                ((string=? "A" (car dest))
                    (apply-rules rules (cdr packets) (cons multipart accepted)))
                (else
                    (let ((relevant-rule
                        (list-ref
                            (table-ref rules (car dest))
                            (cdr dest))))
                        (apply-rules
                            rules
                            (append
                                (rule-split relevant-rule multipart)
                                (cdr packets))
                            accepted)))))))

;; Creating parts from text

(define (line->part part-line)
    ;; "{x=32,m=306,a=2103,s=477}"
    ;; becomes (("in" . 0)
    ;;          ('xlo . 32) ('xhi . 33) ('mlo . 306) ('mhi . 307) ... )
    (let* ((x-rest (split-once-by (substring part-line 1 (string-length part-line)) '(#\,)))
           (x-string (substring (car x-rest) 0 1))
           (x-val (string->number (substring (car x-rest) 2 (string-length (car x-rest)))))
           (m-rest (split-once-by (cdr x-rest) '(#\,)))
           (m-string (substring (car m-rest) 0 1))
           (m-val (string->number (substring (car m-rest) 2 (string-length (car m-rest)))))
           (a-rest (split-once-by (cdr m-rest) '(#\,)))
           (a-string (substring (car a-rest) 0 1))
           (a-val (string->number (substring (car a-rest) 2 (string-length (car a-rest)))))
           (s-rest (split-once-by (cdr a-rest) '(#\, #\})))
           (s-string (substring (car s-rest) 0 1))
           (s-val (string->number (substring (car s-rest) 2 (string-length (car s-rest))))))
        (list
            (cons "in" 0)
            (cons
                (string->symbol (string-append x-string "lo")) x-val)
            (cons
                (string->symbol (string-append x-string "hi")) (+ 1 x-val))
            (cons
                (string->symbol (string-append m-string "lo")) m-val)
            (cons
                (string->symbol (string-append m-string "hi")) (+ 1 m-val))
            (cons
                (string->symbol (string-append a-string "lo")) a-val)
            (cons
                (string->symbol (string-append a-string "hi")) (+ 1 a-val))
            (cons
                (string->symbol (string-append s-string "lo")) s-val)
            (cons
                (string->symbol (string-append s-string "hi")) (+ 1 s-val)))))

(define (part-count multipart)
    (*
        (- (cdr (assq 'xhi multipart)) (cdr (assq 'xlo multipart)))
        (- (cdr (assq 'mhi multipart)) (cdr (assq 'mlo multipart)))
        (- (cdr (assq 'ahi multipart)) (cdr (assq 'alo multipart)))
        (- (cdr (assq 'shi multipart)) (cdr (assq 'slo multipart)))))

;; Main program

(define *input-text*
    (with-input-from-file "input19.txt"
        (lambda () (read-all (current-input-port) read-line))))

(let* ((input-split
            (split-parts *input-text*))
       (rule-table
            (list->table (map line->rule (car input-split)) test: string=?))
       (part-list (map line->part (cdr input-split))))
    (pretty-print (apply +
        (map
            (lambda (mp) (+ (cdr (assq 'xlo mp))
                            (cdr (assq 'mlo mp))
                            (cdr (assq 'alo mp))
                            (cdr (assq 'slo mp))))
            (apply-rules rule-table part-list '()))))
    (pretty-print (fold + 0
        (map part-count
            (apply-rules rule-table
                '((("in" . 0) (xlo . 1) (xhi . 4001)
                              (mlo . 1) (mhi . 4001)
                              (alo . 1) (ahi . 4001)
                              (slo . 1) (shi . 4001)))
                '())))))
