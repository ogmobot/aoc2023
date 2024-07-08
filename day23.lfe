(defmodule day23
    (export
        (obj-hashtable 1)
        (reply 1)
        (file->string 1)
        (input->simple 2)
        (all-paths-from 4)
        (longest-path 1)
        (weighted-example 0)
        (main 0)))

(defun obj-hashtable (ht)
    (receive
        ((tuple 'set key value)
            (obj-hashtable (maps:put key value ht)))
        ((tuple 'get key who)
            (! who (tuple 'response (maps:get key ht)))
            (obj-hashtable ht))
        ('finish 'ok)))

(defun reply (value)
    (receive
        ((tuple 'query who)
            (! who (tuple 'response value)))))

(defun file->string (filename)
    (let* (((tuple 'ok fp) (file:open filename (list 'read_ahead)))
           ((tuple 'ok data) (file:read fp 30000)))
        (progn
            (file:close fp)
            (string:chomp data))))

(defun input->simple (text ignoreslopesp)
    ;; Where text is a long, grid-like string
    (let* ((nth-char (lambda (n) (string:slice text n 1)))
           (width (- (length text) (length (string:find text "\n"))))
           (result (spawn 'day23 'obj-hashtable (list #M())))
           (validp (lambda (n) (andalso (>= n 0) (< n (length text)))))
           (adj (lambda (dir i)
                (cond
                    ((== dir 'up)    (- i (+ 1 width)))
                    ((== dir 'down)  (+ i (+ 1 width)))
                    ((== dir 'left)  (- i 1))
                    ((== dir 'right) (+ i 1))
                    ('true i))))
           (possible-moves
                (lambda (index)
                    (let ((glyph (funcall nth-char index)))
                        (cond
                            ((== glyph "#") '())
                            ((andalso (== glyph "^") (not ignoreslopesp))
                                (list (funcall adj 'up index)))
                            ((andalso (== glyph "v") (not ignoreslopesp))
                                (list (funcall adj 'down index)))
                            ((andalso (== glyph "<") (not ignoreslopesp))
                                (list (funcall adj 'left index)))
                            ((andalso (== glyph ">") (not ignoreslopesp))
                                (list (funcall adj 'right index)))
                            ('true
                                (lists:filtermap
                                    (lambda (dir)
                                        (let ((i (funcall adj dir index)))
                                            (if (andalso
                                                (funcall validp i)
                                                (!= (funcall nth-char i) "#"))
                                                (tuple 'true i)
                                            ; else
                                                'false)))
                                    '(up down left right))))))))
        (progn
            (lists:foreach
                (lambda (pair)
                    (! result (tuple 'set
                        (car pair)
                        (lists:map
                            (lambda (dest) (cons dest 1))
                            (cdr pair)))))
                (lists:filtermap
                    (lambda (i)
                        (let ((moves (funcall possible-moves i)))
                            (if (== (length moves) 0)
                                'false
                            ; else
                                (progn
                                    (cond
                                        ((< i width)
                                            (! result (tuple 'set 'start i)))
                                        ((> i (- (length text) width))
                                            (! result (tuple 'set 'end i))))
                                    (tuple 'true (cons i moves))))))
                    (lists:seq 0 (- (length text) 1))))
            result)))

(defun all-paths-from (network path cost terminals)
    (if (lists:member (car path) terminals)
        (reply (list (tuple cost (car path))))
    ; else
        (progn
            (! network (tuple 'get (car path) (self)))
            (receive
                ((tuple 'response adj-pairs)
                    (let ((children
                        (lists:filtermap
                            (lambda (pair)
                                (if (lists:member (car pair) path)
                                    'false
                                ; else
                                    (tuple 'true
                                        (spawn 'day23 'all-paths-from
                                            (list
                                                network
                                                (cons (car pair) path)
                                                (+ cost (cdr pair))
                                                terminals)))))
                            adj-pairs)))
                        (reply
                            (lists:append
                                (lists:map
                                    (lambda (child)
                                        (! child (tuple 'query (self)))
                                        (receive ((tuple 'response result) result)))
                                    children)))))))))

;; First example, where slopes are one-way.
;; (longest route should be 94)
(defun weighted-example ()
    (obj-hashtable #M(
        "0,1" (list (cons "5,3" 15))
        "3,11" (list (cons "11,21" 30) (cons "13,13" 24))
        "5,3" (list (cons "3,11" 22) (cons "13,5" 22))
        "11,21" (list (cons "19,19" 10))
        "13,5" (list (cons "13,13" 12) (cons "19,13" 38))
        "13,13" (list (cons "11,21" 18) (cons "19,13" 10))
        "19,13" (list (cons "19,19" 10))
        "19,19" (list (cons "22,21" 5)))
        'start "0,1"
        'end "22,21"))

(defun longest-path (network)
    (! network (tuple 'get 'start (self)))
    (let ((start (receive ((tuple 'response x) x))))
        (! network (tuple 'get 'end (self)))
        (let ((end (receive ((tuple 'response x) x))))
            (let ((solver (spawn 'day23 'all-paths-from
                    (list network (list start) 0 (list end)))))
                (progn
                    (! solver (tuple 'query (self)))
                    (receive
                        ((tuple 'response cost-dests)
                            (lists:foldr
                                (lambda (cost-dest best)
                                    (let (((tuple cost _) cost-dest))
                                        (max cost best)))
                                0
                                cost-dests))))))))

(defun main ()
    ; TODO (simple->weighted simple-network) function
    ;       that takes a simple network and uses all-paths-from
    ;       to compress it to a much smaller weighted graph
    ; TODO Replace hard-coded 'setup-network with
    ;       (simple->weighted (input->simple input-data 'true))
    ;       and
    ;       (simple->weighted (input->simple input-data 'false))
    ;       for parts 1 and 2 respectively
    (let ((network-1 (input->simple (file->string "input23.txt") 'false)))
        (io:format "~p~n" (list (longest-path network-1)))
        (! network-1 'finish)))
