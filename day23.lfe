(defmodule day23
    (export
        (obj-hashtable 1)
        (reply 1)
        (all-paths-from 4)
        (all-paths-from-safe 4)
        (main 0)
        (main 1)))

(defun obj-hashtable (ht)
    (receive
        ((tuple 'set key value)
            (obj-hashtable (maps:put key value ht)))
        ((tuple 'get key who)
            (! who (tuple 'response (maps:get key ht)))
            (obj-hashtable ht))
        ((tuple 'get-all who)
            (! who (tuple 'response ht))
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
                ;; Could probably turn this into a maps:fromlist somehow,
                ;; and then just spawn obj-hashtable with the map as arg
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

(defun input->weighted (text ignoreslopesp)
    (let* ((simple-network (input->simple text ignoreslopesp))
           (simple-ht (progn
                (! simple-network (tuple 'get-all (self)))
                (receive ((tuple 'response ht) ht))))
           (terminals (lists:append
                (list
                    (maps:keys
                        (maps:filter
                            (lambda (k v)
                                (andalso
                                    (is_list v)
                                    (> (length v) 2)))
                            simple-ht))
                    (list
                        (maps:get 'start simple-ht)
                        (maps:get 'end   simple-ht))))))
            (let ((result
                    (spawn 'day23 'obj-hashtable (list
                        (maps:from_list (lists:append
                            (list
                                (lists:map
                                    (lambda (term)
                                        (tuple term
                                            (let ((searcher
                                                (spawn 'day23 'all-paths-from-safe (list
                                                    simple-network
                                                    (list term)
                                                    0
                                                    (lists:delete term terminals)))))
                                                (! searcher (tuple 'query (self)))
                                                (receive
                                                    ((tuple 'response x) x)))))
                                    terminals)
                                (list
                                    (tuple 'start (maps:get 'start simple-ht))
                                    (tuple 'end   (maps:get 'end   simple-ht))))))))))
                (! simple-network 'finish)
                result)))

(defun all-paths-from (network path cost terminals)
    (if (lists:member (car path) terminals)
        (reply (list (cons (car path) cost)))
    ; else
        (progn
            (! network (tuple 'get (car path) (self)))
            (receive
                ((tuple 'response adj-pairs)
                    (let ((children
                        (lists:filtermap
                            (lambda (pair) ; pair is (coord . cost)
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

(defun all-paths-from-safe (network path cost terminals)
    ; This doesn't spawn processes at an exponential rate
    (if (lists:member (car path) terminals)
        (reply (list (cons (car path) cost)))
    ;else
        (progn
            (! network (tuple 'get (car path) (self)))
            (receive
                ((tuple 'response adj-pairs)
                    (reply
                        (lists:append
                            (lists:filtermap
                                (lambda (pair) ; pair is (coord . cost)
                                    (if (lists:member (car pair) path)
                                        'false
                                    ; else
                                        (tuple 'true
                                            (let ((child
                                                    (spawn
                                                        'day23
                                                        'all-paths-from-safe
                                                        (list
                                                            network
                                                            (cons (car pair) path)
                                                            (+ cost (cdr pair))
                                                            terminals))))
                                            (! child (tuple 'query (self)))
                                            (receive
                                                ((tuple 'response result)
                                                    result))))))
                                adj-pairs))))))))

(defun longest-path (network)
    (! network (tuple 'get 'start (self)))
    (let ((start (receive ((tuple 'response x) x))))
        (! network (tuple 'get 'end (self)))
        (let ((end (receive ((tuple 'response x) x))))
            (let ((solver (spawn 'day23 'all-paths-from-safe
                    (list network (list start) 0 (list end)))))
                (! solver (tuple 'query (self)))
                (receive
                    ((tuple 'response dest-costs)
                        (lists:foldr
                            (lambda (dest-cost best)
                                (max (cdr dest-cost) best))
                            0
                            dest-costs)))))))

;; Takes ~10m
(defun main ()
    (let* ((input-text (file->string "input23.txt"))
           (network-1 (input->weighted input-text 'false))
           (network-2 (input->weighted input-text 'true)))
        (io:format "~p~n~p~n" (list
            (longest-path network-1)
            (longest-path network-2)))
        (! network-1 'finish)
        (! network-2 'finish)))

(defun main (_) (main))
