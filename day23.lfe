(defmodule day23
    (export
        (echo 0)
        (reply 1)
        (solve 2)
        (main 0)
        (main 1)))

(defun echo ()
    (let ((answer (receive (value value))))
        (receive
            ((tuple 'query who)
                (! who (tuple 'response answer))))))

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
                                (list (cons (funcall adj 'up index) 1)))
                            ((andalso (== glyph "v") (not ignoreslopesp))
                                (list (cons (funcall adj 'down index) 1)))
                            ((andalso (== glyph "<") (not ignoreslopesp))
                                (list (cons (funcall adj 'left index) 1)))
                            ((andalso (== glyph ">") (not ignoreslopesp))
                                (list (cons (funcall adj 'right index) 1)))
                            ('true
                                (lists:filtermap
                                    (lambda (dir)
                                        (let ((i (funcall adj dir index)))
                                            (if (andalso
                                                (>= i 0)
                                                (< i (length text))
                                                (!= (funcall nth-char i) "#"))
                                                (tuple 'true (cons i 1))
                                            ; else
                                                'false)))
                                    '(up down left right)))))))
           (remember-start (spawn 'day23 'echo '()))
           (remember-end   (spawn 'day23 'echo '()))
           (terrain-tuple-list
                (lists:filtermap
                    (lambda (i)
                        (let ((moves (funcall possible-moves i)))
                            (if (== (length moves) 0)
                                'false
                            ; else
                                (progn
                                    (cond
                                        ((< i width)
                                            (! remember-start i))
                                        ((> i (- (length text) width))
                                            (! remember-end i)))
                                    (tuple 'true (tuple i moves))))))
                    (lists:seq 0 (- (length text) 1))))
           (start-index (progn
                (! remember-start (tuple 'query (self)))
                (receive ((tuple 'response value) value))))
           (end-index (progn
                (! remember-end (tuple 'query (self)))
                (receive ((tuple 'response value) value)))))
            (maps:merge
                (maps:from_list terrain-tuple-list)
                `#M(start ,start-index end ,end-index))))

(defun input->weighted (text ignoreslopesp)
    (let* ((simple-ht (input->simple text ignoreslopesp))
           (start-index (maps:get 'start simple-ht))
           (end-index   (maps:get 'end   simple-ht))
           (terminals (cons start-index
                      (cons end-index
                      (maps:keys
                        (maps:filter
                            (lambda (k v) (> (length v) 2))
                            (maps:without '(start end) simple-ht)))))))
            (maps:merge
                (maps:from_list
                    (lists:map
                        (lambda (term)
                            (tuple term
                                (all-paths-from-rec
                                    simple-ht
                                    term
                                    (lists:delete term terminals))))
                        terminals))
                `#M(start ,start-index end ,end-index))))

(defun all-paths-from-rec (network path cost terminals)
    ; This doesn't spawn processes at an exponential rate
    (if (lists:member (car path) terminals)
        (list (cons (car path) cost))
    ;else
        (let ((adj-pairs (maps:get (car path) network)))
            (lists:append
                (lists:filtermap
                    (lambda (pair) ; pair is (coord . cost)
                        (if (lists:member (car pair) path)
                            'false
                        ; else
                            (tuple 'true
                                (all-paths-from-rec
                                    network
                                    (cons (car pair) path)
                                    (+ cost (cdr pair))
                                    terminals))))
                        adj-pairs)))))

(defun all-paths-from-rec (network start-node terminals)
    (all-paths-from-rec network (list start-node) 0 terminals))

(defun longest-path (network)
    (let ((dest-costs
            (all-paths-from-rec
                network
                (maps:get 'start network)
                (list (maps:get 'end network)))))
        (lists:foldr
            (lambda (dest-cost best)
                (max (cdr dest-cost) best))
            0
            dest-costs)))

(defun solve (input-text ignoreslopesp)
    (let ((network (input->weighted input-text ignoreslopesp)))
        (reply (longest-path network))))

;; Takes ~60s
(defun main ()
    (let* ((input-text (file->string "input23.txt"))
           (part-1 (spawn 'day23 'solve (list input-text 'false)))
           (part-2 (spawn 'day23 'solve (list input-text 'true))))
        (! part-1 (tuple 'query (self)))
        (receive ((tuple 'response answer) (io:format "~p~n" (list answer))))
        (! part-2 (tuple 'query (self)))
        (receive ((tuple 'response answer) (io:format "~p~n" (list answer))))))

(defun main (_) (main))
