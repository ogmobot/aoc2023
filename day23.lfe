;#!/usr/bin/env lfescript

(defmodule day23
    (export
        (obj-hashtable 1)
        (reply 1)
        (all-paths-from 4)
        (main 0)))

(defun obj-hashtable (ht)
    (receive
        ((tuple 'set-key key value)
            (obj-hashtable (maps:put key value ht)))
        ((tuple 'get-key key who)
            (! who (tuple 'response (maps:get key ht)))
            (obj-hashtable ht))))

(defun reply (value)
        (receive
            ((tuple 'query requester-pid)
                (! requester-pid (tuple 'response value)))))

(defun all-paths-from (network path cost terminals)
    (if (lists:member (car path) terminals)
        (progn
            (reply (list (tuple cost (car path)))))
    ; else
        (progn
            (! network (tuple 'get-key (car path) (self)))
            (receive
                ((tuple 'response adj-pairs)
                    (let ((children
                        (lists:map
                            (lambda (pair) ; pair is (coord . dist)
                                (spawn 'day23 'all-paths-from
                                    (list
                                        network
                                        (cons (car pair) path)
                                        (+ cost (cdr pair))
                                        terminals)))
                            (lists:filter
                                (lambda (p)
                                    (not (lists:member (car p) path)))
                                adj-pairs))))
                        (let ((replies
                        ;(reply
                            (lists:append
                                (lists:map
                                    (lambda (child)
                                        (! child (tuple 'query (self)))
                                        (receive ((tuple 'response result) result)))
                                    children))))
                            (reply replies))))))))

(defun setup-network (net-pid)
    (! net-pid (tuple 'set-key "0,1"
        (list (cons "5,3" 15))))
    (! net-pid (tuple 'set-key "3,11"
        (list (cons "11,21" 30) (cons "13,13" 24))))
    (! net-pid (tuple 'set-key "5,3"
        (list (cons "3,11" 22) (cons "13,5" 22))))
    (! net-pid (tuple 'set-key "11,21"
        (list (cons "19,19" 10))))
    (! net-pid (tuple 'set-key "13,5"
        (list (cons "13,13" 12) (cons "19,13" 38))))
    (! net-pid (tuple 'set-key "13,13"
        (list (cons "11,21" 18) (cons "19,13" 10))))
    (! net-pid (tuple 'set-key "19,13"
        (list (cons "19,19" 10))))
    (! net-pid (tuple 'set-key "19,19"
        (list (cons "22,21" 5)))))
;; First example, where slopes are one-way.
;; (longest route should be 94)

(defun longest-path (network)
    (let ((solver (spawn 'day23 'all-paths-from
                (list network '("0,1") 0 '("22,21")))))
        (! solver (tuple 'query (self)))
        (receive
            ((tuple 'response cost-dests)
                (lists:foldr
                    (lambda (cost-dest best)
                        (let (((tuple cost _) cost-dest))
                            (max cost best)))
                    0
                    cost-dests)))))

(defun main ()
    ; TODO get input data
    ; TODO (input->simple input-data ignoreslopesp) function
    ;       that creates a network where each node is a character index
    ;       that links to adjacent character indices with weight 1
    ; TODO (simple->weighted simple-network) function
    ;       that takes a simple network and uses all-paths-from
    ;       to compress it to a much smaller weighted graph
    ; TODO Replace hard-coded 'setup-network with
    ;       (simple->weighted (input->simple input-data 'true))
    ;       and
    ;       (simple->weighted (input->simple input-data 'false))
    ;       for parts 1 and 2 respectively
    (let ((network (spawn 'day23 'obj-hashtable (list #M()))))
        (setup-network network)
        (io:format "~p~n" (list (longest-path network)))))
