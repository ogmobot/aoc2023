#!/usr/bin/env lfescript

(defmodule day23 (export (main 1)))

(defun obj-hashtable (ht)
    (receive
        ((tuple 'set-key key value)
            (obj-hashtable (maps:put key value ht)))
        ((tuple 'get-key key who)
            (! who (maps:get key ht))
            (obj-hashtable ht))))

(defun reply (value)
    (receive (pid (! pid value))))

(defun all-paths-from (network path cost terminals)
    (if (lists:member (car path) terminals)
        (reply (list (tuple cost (car path))))
    ; else
        (progn
            (! network (tuple 'get-key (car path) (self)))
            (receive
                (adj-pairs
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
                                (lambda (pair)
                                    (not (lists:member (car pair) path)))
                                adj-pairs))))
                        (reply
                            (lists:append
                                (lists:map
                                    (lambda (child)
                                        (! child (self))
                                        (receive (result result)))
                                    children)))))))))

(defun main (_)
    (let ((x (spawn 'day23 'obj-hashtable (list #M()))))
        (! x (tuple 'set-key 'foo 'bar))
        (! x (tuple 'get-key 'foo (self)))
        (receive
            (val (io:format "foo: ~p~n" (list val))))
        (io:format "Hello, world!~n")))

(main 0)
