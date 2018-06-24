#lang racket/base
(provide digraph)

(define (zero-thunk) 0)
  
;; digraph: 
;;   ('a list) * ('a -> 'a list) * ('a -> 'b) * ('b * 'b -> 'b) * (-> 'b)
;;     -> ('a -> 'b)
;; DeRemer and Pennello 1982
;; Computes (f x) = (f- x) union Union{(f y) | y in (edges x)}
;; We use a hash-table to represent the result function 'a -> 'b set, so
;; the values of type 'a must be comparable with eq?.

(define (digraph nodes edges f- union fail)
  (define results (make-hasheq))
  (define (f x) (hash-ref results x fail))
  ;; Maps elements of 'a to integers.
  (define N (make-hasheq))
  (define (get-N x) (hash-ref N x zero-thunk))
  (define (set-N x d) (hash-set! N x d))
  (define stack null)
  (define (push x) (set! stack (cons x stack)))
  (define (pop) (begin0 
                  (car stack)
                  (set! stack (cdr stack))))
  (define (depth) (length stack))

  ;; traverse: 'a -> 
  (define (traverse x)
    (push x)
    (define d (depth))
    (set-N x d)
    (hash-set! results x (f- x))
    (for-each (λ (y)
                (when (= 0 (get-N y))
                  (traverse y))
                (hash-set! results
                           x
                           (union (f x) (f y)))
                (set-N x (min (get-N x) (get-N y))))
              (edges x))
    (when (= d (get-N x))
      (let loop ([p (pop)])
        (set-N p +inf.0)
        (hash-set! results p (f x))
        (when (not (eq? x p))
          (loop (pop))))))
  ;; Will map elements of 'a to 'b sets
  (for ([x (in-list nodes)]
        #:when (zero? (get-N x)))
       (traverse x))
  f)

