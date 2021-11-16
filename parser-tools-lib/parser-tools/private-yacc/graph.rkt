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
    (letrec [
             ;; Will map elements of 'a to 'b sets
             (results (make-hasheq))
             (f (lambda (x) (hash-ref results x fail)))
             
             ;; Maps elements of 'a to integers.
             (N (make-hasheq))
             (get-N (lambda (x) (hash-ref N x zero-thunk)))
             (set-N (lambda (x d) (hash-set! N x d)))
             
             (stack null)
             (push (lambda (x)
                     (set! stack (cons x stack))))
             (pop (lambda () 
                    (begin0 
                     (car stack)
                     (set! stack (cdr stack)))))
             (depth (lambda () (length stack)))

             ;; traverse: 'a -> 
             (traverse
              (lambda (x)
                (push x)
                (let ((d (depth)))
                  (set-N x d)
                  (hash-set! results x (f- x))
                  (for-each (lambda (y)
                              (when (= 0 (get-N y))
                                  (traverse y))
                              (hash-set! results
                                         x
                                         (union (f x) (f y)))
                              (set-N x (min (get-N x) (get-N y))))
                            (edges x))
                  (when (= d (get-N x))
                      (let loop ((p (pop)))
                        (set-N p +inf.0)
                        (hash-set! results p (f x))
                        (when (not (eq? x p))
                            (loop (pop))))))))]
      (for-each (lambda (x)
                  (when (= 0 (get-N x))
                      (traverse x)))
                nodes)
      f))
