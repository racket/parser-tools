#cs
(module parser-actions mzscheme

  ;; The entries into the action table
  
  (provide shift? reduce? accept? 
	   shift-state reduce-prod-num reduce-lhs-num reduce-rhs-length 
	   make-shift make-reduce make-accept)

  ;; action = (shift int)
  ;;        | (reduce int int int)
  ;;        | (accept)
  ;;        | int>=0
  ;;        | #f

  (define (shift? x) (and (integer? x) (< x 0)))
  (define (make-shift x) (- (+ x 1)))
  (define (shift-state x) (- (+ x 1)))
  (define reduce? vector?)
  (define make-reduce vector)
  (define (reduce-prod-num x) (vector-ref x 0))
  (define (reduce-lhs-num x) (vector-ref x 1))
  (define (reduce-rhs-length x) (vector-ref x 2))
  (define (accept? x) (eq? x 'accept))
  (define (make-accept) 'accept)
   
  ;(define-struct shift (state) (make-inspector))
  ;(define-struct reduce (prod-num lhs-num rhs-length) (make-inspector))
  ;(define-struct accept () (make-inspector))
  )
