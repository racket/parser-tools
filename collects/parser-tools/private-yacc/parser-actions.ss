(module parser-actions mzscheme
  (require "grammar.ss")
  (provide (all-defined))

  ;; An action is 
  ;;  - (make-shift int)
  ;;  - (make-reduce prod)
  ;;  - (make-accept)
  ;;  - (make-goto int)

  (define-struct action () (make-inspector))
  (define-struct (shift action) (state) (make-inspector))
  (define-struct (reduce action) (prod) (make-inspector))
  (define-struct (accept action) () (make-inspector))
  (define-struct (goto action) (state) (make-inspector))


  ;; A runtime-action is
  ;; non-negative-int            (shift)
  ;; (vector int symbol int) (reduce)
  ;; 'accept                 (accept)
  ;; negative-int        (goto)
 
  (define (action->runtime-action a)
    (cond
      ((shift? a) (shift-state a))
      ((reduce? a)
       (let ((p (reduce-prod a)))
         (vector (prod-index p)
                 (gram-sym-symbol (prod-lhs p))
                 (vector-length (prod-rhs p)))))
      ((accept? a) 'accept)
      ((goto? a) (- (+ (goto-state a) 1)))))
  
  (define (runtime-shift? x) (and (integer? x) (>= x 0)))
  (define runtime-reduce? vector?)
  (define (runtime-accept? x) (eq? x 'accept))
  (define (runtime-goto? x) (and (integer? x) (< x 0)))

  (define runtime-shift-state values) 
  (define (runtime-reduce-prod-num x) (vector-ref x 0))
  (define (runtime-reduce-lhs x) (vector-ref x 1))
  (define (runtime-reduce-rhs-length x) (vector-ref x 2))
  (define (runtime-goto-state x) (- (+ x 1)))

  )
