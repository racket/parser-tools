#cs
(module parser-actions mzscheme

  ;; The entries into the action table
  
  (provide shift? reduce? accept? 
	   shift-state reduce-prod-num reduce-lhs-num reduce-rhs-length 
	   make-shift make-reduce)

  ;; action = (shift int)
  ;;        | (reduce int int int)
  ;;        | (accept)
  ;;        | int
  ;;        | #f

  (define-struct shift (state))
  (define-struct reduce (prod-num lhs-num rhs-length))
  (define-struct accept ())
  )
