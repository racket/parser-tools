#cs
(module token-syntax mzscheme

  ;; The things needed at compile time to handle definition of tokens
  
  (provide make-terminals-def terminals-def-t terminals-def?)

  (define-struct terminals-def (t))
)
