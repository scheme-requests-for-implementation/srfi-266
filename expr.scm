(define-library (srfi NNN)

  (export expr)
  (import (scheme base))

  (begin
    (define-syntax expr
      (syntax-rules ()
	((_ x ...) (state-0 (x ...) () ()))))

    (define-syntax state-0-EXPR
      (syntax-rules ()
	((_ () () (x))	x)
        ((_ x ...)	(syntax-error "state-0-EXPR: invalid expression" x ...))
    )))

  (include "generated-expr.scm")
)

