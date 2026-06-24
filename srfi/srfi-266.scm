; SPDX-FileCopyrightText: 2026 José Bollo
;
; SPDX-License-Identifier: MIT
; SRFI-266 demo by José Bollo, 2026

(define-library (srfi 266) ;expr
  (import (scheme base)
	  (scheme cxr)
          (rnrs syntax-case (6)))
  (export expr
          expr-set-prefix
          expr-set-left-infix
          expr-set-right-infix
          expr-set-list
          expr-set-compare
          expr-set-ternary)
)

