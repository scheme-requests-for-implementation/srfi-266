; SPDX-FileCopyrightText: 2026 José Bollo
;
; SPDX-License-Identifier: MIT
; SRFI-266 demo by José Bollo, 2026

(include "srfi/expr-impl.scm")

(use-modules
  (language tree-il) ; specific to GUILE
  (srfi srfi-64))

;--------------------------------------------------------
; test if a symbol matches a temporary name
(define (temporary? x)
  (and (symbol? x)
       (let ((s (symbol->string x)))
          (and (>= (string-length s) 2)
               (string=? (substring s 0 2) "t-")))))

; rename temporary names using predefined pattern
; in order to be comparable
(define (rename-temporaries item)
  (define (rt item tmps cont)
    (cond
      ((temporary? item)
        (let ((memo (assoc item tmps)))
          (if memo
            (cont (cdr memo) tmps)
            (let* ((name  (string-append "temporary-"
                                         (number->string (+ 1 (length tmps)))))
                   (symb  (string->symbol name))
                   (memo  (cons item symb)))
              (cont symb (cons memo tmps))))))
      ((pair? item)
        (rt (car item) tmps
            (lambda (item-car tmps)
              (rt (cdr item) tmps
                  (lambda (item-cdr tmps)
                    (cont (if (and (eq? (car item) item-car)
                                   (eq? (cdr item) item-cdr))
                            item
                            (cons item-car item-cdr))
                          tmps))))))
      (else
        (cont item tmps))))
  (rt item '() (lambda (x y) x)))


; test of one expression
(define (test-one-expr x)
  (let* ((e  (car x))  ; expression to test
         (r  (cadr x)) ; expected result
         (ex (cons 'expr e)) ; s-expr to be expanded
         (rx (tree-il->scheme (macroexpand ex))) ; resulting s-expr after expansion
         (rx (rename-temporaries rx))) ; replace random temporaries by deterministc ones
    ; testing
    (test-equal e r rx)
    (when #t
      (display ex)
      (display "  => ")
      (display rx)
      (newline)
      (newline))
    ))

;--------------------------------------------------------

(define-syntax test-set-aux
  (syntax-rules ()
    ((_ ((ex ...) re) ...)
      (begin
          (test-equal (expr ex ...) re) ... ))))

(define-syntax test-set
  (syntax-rules ()
    ((_ title item ...)
      (begin
        (newline)
        (test-begin title)
        (test-one-expr 'item) ...
        (test-end title)
        (newline)))))

;--------------------------------------------------------

(expr-set-prefix prefix-name 10 name-prefix)
(expr-set-prefix prefix-proc 10 (a) (proc-prefix a))
(expr-set-left-infix left-name 50 name-left)
(expr-set-left-infix left-proc 50 (u v) (proc-left u v))
(expr-set-right-infix right-name 50 name-right)
(expr-set-right-infix right-proc 50 (u v) (proc-right u v))
(expr-set-list list-name 40 name-list)
(expr-set-list list-proc 40 a (proc-list . a))
(expr-set-compare compare-name 40 name-compare)
(expr-set-compare compare-proc 40 a (proc-compare . a))
(expr-set-ternary ternary-name-1 ternary-name-2 200 name-ternary)
(expr-set-ternary ternary-proc-1 ternary-proc-2 200 (a b c) (proc-ternary a b c))
(expr-set-ternary ternary-syntax-1 ternary-syntax-2 200 (a b c) (syntax-ternary a b c))

;--------------------------------------------------------

(include "test-sets.scm")

