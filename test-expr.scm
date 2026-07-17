; SPDX-FileCopyrightText: 2026 José Bollo
;
; SPDX-License-Identifier: MIT
; SRFI-266 demo by José Bollo, 2026

(import
  (scheme base)
  (srfi srfi-64)
  (srfi srfi-111)
)

(cond-expand
  (guile
    (import
            (rnrs arithmetic fixnums (6))
            (rnrs arithmetic flonums (6))
            (rnrs arithmetic bitwise (6))))
  (else
    (import
            (srfi srfi-143)
            (srfi srfi-144)
            (srfi srfi-151)))
)

;--------------------------------------------------------

(include "srfi/expr-impl.scm")

;--------------------------------------------------------

(define-syntax test-set-aux
  (syntax-rules ()
    ((_ ((ex ...) re) ...)
      (begin
          (test-equal (expr ex ...) re) ... ))))

(define-syntax test-set
  (syntax-rules ()
    ((_ title items ...)
      (begin
        (newline)
        (test-begin title)
        (test-set-aux items ...)
        (test-end title)
        (newline)))))

;--------------------------------------------------------
(define a 10)
(define b 100)
(define c 1000)
(define d 10000)
(define e 100000)
(define f 1000000)
(define i 4)
(define j 7)
(define u 20)
(define v 30)
(define w 40)
(define x 50)
(define y 60)
(define z 70)
(define (f0) 500)
(define (f1 x) (* 2 x))
(define (f2 x y) (+ (* 2 x) (* 3 y)))
(define (f3 x y z) (+ (* 2 x) (* 3 y) (* 5 z)))
(define (g1 x) (* 7 x))
(define (ff p) (lambda (x) (+ 13 (p x))))
(define B (box 53))
(define (ff1 p x) (* 103 (p x)))
(define dx 0.000005)
(define x 1)
(define (g x) (- (* x x) 1))
(define (deriv p) (lambda (x) (/ (- (p (+ x dx)) (p x)) dx)))
(define (name-prefix x) (+ x 1000000))
(define (proc-prefix x) (+ x 2000000))
(define (name-left x y) (+ x y 3000000))
(define (proc-left x y) (+ x y 4000000))
(define (name-right x y) (+ x y 5000000))
(define (proc-right x y) (+ x y 6000000))
(define (name-list . x) (apply + 7000000 x))
(define (proc-list . x) (apply + 8000000 x))
(define (name-compare . x) (apply + 9000000 x))
(define (proc-compare . x) (apply + 9100000 x))
(define (name-ternary x y z) (+ x y z 100))
(define (proc-ternary x y z) (+ x y z 200))

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

;--------------------------------------------------------

(include "test-sets.scm")

