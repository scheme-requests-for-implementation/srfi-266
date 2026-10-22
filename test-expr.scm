; SPDX-FileCopyrightText: 2026 José Bollo
;
; SPDX-License-Identifier: MIT
; SRFI-266 demo by José Bollo, 2026

(import
  (srfi srfi-64)
  (srfi srfi-11)
  (srfi srfi-111)
  (rnrs arithmetic fixnums (6))
  (rnrs arithmetic flonums (6))
  (rnrs arithmetic bitwise (6)))

(set! test-log-to-file #f)

;--------------------------------------------------------

(import (srfi srfi-266))

;--------------------------------------------------------

(define-syntax test-set-aux
  (syntax-rules ()
    ((_ ((ex ...) re) ...)
      (begin
         (let ((cur (expr ex ...))
               (exd re))
           (test-equal '(ex ...) cur exd)
           (when #t
             (display '(ex ...))
             (display "  => ")
             (display exd)
             (newline)
             (newline))
          ) ... ))))

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
(set! x 1)
(define (g x) (- (* x x) 1))
(define (deriv p) (lambda (x) (/ (- (p (+ x dx)) (p x)) dx)))
(define (square x) (* x x))

;--------------------------------------------------------

(include "test-sets.scm")

