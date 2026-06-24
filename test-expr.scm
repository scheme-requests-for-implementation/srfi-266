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
    (when #f
      (display ex)
      (newline)
      (display "  => ")
      (display rx)
      (newline)
      (newline))
    ))

(define (test-set title set)
  (newline)
  (test-begin title)
  (for-each test-one-expr set)
  (test-end title)
  (newline))

;--------------------------------------------------------
; standard operations
(define standard-operations-set  '(
        ; standard operations
        ((1)              1)
        ((a)              a)
        (((a))            a)
        ((((f0)))         (f0))
        ((- a)            (- a))
        ((- a + b)        (+ (- a) b))
        ((+ a)            (+ a))
        ((+ - a)          (+ (- a)))
        ((? a)            (if a 1 0))
        ((a + b)          (+ a b))
        ((a + b + c)      (+ a b c))
        ((a - b)          (- a b))
        ((a - b - c)      (- a b c))
        ((a + b - c - d - e + 4 + 5 * f)
	                  (+ (- (+ a b) c d e) 4 (* 5 f)))
        ((a / b)          (/ a b))
        ((a / b / c)      (/ a b c))
        ((a * b)          (* a b))
        ((a * b * c)      (* a b c))
        ((- a ** (i + j)) (- (expt a (+ i j))))
        ((- a * (i + j))  (* (- a) (+ i j)))
        ((a + b * c * d)  (+ a (* b c d)))
        ((a * b + c * d)  (+ (* a b) (* c d)))
        ((a \ b)          (quotient a b))
        ((a \ b \ c)      (quotient (quotient a b) c))
        ((a % b)          (remainder a b))
        ((a % b % c)      (remainder (remainder a b) c))
        ((1 << a % 8)     (bitwise-arithmetic-shift-left 1 (remainder a 8)))
        ((65536 >> a % 8) (bitwise-arithmetic-shift-right 65536 (remainder a 8)))
        ((f1 a)           (f1 a))
        ((f1 a + b)       (+ (f1 a) b))
        ((f1 a * b)       (* (f1 a) b))
        ((f1 (a + b))     (f1 (+ a b)))
        ((f1 (a * b))     (f1 (* a b)))
        ((f3 (a b + c d)) (f3 a (+ b c) d))
        ((f2 ((a + b) c)) (f2 (+ a b) c))
        ((f1 g1 a)        (f1 (g1 a)))
        (((ff g1) a)      ((ff g1) a))
        ((ff1 (g a))      (ff1 g a))
        ((cos(2 * a) + sin(3 * b))
	                  (+ (cos (* 2 a)) (sin (* 3 b))))
        ((square cos a + square sin b)
	                  (+ (square (cos a)) (square (sin b))))
        ((a < b < c)      (< a b c))
        ((a < b <= c)     (let ((temporary-1 b))
			    (and (< a temporary-1)
				 (<= temporary-1 c))))
        ((a < b <= c < d) (let ((temporary-1 b))
			    (and (< a temporary-1)
				 (let ((temporary-2 c))
				   (and (<= temporary-1 temporary-2)
					(< temporary-2 d))))))
        ((a = b = c < d < e)
	                  (let ((temporary-1 c))
			    (and (= a b temporary-1)
				 (< temporary-1 d e))))
        ((@ B != 78)      (not (= (unbox B) 78)))
        ((u implies z)    (or (not u) z))
        ((a and b and c or u or v and not i)
	                  (or (and a b c) u (and v (not i))))
        (((a and b and c) or u or (v and not i))
	                  (or (and a b c) u (and v (not i))))
        ((a and b and (c or u or v) and not i)
	                  (and a b (or c u v) (not i)))
        ((45 if a else 28)
	                  (if a 45 28))
        ((a + b if x implies y else 7 * b + a)
	                  (if (or (not x) y) (+ a b) (+ (* 7 b) a)))
        ((a if (b if c else d) else e)
	                  (if (if c b d) a e))
        ((a if b if c else d else e)
	                  (if (if c b d) a e))
        ((a + b as c in c * d)
	                  (let-values (((c) (+ a b)))
			    (* c d)))
        ((a + b as v in 4 * v ** 2 + 5 * v + 34)
	                  (let-values (((v) (+ a b)))
			    (+ (* 4 (expt v 2)) (* 5 v) 34)))
        ((a + b as v in 4 * v ** 2 + 5 * v - 34)
	                  (let-values (((v) (+ a b)))
			    (- (+ (* 4 (expt v 2)) (* 5 v)) 34)))
        ((a + b as v in 4 * v ** 2 - 5 * v + 34)
	                  (let-values (((v) (+ a b)))
			    (+ (- (* 4 (expt v 2)) (* 5 v)) 34)))
        ((floor/ (a b) as q r in q + ? positive? r)
	                  (let-values (((q r) (floor/ a b)))
			    (+ q (if (positive? r) 1 0))))
        ((a if (b if (u as v in w) else d) else e)
	                  (if (if (let-values (((v) u)) w) b d) a e))
        ((a if b if u as v in w else d else e)
	                  (if (if (let-values (((v) u)) w) b d) a e))
   ))


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

(define user-operations-set  '(
        ; user defined operations
        ((prefix-name 5)                       (name-prefix 5))
        ((prefix-proc 15)                      (proc-prefix 15))
        ((1 left-name 2 left-name 3)           (name-left (name-left 1 2) 3))
        ((1 left-proc 2 left-proc 3)           (proc-left (proc-left 1 2) 3))
        ((1 right-name 2 right-name 3)         (name-right 1 (name-right 2 3)))
        ((1 right-proc 2 right-proc 3)         (proc-right 1 (proc-right 2 3)))
        ((a list-name b list-name c)           (name-list a b c))
        ((a list-proc b list-proc c)           (proc-list a b c))
        ((a compare-name b compare-name c)     (name-compare a b c))
        ((a compare-proc b compare-proc c)     (proc-compare a b c))
        ((1 ternary-name-1 2 ternary-name-2 3) (name-ternary 1 2 3))
        ((1 ternary-proc-1 2 ternary-proc-2 3) (proc-ternary 1 2 3))
   ))

;--------------------------------------------------------

(define sample-expressions-set  '(

        ((sqrt(b * b - 4 * a * c) as delta in
           values ((- b + delta) / (2 * a)
	           (- b - delta) / (2 * a)))
	 (let-values (((delta) (sqrt (- (* b b) (* 4 a c)))))
	   (values (/ (+ (- b) delta) (* 2 a))
                   (/ (- (- b) delta) (* 2 a)))))

	(((g(x + dx) - g(x)) / dx)
	 (/ (- (g (+ x dx)) (g x)) dx))

	(( x - g(x) / deriv(g)(x))
	 (- x (/ (g x) ((deriv g) x))))

	((a < 1 and b + c != 9)
	 (and (< a 1) (not (= (+ b c) 9))))

	((a + b + c < d <= x - y - z)
	 (let ((temporary-1 d)) (and (< (+ a b c) temporary-1) (<= temporary-1 (- x y z)))))
  ))

;--------------------------------------------------------

(test-set "test expr syntax (standard operators)" standard-operations-set)
(test-set "test expr syntax (user defined operators)" user-operations-set)
(test-set "test expr syntax (sample expressions)" sample-expressions-set)

;--------------------------------------------------------

