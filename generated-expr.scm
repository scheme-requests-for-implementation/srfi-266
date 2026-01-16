


(define-syntax state-0
   (syntax-rules (not - +)
      ((_ (not E ...) S T)	(state-6 (E ...) (0 . S) T))
      ((_ (- E ...) S T)	(state-10 (E ...) (0 . S) T))
      ((_ (+ E ...) S T)	(state-11 (E ...) (0 . S) T))
      ((_ (((x ...)) E ...) S T)	(state-16 (E ...) (0 . S) ((x ...) . T)))
      ((_ ((x ...) E ...) S T)	(state-16 (E ...) (0 . S) ((expr x ...) . T)))
      ((_ (x E ...) S T)	(state-16 (E ...) (0 . S) (x . T)))
      ((_ x ...)	(syntax-error "state-0: invalid expression" x ...))
))

(define-syntax state-1
   (syntax-rules (if)
      ((_ (if E ...) S T)	(state-17 (E ...) (1 . S) T))
      ((_ E  (0 S ...) T)	(state-0-EXPR E (S ...) T))
))

(define-syntax state-2
   (syntax-rules (or)
      ((_ (or E ...) S T)	(state-18 (E ...) (2 . S) T))
      ((_ E  (53 S ...) T)	(state-53-OR E (S ...) T))
      ((_ E  (17 S ...) T)	(state-39 E (17 S ...) T))
      ((_ E  (0 S ...) T)	(state-1 E (0 S ...) T))
))

(define-syntax state-2-OR~11
   (syntax-rules ()
      ((_ E  (53 S ...) ($2 $1 T ...))	(state-53-OR E (S ...) ((or $1 . $2) T ...)))
      ((_ E  (17 S ...) ($2 $1 T ...))	(state-39 E (17 S ...) ((or $1 . $2) T ...)))
      ((_ E  (0 S ...) ($2 $1 T ...))	(state-1 E (0 S ...) ((or $1 . $2) T ...)))
))

(define-syntax state-3
   (syntax-rules (and)
      ((_ (and E ...) S T)	(state-19 (E ...) (3 . S) T))
      ((_ E  (18 S ...) T)	(state-40 E (18 S ...) T))
      ((_ E  (s S ...) T)	(state-2 E (s S ...) T))
))

(define-syntax state-3-AND~10
   (syntax-rules ()
      ((_ E  (18 S ...) ($2 $1 T ...))	(state-40 E (18 S ...) ((and $1 . $2) T ...)))
      ((_ E  (s S ...) ($2 $1 T ...))	(state-2 E (s S ...) ((and $1 . $2) T ...)))
))

(define-syntax state-4
   (syntax-rules (implies)
      ((_ (implies E ...) S T)	(state-20 (E ...) (4 . S) T))
      ((_ E  (19 S ...) T)	(state-41 E (19 S ...) T))
      ((_ E  (s S ...) T)	(state-3 E (s S ...) T))
))

(define-syntax state-5
   (syntax-rules (not eqv)
      ((_ (not E ...) S T)	(state-21 (E ...) (5 . S) T))
      ((_ (eqv E ...) S T)	(state-22 (E ...) (5 . S) T))
      ((_ E  (20 S ...) T)	(state-20-EQV E (S ...) T))
      ((_ E  (s S ...) T)	(state-4 E (s S ...) T))
))

(define-syntax state-6
   (syntax-rules (not - +)
      ((_ (not E ...) S T)	(state-6 (E ...) (6 . S) T))
      ((_ (- E ...) S T)	(state-10 (E ...) (6 . S) T))
      ((_ (+ E ...) S T)	(state-11 (E ...) (6 . S) T))
      ((_ (((x ...)) E ...) S T)	(state-16 (E ...) (6 . S) ((x ...) . T)))
      ((_ ((x ...) E ...) S T)	(state-16 (E ...) (6 . S) ((expr x ...) . T)))
      ((_ (x E ...) S T)	(state-16 (E ...) (6 . S) (x . T)))
      ((_ x ...)	(syntax-error "state-6: invalid expression" x ...))
))

(define-syntax state-6-NOT
   (syntax-rules ()
      ((_ E  (6 S ...) ($2 T ...))	(state-6-NOT E (S ...) ((not $2) T ...)))
      ((_ E  (22 S ...) ($2 T ...))	(state-22-NOT E (S ...) ((not $2) T ...)))
      ((_ E  (42 S ...) ($2 T ...))	(state-42-NOT E (S ...) ((not $2) T ...)))
      ((_ E  (s S ...) ($2 T ...))	(state-5 E (s S ...) ((not $2) T ...)))
))

(define-syntax state-7
   (syntax-rules (< <= > >= = <>)
      ((_ (< E ...) S T)	(state-23 (E ...) (7 . S) T))
      ((_ (<= E ...) S T)	(state-24 (E ...) (7 . S) T))
      ((_ (> E ...) S T)	(state-25 (E ...) (7 . S) T))
      ((_ (>= E ...) S T)	(state-26 (E ...) (7 . S) T))
      ((_ (= E ...) S T)	(state-27 (E ...) (7 . S) T))
      ((_ (<> E ...) S T)	(state-28 (E ...) (7 . S) T))
      ((_ E  (6 S ...) T)	(state-6-NOT E (S ...) T))
      ((_ E  (22 S ...) T)	(state-22-NOT E (S ...) T))
      ((_ E  (42 S ...) T)	(state-42-NOT E (S ...) T))
      ((_ E  (s S ...) T)	(state-5 E (s S ...) T))
))

(define-syntax state-7-COMP~9
   (syntax-rules ()
      ((_ E  (6 S ...) ($2 $1 T ...))	(state-6-NOT E (S ...) ((< $1 . $2) T ...)))
      ((_ E  (22 S ...) ($2 $1 T ...))	(state-22-NOT E (S ...) ((< $1 . $2) T ...)))
      ((_ E  (42 S ...) ($2 $1 T ...))	(state-42-NOT E (S ...) ((< $1 . $2) T ...)))
      ((_ E  (s S ...) ($2 $1 T ...))	(state-5 E (s S ...) ((< $1 . $2) T ...)))
))

(define-syntax state-7-COMP~8
   (syntax-rules ()
      ((_ E  (6 S ...) ($2 $1 T ...))	(state-6-NOT E (S ...) ((<= $1 . $2) T ...)))
      ((_ E  (22 S ...) ($2 $1 T ...))	(state-22-NOT E (S ...) ((<= $1 . $2) T ...)))
      ((_ E  (42 S ...) ($2 $1 T ...))	(state-42-NOT E (S ...) ((<= $1 . $2) T ...)))
      ((_ E  (s S ...) ($2 $1 T ...))	(state-5 E (s S ...) ((<= $1 . $2) T ...)))
))

(define-syntax state-7-COMP~7
   (syntax-rules ()
      ((_ E  (6 S ...) ($2 $1 T ...))	(state-6-NOT E (S ...) ((> $1 . $2) T ...)))
      ((_ E  (22 S ...) ($2 $1 T ...))	(state-22-NOT E (S ...) ((> $1 . $2) T ...)))
      ((_ E  (42 S ...) ($2 $1 T ...))	(state-42-NOT E (S ...) ((> $1 . $2) T ...)))
      ((_ E  (s S ...) ($2 $1 T ...))	(state-5 E (s S ...) ((> $1 . $2) T ...)))
))

(define-syntax state-7-COMP~6
   (syntax-rules ()
      ((_ E  (6 S ...) ($2 $1 T ...))	(state-6-NOT E (S ...) ((>= $1 . $2) T ...)))
      ((_ E  (22 S ...) ($2 $1 T ...))	(state-22-NOT E (S ...) ((>= $1 . $2) T ...)))
      ((_ E  (42 S ...) ($2 $1 T ...))	(state-42-NOT E (S ...) ((>= $1 . $2) T ...)))
      ((_ E  (s S ...) ($2 $1 T ...))	(state-5 E (s S ...) ((>= $1 . $2) T ...)))
))

(define-syntax state-8
   (syntax-rules (+)
      ((_ (+ E ...) S T)	(state-29 (E ...) (8 . S) T))
      ((_ E  (27 S ...) T)	(state-27-SUM E (S ...) T))
      ((_ E  (28 S ...) T)	(state-28-SUM E (S ...) T))
      ((_ E  (26 S ...) T)	(state-46 E (26 S ...) T))
      ((_ E  (25 S ...) T)	(state-45 E (25 S ...) T))
      ((_ E  (24 S ...) T)	(state-44 E (24 S ...) T))
      ((_ E  (23 S ...) T)	(state-43 E (23 S ...) T))
      ((_ E  (s S ...) T)	(state-7 E (s S ...) T))
))

(define-syntax state-8-SUM~5
   (syntax-rules ()
      ((_ E  (27 S ...) ($2 $1 T ...))	(state-27-SUM E (S ...) ((+ $1 . $2) T ...)))
      ((_ E  (28 S ...) ($2 $1 T ...))	(state-28-SUM E (S ...) ((+ $1 . $2) T ...)))
      ((_ E  (26 S ...) ($2 $1 T ...))	(state-46 E (26 S ...) ((+ $1 . $2) T ...)))
      ((_ E  (25 S ...) ($2 $1 T ...))	(state-45 E (25 S ...) ((+ $1 . $2) T ...)))
      ((_ E  (24 S ...) ($2 $1 T ...))	(state-44 E (24 S ...) ((+ $1 . $2) T ...)))
      ((_ E  (23 S ...) ($2 $1 T ...))	(state-43 E (23 S ...) ((+ $1 . $2) T ...)))
      ((_ E  (s S ...) ($2 $1 T ...))	(state-7 E (s S ...) ((+ $1 . $2) T ...)))
))

(define-syntax state-9
   (syntax-rules (-)
      ((_ (- E ...) S T)	(state-30 (E ...) (9 . S) T))
      ((_ E  (29 S ...) T)	(state-47 E (29 S ...) T))
      ((_ E  (s S ...) T)	(state-8 E (s S ...) T))
))

(define-syntax state-9-DIFF~4
   (syntax-rules ()
      ((_ E  (29 S ...) ($2 $1 T ...))	(state-47 E (29 S ...) ((- $1 . $2) T ...)))
      ((_ E  (s S ...) ($2 $1 T ...))	(state-8 E (s S ...) ((- $1 . $2) T ...)))
))

(define-syntax state-10
   (syntax-rules (- +)
      ((_ (- E ...) S T)	(state-10 (E ...) (10 . S) T))
      ((_ (+ E ...) S T)	(state-11 (E ...) (10 . S) T))
      ((_ (((x ...)) E ...) S T)	(state-16 (E ...) (10 . S) ((x ...) . T)))
      ((_ ((x ...) E ...) S T)	(state-16 (E ...) (10 . S) ((expr x ...) . T)))
      ((_ (x E ...) S T)	(state-16 (E ...) (10 . S) (x . T)))
      ((_ x ...)	(syntax-error "state-10: invalid expression" x ...))
))

(define-syntax state-10-FACTOR
   (syntax-rules ()
      ((_ E  (10 S ...) ($2 T ...))	(state-10-FACTOR E (S ...) ((- $2) T ...)))
      ((_ E  (11 S ...) ($2 T ...))	(state-11-FACTOR E (S ...) ((- $2) T ...)))
      ((_ E  (30 S ...) ($2 T ...))	(state-48 E (30 S ...) ((- $2) T ...)))
      ((_ E  (s S ...) ($2 T ...))	(state-9 E (s S ...) ((- $2) T ...)))
))

(define-syntax state-11
   (syntax-rules (- +)
      ((_ (- E ...) S T)	(state-10 (E ...) (11 . S) T))
      ((_ (+ E ...) S T)	(state-11 (E ...) (11 . S) T))
      ((_ (((x ...)) E ...) S T)	(state-16 (E ...) (11 . S) ((x ...) . T)))
      ((_ ((x ...) E ...) S T)	(state-16 (E ...) (11 . S) ((expr x ...) . T)))
      ((_ (x E ...) S T)	(state-16 (E ...) (11 . S) (x . T)))
      ((_ x ...)	(syntax-error "state-11: invalid expression" x ...))
))

(define-syntax state-11-FACTOR
   (syntax-rules ()
      ((_ E  (10 S ...) ($2 T ...))	(state-10-FACTOR E (S ...) ($2 T ...)))
      ((_ E  (11 S ...) ($2 T ...))	(state-11-FACTOR E (S ...) ($2 T ...)))
      ((_ E  (30 S ...) ($2 T ...))	(state-48 E (30 S ...) ($2 T ...)))
      ((_ E  (s S ...) ($2 T ...))	(state-9 E (s S ...) ($2 T ...)))
))

(define-syntax state-12
   (syntax-rules (*)
      ((_ (* E ...) S T)	(state-31 (E ...) (12 . S) T))
      ((_ E  (10 S ...) T)	(state-10-FACTOR E (S ...) T))
      ((_ E  (11 S ...) T)	(state-11-FACTOR E (S ...) T))
      ((_ E  (30 S ...) T)	(state-48 E (30 S ...) T))
      ((_ E  (s S ...) T)	(state-9 E (s S ...) T))
))

(define-syntax state-12-MUL~3
   (syntax-rules ()
      ((_ E  (10 S ...) ($2 $1 T ...))	(state-10-FACTOR E (S ...) ((* $1 . $2) T ...)))
      ((_ E  (11 S ...) ($2 $1 T ...))	(state-11-FACTOR E (S ...) ((* $1 . $2) T ...)))
      ((_ E  (30 S ...) ($2 $1 T ...))	(state-48 E (30 S ...) ((* $1 . $2) T ...)))
      ((_ E  (s S ...) ($2 $1 T ...))	(state-9 E (s S ...) ((* $1 . $2) T ...)))
))

(define-syntax state-13
   (syntax-rules (/ :)
      ((_ (/ E ...) S T)	(state-32 (E ...) (13 . S) T))
      ((_ (: E ...) S T)	(state-33 (E ...) (13 . S) T))
      ((_ E  (31 S ...) T)	(state-49 E (31 S ...) T))
      ((_ E  (s S ...) T)	(state-12 E (s S ...) T))
))

(define-syntax state-13-DIV~2
   (syntax-rules ()
      ((_ E  (31 S ...) ($2 $1 T ...))	(state-49 E (31 S ...) ((/ $1 . $2) T ...)))
      ((_ E  (s S ...) ($2 $1 T ...))	(state-12 E (s S ...) ((/ $1 . $2) T ...)))
))

(define-syntax state-14
   (syntax-rules (%)
      ((_ (% E ...) S T)	(state-34 (E ...) (14 . S) T))
      ((_ E  (32 S ...) T)	(state-50 E (32 S ...) T))
      ((_ E  (s S ...) T)	(state-13 E (s S ...) T))
))

(define-syntax state-15
   (syntax-rules (^)
      ((_ (^ E ...) S T)	(state-35 (E ...) (15 . S) T))
      ((_ E  (34 S ...) T)	(state-34-EXP E (S ...) T))
      ((_ E  (33 S ...) T)	(state-51 E (33 S ...) T))
      ((_ E  (s S ...) T)	(state-14 E (s S ...) T))
))

(define-syntax state-16
   (syntax-rules (@ @. @@)
      ((_ (@ E ...) S T)	(state-36 (E ...) (16 . S) T))
      ((_ (@. E ...) S T)	(state-37 (E ...) (16 . S) T))
      ((_ (@@ E ...) S T)	(state-38 (E ...) (16 . S) T))
      ((_ E  (54 S ...) T)	(state-55 E (54 S ...) T))
      ((_ E  (35 S ...) T)	(state-52 E (35 S ...) T))
      ((_ E  (s S ...) T)	(state-15 E (s S ...) T))
))

(define-syntax state-17
   (syntax-rules (not - +)
      ((_ (not E ...) S T)	(state-6 (E ...) (17 . S) T))
      ((_ (- E ...) S T)	(state-10 (E ...) (17 . S) T))
      ((_ (+ E ...) S T)	(state-11 (E ...) (17 . S) T))
      ((_ (((x ...)) E ...) S T)	(state-16 (E ...) (17 . S) ((x ...) . T)))
      ((_ ((x ...) E ...) S T)	(state-16 (E ...) (17 . S) ((expr x ...) . T)))
      ((_ (x E ...) S T)	(state-16 (E ...) (17 . S) (x . T)))
      ((_ x ...)	(syntax-error "state-17: invalid expression" x ...))
))

(define-syntax state-18
   (syntax-rules (not - +)
      ((_ (not E ...) S T)	(state-6 (E ...) (18 . S) T))
      ((_ (- E ...) S T)	(state-10 (E ...) (18 . S) T))
      ((_ (+ E ...) S T)	(state-11 (E ...) (18 . S) T))
      ((_ (((x ...)) E ...) S T)	(state-16 (E ...) (18 . S) ((x ...) . T)))
      ((_ ((x ...) E ...) S T)	(state-16 (E ...) (18 . S) ((expr x ...) . T)))
      ((_ (x E ...) S T)	(state-16 (E ...) (18 . S) (x . T)))
      ((_ x ...)	(syntax-error "state-18: invalid expression" x ...))
))

(define-syntax state-19
   (syntax-rules (not - +)
      ((_ (not E ...) S T)	(state-6 (E ...) (19 . S) T))
      ((_ (- E ...) S T)	(state-10 (E ...) (19 . S) T))
      ((_ (+ E ...) S T)	(state-11 (E ...) (19 . S) T))
      ((_ (((x ...)) E ...) S T)	(state-16 (E ...) (19 . S) ((x ...) . T)))
      ((_ ((x ...) E ...) S T)	(state-16 (E ...) (19 . S) ((expr x ...) . T)))
      ((_ (x E ...) S T)	(state-16 (E ...) (19 . S) (x . T)))
      ((_ x ...)	(syntax-error "state-19: invalid expression" x ...))
))

(define-syntax state-20
   (syntax-rules (not - +)
      ((_ (not E ...) S T)	(state-6 (E ...) (20 . S) T))
      ((_ (- E ...) S T)	(state-10 (E ...) (20 . S) T))
      ((_ (+ E ...) S T)	(state-11 (E ...) (20 . S) T))
      ((_ (((x ...)) E ...) S T)	(state-16 (E ...) (20 . S) ((x ...) . T)))
      ((_ ((x ...) E ...) S T)	(state-16 (E ...) (20 . S) ((expr x ...) . T)))
      ((_ (x E ...) S T)	(state-16 (E ...) (20 . S) (x . T)))
      ((_ x ...)	(syntax-error "state-20: invalid expression" x ...))
))

(define-syntax state-20-EQV
   (syntax-rules ()
      ((_ E  (_ 19 S ...) ($3 $1 T ...))	(state-41 E (19 S ...) ((or (not $1) $3) T ...)))
      ((_ E  (_ s S ...) ($3 $1 T ...))	(state-3 E (s S ...) ((or (not $1) $3) T ...)))
))

(define-syntax state-21
   (syntax-rules (eqv)
      ((_ (eqv E ...) S T)	(state-42 (E ...) (21 . S) T))
      ((_ x ...)	(syntax-error "state-21: invalid expression" x ...))
))

(define-syntax state-22
   (syntax-rules (not - +)
      ((_ (not E ...) S T)	(state-6 (E ...) (22 . S) T))
      ((_ (- E ...) S T)	(state-10 (E ...) (22 . S) T))
      ((_ (+ E ...) S T)	(state-11 (E ...) (22 . S) T))
      ((_ (((x ...)) E ...) S T)	(state-16 (E ...) (22 . S) ((x ...) . T)))
      ((_ ((x ...) E ...) S T)	(state-16 (E ...) (22 . S) ((expr x ...) . T)))
      ((_ (x E ...) S T)	(state-16 (E ...) (22 . S) (x . T)))
      ((_ x ...)	(syntax-error "state-22: invalid expression" x ...))
))

(define-syntax state-22-NOT
   (syntax-rules ()
      ((_ E  (_ 20 S ...) ($3 $1 T ...))	(state-20-EQV E (S ...) ((eqv? $1 $3) T ...)))
      ((_ E  (_ s S ...) ($3 $1 T ...))	(state-4 E (s S ...) ((eqv? $1 $3) T ...)))
))

(define-syntax state-23
   (syntax-rules (- +)
      ((_ (- E ...) S T)	(state-10 (E ...) (23 . S) T))
      ((_ (+ E ...) S T)	(state-11 (E ...) (23 . S) T))
      ((_ (((x ...)) E ...) S T)	(state-16 (E ...) (23 . S) ((x ...) . T)))
      ((_ ((x ...) E ...) S T)	(state-16 (E ...) (23 . S) ((expr x ...) . T)))
      ((_ (x E ...) S T)	(state-16 (E ...) (23 . S) (x . T)))
      ((_ x ...)	(syntax-error "state-23: invalid expression" x ...))
))

(define-syntax state-24
   (syntax-rules (- +)
      ((_ (- E ...) S T)	(state-10 (E ...) (24 . S) T))
      ((_ (+ E ...) S T)	(state-11 (E ...) (24 . S) T))
      ((_ (((x ...)) E ...) S T)	(state-16 (E ...) (24 . S) ((x ...) . T)))
      ((_ ((x ...) E ...) S T)	(state-16 (E ...) (24 . S) ((expr x ...) . T)))
      ((_ (x E ...) S T)	(state-16 (E ...) (24 . S) (x . T)))
      ((_ x ...)	(syntax-error "state-24: invalid expression" x ...))
))

(define-syntax state-25
   (syntax-rules (- +)
      ((_ (- E ...) S T)	(state-10 (E ...) (25 . S) T))
      ((_ (+ E ...) S T)	(state-11 (E ...) (25 . S) T))
      ((_ (((x ...)) E ...) S T)	(state-16 (E ...) (25 . S) ((x ...) . T)))
      ((_ ((x ...) E ...) S T)	(state-16 (E ...) (25 . S) ((expr x ...) . T)))
      ((_ (x E ...) S T)	(state-16 (E ...) (25 . S) (x . T)))
      ((_ x ...)	(syntax-error "state-25: invalid expression" x ...))
))

(define-syntax state-26
   (syntax-rules (- +)
      ((_ (- E ...) S T)	(state-10 (E ...) (26 . S) T))
      ((_ (+ E ...) S T)	(state-11 (E ...) (26 . S) T))
      ((_ (((x ...)) E ...) S T)	(state-16 (E ...) (26 . S) ((x ...) . T)))
      ((_ ((x ...) E ...) S T)	(state-16 (E ...) (26 . S) ((expr x ...) . T)))
      ((_ (x E ...) S T)	(state-16 (E ...) (26 . S) (x . T)))
      ((_ x ...)	(syntax-error "state-26: invalid expression" x ...))
))

(define-syntax state-27
   (syntax-rules (- +)
      ((_ (- E ...) S T)	(state-10 (E ...) (27 . S) T))
      ((_ (+ E ...) S T)	(state-11 (E ...) (27 . S) T))
      ((_ (((x ...)) E ...) S T)	(state-16 (E ...) (27 . S) ((x ...) . T)))
      ((_ ((x ...) E ...) S T)	(state-16 (E ...) (27 . S) ((expr x ...) . T)))
      ((_ (x E ...) S T)	(state-16 (E ...) (27 . S) (x . T)))
      ((_ x ...)	(syntax-error "state-27: invalid expression" x ...))
))

(define-syntax state-27-SUM
   (syntax-rules ()
      ((_ E  (_ 6 S ...) ($3 $1 T ...))	(state-6-NOT E (S ...) ((= $1 $3) T ...)))
      ((_ E  (_ 22 S ...) ($3 $1 T ...))	(state-22-NOT E (S ...) ((= $1 $3) T ...)))
      ((_ E  (_ 42 S ...) ($3 $1 T ...))	(state-42-NOT E (S ...) ((= $1 $3) T ...)))
      ((_ E  (_ s S ...) ($3 $1 T ...))	(state-5 E (s S ...) ((= $1 $3) T ...)))
))

(define-syntax state-28
   (syntax-rules (- +)
      ((_ (- E ...) S T)	(state-10 (E ...) (28 . S) T))
      ((_ (+ E ...) S T)	(state-11 (E ...) (28 . S) T))
      ((_ (((x ...)) E ...) S T)	(state-16 (E ...) (28 . S) ((x ...) . T)))
      ((_ ((x ...) E ...) S T)	(state-16 (E ...) (28 . S) ((expr x ...) . T)))
      ((_ (x E ...) S T)	(state-16 (E ...) (28 . S) (x . T)))
      ((_ x ...)	(syntax-error "state-28: invalid expression" x ...))
))

(define-syntax state-28-SUM
   (syntax-rules ()
      ((_ E  (_ 6 S ...) ($3 $1 T ...))	(state-6-NOT E (S ...) ((not (= $1 $3)) T ...)))
      ((_ E  (_ 22 S ...) ($3 $1 T ...))	(state-22-NOT E (S ...) ((not (= $1 $3)) T ...)))
      ((_ E  (_ 42 S ...) ($3 $1 T ...))	(state-42-NOT E (S ...) ((not (= $1 $3)) T ...)))
      ((_ E  (_ s S ...) ($3 $1 T ...))	(state-5 E (s S ...) ((not (= $1 $3)) T ...)))
))

(define-syntax state-29
   (syntax-rules (- +)
      ((_ (- E ...) S T)	(state-10 (E ...) (29 . S) T))
      ((_ (+ E ...) S T)	(state-11 (E ...) (29 . S) T))
      ((_ (((x ...)) E ...) S T)	(state-16 (E ...) (29 . S) ((x ...) . T)))
      ((_ ((x ...) E ...) S T)	(state-16 (E ...) (29 . S) ((expr x ...) . T)))
      ((_ (x E ...) S T)	(state-16 (E ...) (29 . S) (x . T)))
      ((_ x ...)	(syntax-error "state-29: invalid expression" x ...))
))

(define-syntax state-30
   (syntax-rules (- +)
      ((_ (- E ...) S T)	(state-10 (E ...) (30 . S) T))
      ((_ (+ E ...) S T)	(state-11 (E ...) (30 . S) T))
      ((_ (((x ...)) E ...) S T)	(state-16 (E ...) (30 . S) ((x ...) . T)))
      ((_ ((x ...) E ...) S T)	(state-16 (E ...) (30 . S) ((expr x ...) . T)))
      ((_ (x E ...) S T)	(state-16 (E ...) (30 . S) (x . T)))
      ((_ x ...)	(syntax-error "state-30: invalid expression" x ...))
))

(define-syntax state-31
   (syntax-rules ()
      ((_ (((x ...)) E ...) S T)	(state-16 (E ...) (31 . S) ((x ...) . T)))
      ((_ ((x ...) E ...) S T)	(state-16 (E ...) (31 . S) ((expr x ...) . T)))
      ((_ (x E ...) S T)	(state-16 (E ...) (31 . S) (x . T)))
      ((_ x ...)	(syntax-error "state-31: invalid expression" x ...))
))

(define-syntax state-32
   (syntax-rules ()
      ((_ (((x ...)) E ...) S T)	(state-16 (E ...) (32 . S) ((x ...) . T)))
      ((_ ((x ...) E ...) S T)	(state-16 (E ...) (32 . S) ((expr x ...) . T)))
      ((_ (x E ...) S T)	(state-16 (E ...) (32 . S) (x . T)))
      ((_ x ...)	(syntax-error "state-32: invalid expression" x ...))
))

(define-syntax state-33
   (syntax-rules ()
      ((_ (((x ...)) E ...) S T)	(state-16 (E ...) (33 . S) ((x ...) . T)))
      ((_ ((x ...) E ...) S T)	(state-16 (E ...) (33 . S) ((expr x ...) . T)))
      ((_ (x E ...) S T)	(state-16 (E ...) (33 . S) (x . T)))
      ((_ x ...)	(syntax-error "state-33: invalid expression" x ...))
))

(define-syntax state-34
   (syntax-rules ()
      ((_ (((x ...)) E ...) S T)	(state-16 (E ...) (34 . S) ((x ...) . T)))
      ((_ ((x ...) E ...) S T)	(state-16 (E ...) (34 . S) ((expr x ...) . T)))
      ((_ (x E ...) S T)	(state-16 (E ...) (34 . S) (x . T)))
      ((_ x ...)	(syntax-error "state-34: invalid expression" x ...))
))

(define-syntax state-34-EXP
   (syntax-rules ()
      ((_ E  (_ 33 S ...) ($3 $1 T ...))	(state-51 E (33 S ...) ((remainder $1 $3) T ...)))
      ((_ E  (_ s S ...) ($3 $1 T ...))	(state-14 E (s S ...) ((remainder $1 $3) T ...)))
))

(define-syntax state-35
   (syntax-rules ()
      ((_ (((x ...)) E ...) S T)	(state-16 (E ...) (35 . S) ((x ...) . T)))
      ((_ ((x ...) E ...) S T)	(state-16 (E ...) (35 . S) ((expr x ...) . T)))
      ((_ (x E ...) S T)	(state-16 (E ...) (35 . S) (x . T)))
      ((_ x ...)	(syntax-error "state-35: invalid expression" x ...))
))

(define-syntax state-36
   (syntax-rules ()
      ((_ (((x ...)) E ...) S T)	(state-36-ITEM (E ...) S ((x ...) . T)))
      ((_ ((x ...) E ...) S T)	(state-36-ITEM (E ...) S ((expr x ...) . T)))
      ((_ (x E ...) S T)	(state-36-ITEM (E ...) S (x . T)))
      ((_ x ...)	(syntax-error "state-36: invalid expression" x ...))
))

(define-syntax state-36-ITEM
   (syntax-rules ()
      ((_ E  (_ 54 S ...) ($3 $1 T ...))	(state-55 E (54 S ...) ((vector-ref $1 $3) T ...)))
      ((_ E  (_ 35 S ...) ($3 $1 T ...))	(state-52 E (35 S ...) ((vector-ref $1 $3) T ...)))
      ((_ E  (_ s S ...) ($3 $1 T ...))	(state-15 E (s S ...) ((vector-ref $1 $3) T ...)))
))

(define-syntax state-37
   (syntax-rules ()
      ((_ (((x ...)) E ...) S T)	(state-37-ITEM (E ...) S ((x ...) . T)))
      ((_ ((x ...) E ...) S T)	(state-37-ITEM (E ...) S ((expr x ...) . T)))
      ((_ (x E ...) S T)	(state-37-ITEM (E ...) S (x . T)))
      ((_ x ...)	(syntax-error "state-37: invalid expression" x ...))
))

(define-syntax state-37-ITEM
   (syntax-rules ()
      ((_ E  (_ 54 S ...) ($3 $1 T ...))	(state-55 E (54 S ...) ((list-ref $1 $3) T ...)))
      ((_ E  (_ 35 S ...) ($3 $1 T ...))	(state-52 E (35 S ...) ((list-ref $1 $3) T ...)))
      ((_ E  (_ s S ...) ($3 $1 T ...))	(state-15 E (s S ...) ((list-ref $1 $3) T ...)))
))

(define-syntax state-38
   (syntax-rules ()
      ((_ (((x ...)) E ...) S T)	(state-38-ITEM (E ...) S ((x ...) . T)))
      ((_ ((x ...) E ...) S T)	(state-38-ITEM (E ...) S ((expr x ...) . T)))
      ((_ (x E ...) S T)	(state-38-ITEM (E ...) S (x . T)))
      ((_ x ...)	(syntax-error "state-38: invalid expression" x ...))
))

(define-syntax state-38-ITEM
   (syntax-rules ()
      ((_ E  (_ 54 S ...) ($3 $1 T ...))	(state-55 E (54 S ...) ((bytevector-u8-ref $1 $3) T ...)))
      ((_ E  (_ 35 S ...) ($3 $1 T ...))	(state-52 E (35 S ...) ((bytevector-u8-ref $1 $3) T ...)))
      ((_ E  (_ s S ...) ($3 $1 T ...))	(state-15 E (s S ...) ((bytevector-u8-ref $1 $3) T ...)))
))

(define-syntax state-39
   (syntax-rules (else)
      ((_ (else E ...) S T)	(state-53 (E ...) (39 . S) T))
      ((_ x ...)	(syntax-error "state-39: invalid expression" x ...))
))

(define-syntax state-40
   (syntax-rules (or)
      ((_ (or E ...) S T)	(state-18 (E ...) (40 . S) T))
      ((_ E  (_ 2 S ...) ($2 T ...))	(state-2-OR~11 E (S ...) (($2) T ...)))
      ((_ E  (_ 40 S ...) ($2 T ...))	(state-40-OR~11 E (S ...) (($2) T ...)))
))

(define-syntax state-40-OR~11
   (syntax-rules ()
      ((_ E  (_ 2 S ...) ($3 $2 T ...))	(state-2-OR~11 E (S ...) (($2 . $3) T ...)))
      ((_ E  (_ 40 S ...) ($3 $2 T ...))	(state-40-OR~11 E (S ...) (($2 . $3) T ...)))
))

(define-syntax state-41
   (syntax-rules (and)
      ((_ (and E ...) S T)	(state-19 (E ...) (41 . S) T))
      ((_ E  (_ 3 S ...) ($2 T ...))	(state-3-AND~10 E (S ...) (($2) T ...)))
      ((_ E  (_ 41 S ...) ($2 T ...))	(state-41-AND~10 E (S ...) (($2) T ...)))
))

(define-syntax state-41-AND~10
   (syntax-rules ()
      ((_ E  (_ 3 S ...) ($3 $2 T ...))	(state-3-AND~10 E (S ...) (($2 . $3) T ...)))
      ((_ E  (_ 41 S ...) ($3 $2 T ...))	(state-41-AND~10 E (S ...) (($2 . $3) T ...)))
))

(define-syntax state-42
   (syntax-rules (not - +)
      ((_ (not E ...) S T)	(state-6 (E ...) (42 . S) T))
      ((_ (- E ...) S T)	(state-10 (E ...) (42 . S) T))
      ((_ (+ E ...) S T)	(state-11 (E ...) (42 . S) T))
      ((_ (((x ...)) E ...) S T)	(state-16 (E ...) (42 . S) ((x ...) . T)))
      ((_ ((x ...) E ...) S T)	(state-16 (E ...) (42 . S) ((expr x ...) . T)))
      ((_ (x E ...) S T)	(state-16 (E ...) (42 . S) (x . T)))
      ((_ x ...)	(syntax-error "state-42: invalid expression" x ...))
))

(define-syntax state-42-NOT
   (syntax-rules ()
      ((_ E  (_ _ 20 S ...) ($4 $1 T ...))	(state-20-EQV E (S ...) ((not (eqv? $1 $4)) T ...)))
      ((_ E  (_ _ s S ...) ($4 $1 T ...))	(state-4 E (s S ...) ((not (eqv? $1 $4)) T ...)))
))

(define-syntax state-43
   (syntax-rules (<)
      ((_ (< E ...) S T)	(state-23 (E ...) (43 . S) T))
      ((_ E  (_ 7 S ...) ($2 T ...))	(state-7-COMP~9 E (S ...) (($2) T ...)))
      ((_ E  (_ 43 S ...) ($2 T ...))	(state-43-COMP~9 E (S ...) (($2) T ...)))
))

(define-syntax state-43-COMP~9
   (syntax-rules ()
      ((_ E  (_ 7 S ...) ($3 $2 T ...))	(state-7-COMP~9 E (S ...) (($2 . $3) T ...)))
      ((_ E  (_ 43 S ...) ($3 $2 T ...))	(state-43-COMP~9 E (S ...) (($2 . $3) T ...)))
))

(define-syntax state-44
   (syntax-rules (<=)
      ((_ (<= E ...) S T)	(state-24 (E ...) (44 . S) T))
      ((_ E  (_ 7 S ...) ($2 T ...))	(state-7-COMP~8 E (S ...) (($2) T ...)))
      ((_ E  (_ 44 S ...) ($2 T ...))	(state-44-COMP~8 E (S ...) (($2) T ...)))
))

(define-syntax state-44-COMP~8
   (syntax-rules ()
      ((_ E  (_ 7 S ...) ($3 $2 T ...))	(state-7-COMP~8 E (S ...) (($2 . $3) T ...)))
      ((_ E  (_ 44 S ...) ($3 $2 T ...))	(state-44-COMP~8 E (S ...) (($2 . $3) T ...)))
))

(define-syntax state-45
   (syntax-rules (>)
      ((_ (> E ...) S T)	(state-25 (E ...) (45 . S) T))
      ((_ E  (_ 7 S ...) ($2 T ...))	(state-7-COMP~7 E (S ...) (($2) T ...)))
      ((_ E  (_ 45 S ...) ($2 T ...))	(state-45-COMP~7 E (S ...) (($2) T ...)))
))

(define-syntax state-45-COMP~7
   (syntax-rules ()
      ((_ E  (_ 7 S ...) ($3 $2 T ...))	(state-7-COMP~7 E (S ...) (($2 . $3) T ...)))
      ((_ E  (_ 45 S ...) ($3 $2 T ...))	(state-45-COMP~7 E (S ...) (($2 . $3) T ...)))
))

(define-syntax state-46
   (syntax-rules (>=)
      ((_ (>= E ...) S T)	(state-26 (E ...) (46 . S) T))
      ((_ E  (_ 7 S ...) ($2 T ...))	(state-7-COMP~6 E (S ...) (($2) T ...)))
      ((_ E  (_ 46 S ...) ($2 T ...))	(state-46-COMP~6 E (S ...) (($2) T ...)))
))

(define-syntax state-46-COMP~6
   (syntax-rules ()
      ((_ E  (_ 7 S ...) ($3 $2 T ...))	(state-7-COMP~6 E (S ...) (($2 . $3) T ...)))
      ((_ E  (_ 46 S ...) ($3 $2 T ...))	(state-46-COMP~6 E (S ...) (($2 . $3) T ...)))
))

(define-syntax state-47
   (syntax-rules (+)
      ((_ (+ E ...) S T)	(state-29 (E ...) (47 . S) T))
      ((_ E  (_ 8 S ...) ($2 T ...))	(state-8-SUM~5 E (S ...) (($2) T ...)))
      ((_ E  (_ 47 S ...) ($2 T ...))	(state-47-SUM~5 E (S ...) (($2) T ...)))
))

(define-syntax state-47-SUM~5
   (syntax-rules ()
      ((_ E  (_ 8 S ...) ($3 $2 T ...))	(state-8-SUM~5 E (S ...) (($2 . $3) T ...)))
      ((_ E  (_ 47 S ...) ($3 $2 T ...))	(state-47-SUM~5 E (S ...) (($2 . $3) T ...)))
))

(define-syntax state-48
   (syntax-rules (-)
      ((_ (- E ...) S T)	(state-30 (E ...) (48 . S) T))
      ((_ E  (_ 9 S ...) ($2 T ...))	(state-9-DIFF~4 E (S ...) (($2) T ...)))
      ((_ E  (_ 48 S ...) ($2 T ...))	(state-48-DIFF~4 E (S ...) (($2) T ...)))
))

(define-syntax state-48-DIFF~4
   (syntax-rules ()
      ((_ E  (_ 9 S ...) ($3 $2 T ...))	(state-9-DIFF~4 E (S ...) (($2 . $3) T ...)))
      ((_ E  (_ 48 S ...) ($3 $2 T ...))	(state-48-DIFF~4 E (S ...) (($2 . $3) T ...)))
))

(define-syntax state-49
   (syntax-rules (*)
      ((_ (* E ...) S T)	(state-31 (E ...) (49 . S) T))
      ((_ E  (_ 12 S ...) ($2 T ...))	(state-12-MUL~3 E (S ...) (($2) T ...)))
      ((_ E  (_ 49 S ...) ($2 T ...))	(state-49-MUL~3 E (S ...) (($2) T ...)))
))

(define-syntax state-49-MUL~3
   (syntax-rules ()
      ((_ E  (_ 12 S ...) ($3 $2 T ...))	(state-12-MUL~3 E (S ...) (($2 . $3) T ...)))
      ((_ E  (_ 49 S ...) ($3 $2 T ...))	(state-49-MUL~3 E (S ...) (($2 . $3) T ...)))
))

(define-syntax state-50
   (syntax-rules (/ :)
      ((_ (/ E ...) S T)	(state-32 (E ...) (50 . S) T))
      ((_ (: E ...) S T)	(state-33 (E ...) (50 . S) T))
      ((_ E  (_ 13 S ...) ($2 T ...))	(state-13-DIV~2 E (S ...) (($2) T ...)))
      ((_ E  (_ 50 S ...) ($2 T ...))	(state-50-DIV~2 E (S ...) (($2) T ...)))
))

(define-syntax state-50-DIV~2
   (syntax-rules ()
      ((_ E  (_ 13 S ...) ($3 $2 T ...))	(state-13-DIV~2 E (S ...) (($2 . $3) T ...)))
      ((_ E  (_ 50 S ...) ($3 $2 T ...))	(state-50-DIV~2 E (S ...) (($2 . $3) T ...)))
))

(define-syntax state-51
   (syntax-rules (%)
      ((_ (% E ...) S T)	(state-34 (E ...) (51 . S) T))
      ((_ E  (_ _ 32 S ...) ($3 $1 T ...))	(state-50 E (32 S ...) ((quotient $1 $3) T ...)))
      ((_ E  (_ _ s S ...) ($3 $1 T ...))	(state-13 E (s S ...) ((quotient $1 $3) T ...)))
))

(define-syntax state-52
   (syntax-rules (^)
      ((_ (^ E ...) S T)	(state-54 (E ...) (52 . S) T))
      ((_ E  (_ _ 34 S ...) ($3 $1 T ...))	(state-34-EXP E (S ...) ((expt $1 $3) T ...)))
      ((_ E  (_ _ 33 S ...) ($3 $1 T ...))	(state-51 E (33 S ...) ((expt $1 $3) T ...)))
      ((_ E  (_ _ s S ...) ($3 $1 T ...))	(state-14 E (s S ...) ((expt $1 $3) T ...)))
))

(define-syntax state-52-EXP~1
   (syntax-rules ()
      ((_ E  (_ _ 34 S ...) ($4 $3 $1 T ...))	(state-34-EXP E (S ...) ((expt $1 (* $3 . $4)) T ...)))
      ((_ E  (_ _ 33 S ...) ($4 $3 $1 T ...))	(state-51 E (33 S ...) ((expt $1 (* $3 . $4)) T ...)))
      ((_ E  (_ _ s S ...) ($4 $3 $1 T ...))	(state-14 E (s S ...) ((expt $1 (* $3 . $4)) T ...)))
))

(define-syntax state-53
   (syntax-rules (not - +)
      ((_ (not E ...) S T)	(state-6 (E ...) (53 . S) T))
      ((_ (- E ...) S T)	(state-10 (E ...) (53 . S) T))
      ((_ (+ E ...) S T)	(state-11 (E ...) (53 . S) T))
      ((_ (((x ...)) E ...) S T)	(state-16 (E ...) (53 . S) ((x ...) . T)))
      ((_ ((x ...) E ...) S T)	(state-16 (E ...) (53 . S) ((expr x ...) . T)))
      ((_ (x E ...) S T)	(state-16 (E ...) (53 . S) (x . T)))
      ((_ x ...)	(syntax-error "state-53: invalid expression" x ...))
))

(define-syntax state-53-OR
   (syntax-rules ()
      ((_ E  (_ _ _ 0 S ...) ($5 $3 $1 T ...))	(state-0-EXPR E (S ...) ((if $3 $1 $5) T ...)))
))

(define-syntax state-54
   (syntax-rules ()
      ((_ (((x ...)) E ...) S T)	(state-16 (E ...) (54 . S) ((x ...) . T)))
      ((_ ((x ...) E ...) S T)	(state-16 (E ...) (54 . S) ((expr x ...) . T)))
      ((_ (x E ...) S T)	(state-16 (E ...) (54 . S) (x . T)))
      ((_ x ...)	(syntax-error "state-54: invalid expression" x ...))
))

(define-syntax state-55
   (syntax-rules (^)
      ((_ (^ E ...) S T)	(state-54 (E ...) (55 . S) T))
      ((_ E  (_ 52 S ...) ($2 T ...))	(state-52-EXP~1 E (S ...) (($2) T ...)))
      ((_ E  (_ 55 S ...) ($2 T ...))	(state-55-EXP~1 E (S ...) (($2) T ...)))
))

(define-syntax state-55-EXP~1
   (syntax-rules ()
      ((_ E  (_ 52 S ...) ($3 $2 T ...))	(state-52-EXP~1 E (S ...) (($2 . $3) T ...)))
      ((_ E  (_ 55 S ...) ($3 $2 T ...))	(state-55-EXP~1 E (S ...) (($2 . $3) T ...)))
))


