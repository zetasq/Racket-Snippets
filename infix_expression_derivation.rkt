#lang racket

(define operators '(+ * **))

(define (op-precedence op)
  (define (iter remain-ops current-idx)
    (if (empty? remain-ops)
        -1
        (if (eq? op (car remain-ops))
            current-idx
            (iter (cdr remain-ops) (add1 current-idx)))))
  (iter operators 0))

(define (op? op)
  (>= (op-precedence op) 0))

(define (op-precedence> op1 op2)
  (> (op-precedence op1) (op-precedence op2)))

(define (lowest-precedence-op exp)
  (define (iter sub-exp current-op)
    (if (empty? sub-exp)
        current-op
        (if (null? current-op)
            (if (op? (car sub-exp))
                (iter (cdr sub-exp) (car sub-exp))
                (iter (cdr sub-exp) null))
            (if (and (op? (car sub-exp))
                     (op-precedence> current-op (car sub-exp)))
                (iter (cdr sub-exp) (car sub-exp))
                (iter (cdr sub-exp) current-op)))))
  (iter exp null))
 

;;; result: (lowest-precedence-op left-exp right-exp)
(define (exp-cut exp)
  (let ((base-op (lowest-precedence-op exp)))
    (define (iter remain-exp left-exp right-exp met-base-op)
      (cond ((empty? remain-exp)
             (list base-op left-exp right-exp))
            (met-base-op
             (iter (cdr remain-exp)
                   left-exp
                   (append right-exp
                           (list (car remain-exp)))
                   #t))
            ((eq? (car remain-exp) base-op)
             (iter (cdr remain-exp) left-exp right-exp #t))
            (else
             (iter (cdr remain-exp)
                   (append left-exp (list (car remain-exp)))
                   right-exp
                   #f))))
    (iter exp null null #f)))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (sum? x)
  (eq? (lowest-precedence-op x) '+))

(define (addend s)
  (cadr (exp-cut s)))

(define (augend s)
  (caddr (exp-cut s)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (product? x)
  (eq? (lowest-precedence-op x) '*))

(define (multiplier p)
  (cadr (exp-cut p)))

(define (multiplicand p)
  (caddr (exp-cut p)))

(define (make-exponent base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))

(define (exponent? x)
  (eq? (lowest-precedence-op x) '**))

(define (base x)
  (cadr (exp-cut x)))

(define (exponent x)
  (caddr (exp-cut x)))

(define (deriv exp var)
  (cond ((and (list? exp) (= (length exp) 1))
         (deriv (car exp) var))
        ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponent? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponent (base exp)
                                                    (- (exponent exp) 1)))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

