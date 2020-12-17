(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (cons-all first rests)
  (if (null? rests) nil
  (cons (cons first (car rests)) (cons-all first (cdr rests)))))

(define (zip pairs)
  (cons (map car pairs) (cons (map cadr pairs) nil)))

;; Returns a list of two-element lists
(define (enumerate s)
  (define (enum-helper i lst s)
   (cond ((null? s) lst)
    (else
     (enum-helper (+ i 1) (append lst (list (cons i (list (car s))))) (cdr s)))))
  (enum-helper 0 nil s))

;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  (cond
    ((null? denoms) nil)
    ((< total (car denoms)) (list-change total (cdr denoms)))
    ((= total (car denoms)) (cons (cons (car denoms) nil) (list-change total (cdr denoms))))
    (else (append
            (cons-all (car denoms) (list-change (- total (car denoms)) denoms))
            (list-change total (cdr denoms))))))

;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         expr
         )
        ((quoted? expr)
         expr
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           (cons form (cons params (map let-to-lambda body)))
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           (cons
             (cons 'lambda (cons (car (zip values)) (map let-to-lambda body)))
             (map let-to-lambda (cadr (zip values))))
           ))
        (else
         (cons (car expr) (map let-to-lambda (cdr expr)))
         )))
