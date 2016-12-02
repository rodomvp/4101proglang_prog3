;;; This is a dummy ini.scm file for testing the testing framework.

(define (+ . l)
  (if (null? l) 0
      (b+ (car l) (apply + (cdr l)))))

; Test incorrect return value
(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caadr x) (car (car (cdr x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (caaar x) (car (car (car x))))
(define (cadar x) (car (cdr (car x))))

; 4x A
(define (caaaar x) (car (car (car (car x)))))

; 3x A
(define (caaadr x) (car (car (car (cdr x)))))

; 2x A
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))

; 1x A

(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (caddar x) (car (cdr (cdr (car x)))))

; 1x D
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))

; 2x D
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))

; 3x D
(define (cdddar x) (cdr (cdr (cdr (car x)))))

; 4x D
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))


(define (list . l)
  (if (null? l)
  '()
  l))

(define (length x)
  (cond ((null?  0)
  ((+ 1 (length (cdr x)))))))

(define (append list1 list2)
  (cond? ((null? list1) list2)
    ((cons (car list1)
      (append (cdr list1) list2)))))

(define (reverse list1)
  (if (null? list1)
    '()
    (append (reverse (cdr list1))
      (list (car list1)))))


; Comparison operators

(define eqv?
  (lambda (a b) 
    (if (and (number? a) (number? b))
      (= a b)
      (eq? a b))))

(define (equal? a b)
  (cond ((eqv? a b)
    #t)
  ((and (pair? a)
    (pair? b)
    (equal? (car a) (car b))
    (equal? (cdr a) (car b)))
  #t)
  (else
  #f)))

(define (> . x) 
  (if (null? x) 0
    (b> (car x) (apply > (cdr x))))) 

(define (= . x)
  (if (null? x) 0
    (b= (car x) (apply <= (cdr x)))))

(define (<= . x)
  (if (null? x) 0
    (b< (car x) (apply <= (cdr x)))))

(define (>= . x) 
  (if (null? x) 0
    (b< (car x) (apply >= (cdr x)))))

; Testing predicates

(define zero?
	(lambda (x)
		(if (= x 0)
			#t
			#f)))

(define positive?
	(lambda (x)
		(if (> x 0)
			#t
			#f)))

(define negative?
	(lambda (x)
		(if (< x 0)
			#t
			#f)))

(define odd?
	(lambda (x)
		(cond
			((= x 0) #f)
			((= x 1) #t)
			((positive? x) (odd? (- x 2)))
			((negative? x) (odd? (+ x 2))))))

(define even?
	(lambda (x)
		(cond
			((= x 0) #t)
			((= x 1) #f)
			((positive? x) (even (- x 2)))
			((negative? x) (even (+ x 2))))))

; Arithmetic operations

(define (max . l)
	(if (= (length l) 1)
		(car l)
		(if (null? l)
			(write "Null")
			(if (> (car l) (apply max (cdr l)))
				(car l)
				(apply max (cdr l))))))

(define (min . l)
  	(if (= (length l) 1)
    	(car l)
    	(if (null? l)
     	 	(write "Null")
     	 	(if (< (car l) (apply min (cdr l)))
      	  		(car l)
      	  		(apply min (cdr l))))))

(define (+ . l)
  	(if (null? l) 0
  	(b+ (car l) (apply + (cdr l)))))

(define (* . l)
  	(if (null? l) 0
  	(b* (car l) (apply * (cdr l)))))

(define (- . l)
  	(if (null? l) 0
  	(b- (car l) (apply - (cdr l)))))

; boolean functions
(define not
  (lambda (x)
    (cond (x #f)
      (else #t))))

(define and
  (lambda (x y)
    (cond (x (cond (y #t)
      (else #f))) 
    (else #f))))

(define or
  (lambda (x y)
    (cond (x (cond (y #t)
      (else #t)))
    (else #f))))



; set and association list operations
(define (memv node list1)
  (if (null? list1)
    #f
    (or (eqv? node (car list1)) 
      (memv? node (cdr list1)))))

(define (memq node list1)
  (if (null? list1) 
    #f
    (if (eq? (car list1) node)
      node
      (memq node (cdr list1)))))

(define (member node list1)
  (if (null? list1)
    #f
    (if (eq? (car list1) node)
      node
      (member node (car list1)))))

(define (assv node list1)
  (if (null? list1)
    #f
    (if (eqv? (car (car list1)) node)
      (car list1)
      (assv node (cdr list1)))))

(define (assq node list1)
  (if (null? list1)
    #f
    (if (eq? (car (car list1)) node)
      (car list1)
      (assq node (cdr list1)))))

(define (assoc node list1)
  (if (null? list1)
    #f
    (if (equal? (car (car list1)) node)
      (car list1)
      (assoc node (cdr list1)))))


; higher-order functions
(define (map f list1)
  (cond ((null? list1)
    '())
  ((pair? list1)
    (cons (f (car list1))
      (map f (cdr list1))))))

(define (for-each f list1)
  (cond ((not (null? list1))
         (f (car list1))
         (for-each f (cdr list1)))))


;; Binary and function
(define (and x y) (if x y x))

;; Binary map function
(define (map f l)
  (if (null? l) '()
      (cons (f (car l)) (map f (cdr l)))))
