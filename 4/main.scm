(define (to-list num)
  (map char->integer (string->list (number->string num))))

(define (in-order? num)
  (cond
    ((null? (cdr num)) #t)
    ((> (car num) (cadr num)) #f)
    (else (in-order? (cdr num)))))

(define (delete-duplicates l)
  (cond ((null? l)
         '())
        ((member (car l) (cdr l))
         (delete-duplicates (cdr l)))
        (else
          (cons (car l) (delete-duplicates (cdr l))))))

(define (count what ls)
  (cond
    ((null? ls) 0)
    ((eq? what (car ls))
     (+ 1 (count what (cdr ls))))
    (else (count what (cdr ls)))))

(define (count-all ls)
  (cond
    ((null? ls) '())
    (else
      (cons (count (car ls) ls) (count-all (delete (car ls) ls))))))

(define (repeat? num)
  (if
    (= (length num) (length (delete-duplicates num)))
    #f
    #t))

(define (password1? num)
  (if
    (and (in-order? num) (repeat? num))
    #t
    #f))

(define (password2? num)
  (if
    (and (in-order? num) (member 2 (count-all num)))
    #t
    #f))

(define (solve-1 lower upper)
  (let loop ([i upper][pass 0])
    (cond
      ((= i lower) pass)
      ((password1? (to-list i))
       (loop (- i 1) (+ pass 1)))
      (else (loop (- i 1) pass)))))

(define (solve-2 lower upper)
  (let loop ([i upper][pass 0])
    (cond
      ((= i lower) pass)
      ((password2? (to-list i))
       (loop (- i 1) (+ pass 1)))
      (else (loop (- i 1) pass)))))
