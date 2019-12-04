(use-modules (ice-9 rdelim))
(use-modules (ice-9 textual-ports))

(define input (open-input-file "input"))

(define parse
  (lambda (file)
    (let ([f (get-line file)])
      (list->vector (string-split f #\,)))))

(define (last l) (car (reverse l)))
(define (x pair) (car pair))
(define (y pair) (last pair))

(define (direction s)
  (string-ref s 0))

(define (mag s)
  (string->number (substring s 1)))

(define (dist p)
  (+ (abs (x p)) (abs (y p))))

(define (slope p1 p2)
  (cond
    ((= (- (x p1) (x p2)) 0)
     'inf)
    ((= (- (y p1) (y p2)) 0)
     'zero)))

(define wire1 (parse input))
(define wire2 (parse input))

(define (draw-wire w)
  (let d ([start (list '(0 0))][ptr 0])
    (cond
      ((= ptr (vector-length w)) start)
      ((eq? (direction (vector-ref w ptr)) #\R)
       (d (append start
                (cons 
                  (list
                   (+ (mag (vector-ref w ptr)) (x (last start)))
                   (y (last start)))
                  '()))
          (+ ptr 1)))
      ((eq? (direction (vector-ref w ptr)) #\L)
       (d (append start
                (cons
                  (list
                   (- (x (last start)) (mag (vector-ref w ptr)))
                   (y (last start)))
                  '()))
          (+ ptr 1)))
      ((eq? (direction (vector-ref w ptr)) #\U)
       (d (append start
                (cons
                  (list
                   (x (last start))
                   (+ (y (last start)) (mag (vector-ref w ptr))))
                  '()))
          (+ ptr 1)))
      ((eq? (direction (vector-ref w ptr)) #\D)
       (d (append start
                (cons
                  (list
                   (x (last start))
                   (- (y (last start)) (mag (vector-ref w ptr))))
                  '()))
          (+ ptr 1)))
      (else start))))

(define (intersection p1 p2 p3 p4)
  (cond
    ((or 
       (eq? p1 '(0 0))
       (eq? p2 '(0 0))
       (eq? p3 '(0 0))
       (eq? p4 '(0 0)))
     '())
    ((or (null? p1) (null? p2) (null? p3) (null? p4)) '())
    ((eq? (slope p1 p2) (slope p3 p4)) '())
    ((= (x p1) (x p2)) (intersection p3 p4 p1 p2))
    ((> (x p1) (x p2)) (intersection p2 p1 p3 p4))
    ((> (y p3) (y p4)) (intersection p1 p2 p4 p3))
    ((or (> (x p4) (x p2))
         (> (y p3) (y p1))
         (< (x p4) (x p1))
         (< (y p4) (y p2))) '())
    (else 
      (list (x p3) (y p1)))))

(define dt1 (draw-wire wire1))
(define dt2 (draw-wire wire2))

(define (find-all wire1 wire2)
  (let loop ([w1 wire1][w2 wire2][points '()])
    (cond
      ((and (null? (cddr w1)) (null? (cddr w2))) points)
      ((null? (cddr w2)) (loop (cdr w1) wire2 points))
      (else
        (let ([i (intersection (car w1) (cadr w1) (car w2) (cadr w2))])
          (if
            (not (eq? i '()))
            (loop w1 (cdr w2) (append points (list i)))
            (loop w1 (cdr w2) points)))))))
