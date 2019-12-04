(use-modules (ice-9 rdelim))
(use-modules (ice-9 textual-ports))

(define input (open-input-file "input"))

(define parse
  (lambda (file)
    (let ([f (get-string-all file)])
      (list->vector (map string->number (string-split f #\,))))))

(define refvec (parse input))

(define valat
  (lambda (v addr)
    (vector-ref v (vector-ref v addr))))

(define interpret
  (lambda (v)
    (let op ([ptr 0])
      (cond
        ((= (vector-ref v ptr) 1)
         (vector-set! v
                      (vector-ref v (+ ptr 3))
                      (+ (valat v (+ ptr 1)) (valat v (+ ptr 2))))
         (op (+ ptr 4)))
        ((= (vector-ref v ptr) 2)
         (vector-set! v
                      (vector-ref v (+ ptr 3))
                      (* (valat v (+ ptr 1)) (valat v (+ ptr 2))))
         (op (+ ptr 4)))
        ((= (vector-ref v ptr) 99) (vector-ref v 0))))))

(define gravity-assist
  (let loop ([noun 99][verb 99][v (vector-copy refvec)])
    (vector-set! v 1 noun)
    (vector-set! v 2 verb)
    (if
      (= (interpret v) 19690720)
      (+ (* 100 noun) verb)
      (if
        (= verb 0)
        (loop (- noun 1) 99 (vector-copy refvec))
        (loop noun (- verb 1) (vector-copy refvec))))))
