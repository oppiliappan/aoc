(use-modules (ice-9 rdelim))
(use-modules (ice-9 textual-ports))

(define input (open-input-file "input"))

(define parse
  (lambda (file)
    (let ([f (get-string-all file)])
      (list->vector (map string->number (string-split f #\,))))))

(define invec (parse input))

;;; fetch opcode
(define (oper num)
  (modulo num 100))

;;; fetch addressing modes
(define (get-modes num)
  (reverse 
    (map (lambda (n) (- (char->integer n) 48))
         (string->list (number->string num)))))

(define (checked l)
  (if
    (< (length l) 3)
    '()
    (cddr l)))

(define (mode-at loc m)
  (if (or (= 0 (length m)) (> loc (length m)))
    0
    (if (eq? loc 0)
      (car m)
      (mode-at (- loc 1) (cdr m)))))

;;; 0 indirect addressing
;;; 1 immidiate addressing
(define (addr v address mode)
  (cond 
    ((= mode 0) (vector-ref v (vector-ref v address)))
    ((= mode 1) (vector-ref v address))
    (else (display "wut"))))

;;; extremely dirty
(define interpret
  (lambda (v)
    (let ip ([ptr 0])
      (let ([operator (oper (vector-ref v ptr))]
            [modes (checked (get-modes (vector-ref v ptr)))])
        (let ([param1 (addr v (+ ptr 1) (mode-at 0 modes))]
              [param2 (addr v (+ ptr 2) (mode-at 1 modes))]) ; param2 may not always be valid
          (cond
           ((= operator 1) ; add
            (vector-set! v (vector-ref v (+ ptr 3)) ; will never be in immidiate mode
                         (+ param1 param2))
            (ip (+ ptr 4)))

           ((= operator 2) ; mul
            (vector-set! v (vector-ref v (+ ptr 3)) ; will never be in immidiate mode
                         (* param1 param2))
            (ip (+ ptr 4)))

           ((= operator 3) ; input
            (vector-set! v (vector-ref v (+ ptr 1))
                         (read))
            (ip (+ ptr 2)))

           ((= operator 4) ; display
            (display param1)
            (newline)
            (ip (+ ptr 2)))

           ((= operator 5) ; jump if true
            (if (not (= 0 param1))
              (ip param2)
              (ip (+ ptr 3))))

           ((= operator 6) ; jump if false
            (if (= 0 param1)
              (ip param2)
              (ip (+ ptr 3))))

           ((= operator 7) ; less than
            (if (< param1 param2)
              (vector-set! v (addr v (+ ptr 3) 1) 1)
              (vector-set! v (addr v (+ ptr 3) 1) 0))
            (ip (+ ptr 4)))

           ((= operator 8) ; equal to
            (if (= param1 param2)
              (vector-set! v (addr v (+ ptr 3) 1) 1)
              (vector-set! v (addr v (+ ptr 3) 1) 0))
            (ip (+ ptr 4)))

           ((= (vector-ref v ptr) 99) ; halt
            (display "end"))))))))
