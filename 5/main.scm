(use-modules (ice-9 rdelim))
(use-modules (ice-9 textual-ports))

(define input (open-input-file "input"))

(define parse
  (lambda (file)
    (let ([f (get-string-all file)])
      (list->vector (map string->number (string-split f #\,))))))

(define testvec (list->vector 
                  (map string->number (string-split "3,225,1,225,6,6,1100,1,238,225,104,0,2,136,183,224,101,-5304,224,224,4,224,1002,223,8,223,1001,224,6,224,1,224,223,223,1101,72,47,225,1101,59,55,225,1101,46,75,225,1101,49,15,224,101,-64,224,224,4,224,1002,223,8,223,1001,224,5,224,1,224,223,223,102,9,210,224,1001,224,-270,224,4,224,1002,223,8,223,1001,224,2,224,1,223,224,223,101,14,35,224,101,-86,224,224,4,224,1002,223,8,223,101,4,224,224,1,224,223,223,1102,40,74,224,1001,224,-2960,224,4,224,1002,223,8,223,101,5,224,224,1,224,223,223,1101,10,78,225,1001,39,90,224,1001,224,-149,224,4,224,102,8,223,223,1001,224,4,224,1,223,224,223,1002,217,50,224,1001,224,-1650,224,4,224,1002,223,8,223,1001,224,7,224,1,224,223,223,1102,68,8,225,1,43,214,224,1001,224,-126,224,4,224,102,8,223,223,101,3,224,224,1,224,223,223,1102,88,30,225,1102,18,80,225,1102,33,28,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,108,677,677,224,102,2,223,223,1005,224,329,1001,223,1,223,1107,677,226,224,102,2,223,223,1006,224,344,1001,223,1,223,108,226,226,224,102,2,223,223,1005,224,359,1001,223,1,223,1108,677,226,224,102,2,223,223,1006,224,374,101,1,223,223,108,677,226,224,102,2,223,223,1006,224,389,1001,223,1,223,107,226,226,224,102,2,223,223,1005,224,404,1001,223,1,223,8,226,226,224,102,2,223,223,1006,224,419,101,1,223,223,1107,677,677,224,102,2,223,223,1006,224,434,1001,223,1,223,1107,226,677,224,1002,223,2,223,1006,224,449,101,1,223,223,7,677,677,224,1002,223,2,223,1006,224,464,1001,223,1,223,1108,226,677,224,1002,223,2,223,1005,224,479,1001,223,1,223,8,677,226,224,1002,223,2,223,1005,224,494,101,1,223,223,7,226,677,224,102,2,223,223,1005,224,509,101,1,223,223,1008,677,226,224,102,2,223,223,1006,224,524,101,1,223,223,8,226,677,224,1002,223,2,223,1006,224,539,1001,223,1,223,1007,677,677,224,102,2,223,223,1005,224,554,101,1,223,223,107,226,677,224,1002,223,2,223,1005,224,569,1001,223,1,223,1108,677,677,224,1002,223,2,223,1006,224,584,1001,223,1,223,1008,226,226,224,1002,223,2,223,1005,224,599,101,1,223,223,1008,677,677,224,102,2,223,223,1005,224,614,101,1,223,223,7,677,226,224,1002,223,2,223,1005,224,629,1001,223,1,223,107,677,677,224,1002,223,2,223,1006,224,644,101,1,223,223,1007,226,677,224,1002,223,2,223,1005,224,659,1001,223,1,223,1007,226,226,224,102,2,223,223,1005,224,674,101,1,223,223,4,223,99,226" #\,))))

(define t1 (list->vector 
                  (map string->number (string-split "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99" #\,))))

(define refvec (parse input))

(define (oper num)
  (modulo num 100))

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

(define (addr v address mode)
  (cond 
    ((= mode 0) (vector-ref v (vector-ref v address)))
    ((= mode 1) (vector-ref v address))
    (else (display "wut"))))

;;; extremely dirty
(define interpret
  (lambda (v)
    (let op ([ptr 0])
      (display (oper (vector-ref v ptr)))
      (newline)
      (cond
        ((= (oper (vector-ref v ptr)) 1)
         (let ([modes (checked (get-modes (vector-ref v ptr)))])
           (vector-set! v
                        (vector-ref v (+ ptr 3)) ; will never be in immidiate mode
                        (+ (addr v (+ ptr 1) (mode-at 0 modes))
                           (addr v (+ ptr 2) (mode-at 1 modes)))))
         (op (+ ptr 4)))

        ((= (oper (vector-ref v ptr)) 2)
         (let ([modes (checked (get-modes (vector-ref v ptr)))])
           (vector-set! v
                        (vector-ref v (+ ptr 3)) ; will never be in immidiate mode
                        (* (addr v (+ ptr 1) (mode-at 0 modes))
                           (addr v (+ ptr 2) (mode-at 1 modes)))))
         (op (+ ptr 4)))

        ((= (oper (vector-ref v ptr)) 3)
         (vector-set! v
                      (vector-ref v (+ ptr 1))
                      (read))
         (op (+ ptr 2)))

        ((= (oper (vector-ref v ptr)) 4)
         (let ([modes (checked (get-modes (vector-ref v ptr)))])
           (display (addr v (+ ptr 1) (mode-at 0 modes)))
           (newline))
         (op (+ ptr 2)))

        ((= (oper (vector-ref v ptr)) 5)
         (let ([modes (checked (get-modes (vector-ref v ptr)))])
           (if
             (not (= 0 (addr v (+ ptr 1) (mode-at 0 modes))))
             (op (addr v (+ ptr 2) (mode-at 1 modes)))
             (op (+ ptr 3)))))

        ((= (oper (vector-ref v ptr)) 6)
         (let ([modes (checked (get-modes (vector-ref v ptr)))])
           (if
             (= 0 (addr v (+ ptr 1) (mode-at 0 modes)))
             (op (addr v (+ ptr 2) (mode-at 1 modes)))
             (op (+ ptr 3)))))

        ((= (oper (vector-ref v ptr)) 7)
         (let ([modes (checked (get-modes (vector-ref v ptr)))])
           (if
             (< (addr v (+ ptr 1) (mode-at 0 modes))
                (addr v (+ ptr 2) (mode-at 1 modes)))
             (vector-set! v
                          (addr v (+ ptr 3) 1)
                          1)
             (vector-set! v 
                          (addr v (+ ptr 3) 1)
                          0))
           (op (+ ptr 4))))

        ((= (oper (vector-ref v ptr)) 8)
         (let ([modes (checked (get-modes (vector-ref v ptr)))])
           (if
             (= (addr v (+ ptr 1) (mode-at 0 modes))
                (addr v (+ ptr 2) (mode-at 1 modes)))
             (vector-set! v
                          (addr v (+ ptr 3) 1)
                          1)
             (vector-set! v 
                          (addr v (+ ptr 3) 1)
                          0))
           (op (+ ptr 4))))

        ((= (vector-ref v ptr) 99) (display "end"))))))
