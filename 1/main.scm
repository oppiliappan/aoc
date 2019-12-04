(use-modules (ice-9 rdelim))

(define in (open-input-file "input"))

(define f
  (lambda (file)
    (define fuelofuel
      (lambda (n)
        (let fof([total 0][x n])
          (if (< x 0)
            total
            (fof (+ total x) (- (floor-quotient x 3) 2))))))
    (let fuelcalc ([sum 0][x (read-line file)])
      (if (eof-object? x)
        sum
        (fuelcalc
          (+ sum
            (fuelofuel (- (floor-quotient (string->number x) 3) 2)))
          (read-line file))))))
