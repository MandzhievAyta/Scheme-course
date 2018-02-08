#lang scheme/base

;создает список всевозможных комбинаций пар элементов из двух списков (каждая пара содержит элементы из разных списков)
(define (combinations-2 list1 list2)
  (define (loop l1 l2 res)
    (cond
      ((and (= (length l1) 1) (= (length l2) 0))  res)
      ((= (length l2) 0) (loop (cdr l1) list2 res))
      (else (loop l1 (cdr l2) (cons (list (car l1) (car l2)) res)))
    )
  )
  (loop list1 list2 '())
)

;генератор списка от start до end не включая
(define (range-list start end)
  (define (loop i res)
    (if (= i start)
      (cons i res)
      (loop (- i 1) (cons i res))
    )
  )
  (loop (- end 1) '())
)

;функция, генерирующая граф-цепочку, где num - количество вершин
(define (gen-chain num)
  (define (loop i res)
    (if (= i 0)
      res
      (loop (- i 1) (cons (list (- i 1)  i) res))
    )
  )
  (loop num '())
)

;функция, генерирующая граф-цикл, num - количество вершин
(define (gen-cycle num)
  (define (loop i res)
    (if (= i 0)
      res
      (loop (- i 1) (cons (list (- i 1)  i) res))
    )
  )
  (loop (- num 1) (list (list (- num 1) 0)))
)

;функция, генерирующая полный граф, num - количество вершин
(define (gen-complete num)
  (define (loop i j res)
    (cond
      ((and (= i 1) (= j 0)) (reverse (cons (list 1 0) res)))
      ((= j 0) (loop (- i 1) (- i 2) (cons (list i 0) res)))
      (else (loop i (- j 1) (cons (list i j) res)))
    )
  )
  (loop num (- num 1) '())
)

;функция, генерирующая двудольный граф, в котором все вершин из одной группы, соеденены со всеми вершинами из другой
;параметры - количество вершин первой группы и список вершин второй группы
(define (gen-bipartite first-group second-group)
  (combinations-2 (range-list 0 first-group) (range-list first-group (+ first-group second-group)))
)

;генерирует цепочки длиной от 1 до num и печатает их в input-file для генетического алгоритма, а также записывает эталонный ответ в result-file
(define (print-chains num input-file result-file)
  (define (loop i)
    (if (= i num)
      (void)
      (begin
        ;(printf "~a ~a ~a~n" (gen-chain i) #t 2)
        ;(printf "~a ~a ~a~n" (gen-chain i) #f 1)
        (fprintf input-file "~a~n~a~n" (gen-chain i) 2)
        (fprintf result-file "~a~n~a~n" #t 2)
        (fprintf input-file "~a~n~a~n" (gen-chain i) 1)
        (fprintf result-file "~a~n" #f)
        (loop (+ i 1))
      )
    )
  )
  (loop 2)
)

;генерирует циклические графы с количеством вершин от 1 до num и печатает их в input-file для генетического алгоритма,
; а также записывает эталонный ответ в result-file
(define (print-cycle num input-file result-file)
  (define (loop i)
    (if (= i num)
      (void)
      (begin
        ;(printf "~a ~a ~a~n" (gen-cycle i) #t (+ 2 (modulo i 2)))
        ;(printf "~a ~a ~a~n" (gen-cycle i) #f 1)
        (fprintf input-file "~a~n~a~n" (gen-cycle i) (+ 2 (modulo i 2)))
        (fprintf result-file "~a~n~a~n" #t (+ 2 (modulo i 2)))
        (fprintf input-file "~a~n~a~n" (gen-cycle i) 1)
        (fprintf result-file "~a~n" #f )
        (loop (+ i 1))
      )
    )
  )
  (loop 3)
)

;генерирует полные графы с количеством вершин от 4 до num и печатает их в input-file для генетического алгоритма,
; а также записывает эталонный ответ в result-file
(define (print-complete num input-file result-file)
  (define (loop i)
    (if (= i num)
      (void)
      (begin
        ;(printf "~a ~a ~a~n" (gen-complete i) #t (+ ( - i 1) (modulo i 2)))
        ;(printf "~a ~a ~a~n" (gen-complete i) #f (- i 2))
        (fprintf input-file "~a~n~a~n" (gen-complete i) (+ i (modulo (+ i 1) 2)))
        (fprintf result-file "~a~n~a~n" #t (+ i (modulo (+ i 1) 2)))
        (fprintf input-file "~a~n~a~n" (gen-complete i) (- i 2))
        (fprintf result-file "~a~n" #f )
        (loop (+ i 1))
      )
    )
  )
  (loop 3)
)

;генерирует двудольные графы с количеством вершин от 6 до num + num/2(в первой группе i, а во второй i/2 вершин) и
; печатает их в input-file для генетического алгоритма, а также записывает эталонный ответ в result-file
(define (print-bipartite num input-file result-file)
  (define (loop i)
    (if (= i num)
      (void)
      (begin
        ;(printf "~a ~a ~a~n" (gen-bipartite i (truncate (/ i 2))) #t i)
        ;(printf "~a ~a ~a~n" (gen-bipartite i (truncate (/ i 2))) #f (- i 2))
        (fprintf input-file "~a~n~a~n" (gen-bipartite i (truncate (/ i 2))) i)
        (fprintf result-file "~a~n~a~n" #t i)
        (fprintf input-file "~a~n~a~n" (gen-bipartite i (truncate (/ i 2))) (- i 2))
        (fprintf result-file "~a~n" #f )
        (loop (+ i 1))
      )
    )
  )
  (loop 4)
)

;открываем файлы и записываем туда сгенерированные тесты
(let ((input-file (open-output-file "input_file.txt")) (result-file (open-output-file "result_file.txt")))
  (begin
    (print-chains 50 input-file result-file)
    (print-cycle 50 input-file result-file)
    (print-complete 50 input-file result-file)
    (print-bipartite 30 input-file result-file)
    (close-output-port input-file)
    (close-output-port result-file)
  )
)
