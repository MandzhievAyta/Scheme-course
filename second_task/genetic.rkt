#lang scheme/base
(require scheme/list)
(require scheme/math)

(define max_generations 60)
(define population_size 20)
(define mutation_probability .7)
(define mutation_severity .05)
(define cross_probability .3)
(define crossing_quota (max (min 3 population_size) (exact-round (* .8 population_size))))
(define selection_quota (max (min 2 population_size) (exact-round (* .4 population_size))))
(define keep_quota (max 1 (exact-round (* .1 population_size))))

;получить список всех вершин
(define (get-all-vertices i ht)
    (cond
        ((< i 0)  ht)
        (else
            (let ((edge (vector-ref graph i)))
                (get-all-vertices (- i 1) (hash-set* ht (car edge) '() (cadr edge) '()))
            )
        )
    )
)

;генерация рандомной хромосомы
(define (random-chromosome colours)
    (build-vector
        edges-amount
        (lambda (i) (+ (random colours) 1))
    )
)
;Генерировать начальную популяцию
(define (generate-initial-population colours)
    (build-vector
        population_size
        (lambda (i) (random-chromosome colours))
    )
)

;Количество конфликтов в хромосоме
(define (get-amount-conflicts chromosome)
    (define (loop i ht answer)
        (if (< i 0)
            answer
            (let*
                (
                    (key1 (cons (car (vector-ref graph i)) (vector-ref chromosome i)))
                    (val1 (+ 1 (hash-ref ht key1 -1)))
                    (key2 (cons (cadr (vector-ref graph i)) (vector-ref chromosome i)))
                    (val2 (+ 1 (hash-ref ht key2 -1)))
                )
                (loop
                    (- i 1)
                    (hash-set* ht key1 val1 key2 val2)
                    (+ answer val1 val2)
                )
            )
        )
    )
    (loop
        (- (vector-length chromosome) 1)
        (make-immutable-hash)
        0
    )
)

(define (get-conflicts-all-chromosomes population)
    (build-vector
        (vector-length population)
        (lambda (i) (get-amount-conflicts (vector-ref population i)))
    )
)

(define (cross-chromosome a b)
    (let ((tap (vector-ref vertices (random vertices-amount))))
        (build-vector
            edges-amount
            (lambda (i)
                (let ((edge (vector-ref graph i)))
                    (if (or (equal? tap (car edge)) (equal? tap (cadr edge)))
                        (vector-ref b i)
                        (vector-ref a i)
                    )
                )
            )
        )
    )
)

(define (mutate-chromosome colours chromosome)
    (build-vector
        edges-amount
        (lambda (i)
            (cond
                ((< (random) mutation_severity)
                (+ 1 (random colours)))
                (else (vector-ref chromosome i)))
        )
    )
)

;Поиск наименьшего количества конфликтов
(define (find-best scores)
    (define (loop i j)
        (if (<= i 0)
            j
            (loop
                (- i 1)
                (if (< (vector-ref scores i) (vector-ref scores j))
                    i
                    j
                )
            )
        )

    )
    (loop (- (vector-length scores) 1) 0)
)


(define (next-generation colours population old_best remaining_generations)
    (let*
        (
            (scores (get-conflicts-all-chromosomes population))
            (best (find-best scores))
            (bestscore (vector-ref scores best))
        )
        (cond
            (
                (<= remaining_generations 0)
                (if (= 0 bestscore)
                    (vector-ref population best)
                    '()
                )
            )
            (
                (> bestscore 0)
                (let*
                    (
                        (
                            sorted
                            (list->vector (sort (range (vector-length population)) < #:key (lambda (i) (vector-ref scores i))))
                        )
                        (
                            next
                            (build-vector
                                population_size
                                (lambda (i)
                                    (let
                                        (
                                            (
                                                chromosome
                                                (cond
                                                    ((< i keep_quota)
                                                        (vector-ref population (vector-ref sorted i))
                                                    )
                                                    ((< (random) cross_probability)
                                                        (cross-chromosome
                                                            (vector-ref population (vector-ref sorted (random crossing_quota)))
                                                            (vector-ref population (vector-ref sorted (random crossing_quota)))
                                                        )
                                                    )
                                                    (else
                                                        (vector-ref population (vector-ref sorted (random selection_quota)))
                                                    )
                                                )
                                            )
                                        )
                                        (if (and (>= i keep_quota) (< (random) mutation_probability))
                                            (mutate-chromosome colours chromosome)
                                            chromosome
                                        )
                                    )
                                )
                            )
                        )
                    )
                    (cond ((< bestscore old_best)
                        (next-generation colours next bestscore max_generations))
                        (else
                            (next-generation colours next old_best (sub1 remaining_generations))
                        )
                    )
                )
            )
            (else
                (vector-ref population best)
            )
        )
    )
)

(define (genetic-solve colours)
    (next-generation
        colours
        (generate-initial-population colours)
        +inf.0
        max_generations
    )
)

;Размечаем полный граф в colours-amount цветов
(define (tag-complete-graph colours-amount)
    (build-vector
        edges-amount
        (lambda (i)
            (let*
                (
                    (edge (vector-ref graph i))
                    (a (car edge))
                    (b (cadr edge))
                )
                (cond
                    ((>= a colours-amount) (+ b 1))
                    ((>= b colours-amount) (+ a 1))
                    (else
                        (if (even? (+ a b))
                            (+ (quotient (+ a b) 2) 1)
                            (+ (remainder (quotient (+ (+ a b) colours-amount) 2) colours-amount) 1)
                        )
                    )
                )
            )
        )
    )
)
;Пытаемся найти лучшее решение, путем бинарного поиска, каждый раз пытаясь решить задачу генетическим алгоритмом.
(define (find-better-solution min_colours max-colours upper_answer)
 (begin ;(printf "LOL~aLOL" upper_answe.)
    (if (>= (+ 1 min_colours) max-colours)
        (cons #t upper_answer)
        (let*
            (
                (mid-colours (quotient (+ min_colours max-colours) 2))
                (answer (genetic-solve mid-colours))
            )
            (if (null? answer)
                (find-better-solution mid-colours max-colours upper_answer)
                (find-better-solution min_colours mid-colours (cons mid-colours answer))
            )
        )
    )
 )
)

;Решаем задачу в зависимости от того, можем ли мы раскрасить граф, как полный или нет.
(define (solve-task)
    (let ((colours-amount-for-complete-graph (- vertices-amount (modulo (+ vertices-amount 1) 2))))
        (if (>= max-colours colours-amount-for-complete-graph)
            (find-better-solution
                (- max-vertices-power 1)
                colours-amount-for-complete-graph
                (cons colours-amount-for-complete-graph (tag-complete-graph colours-amount-for-complete-graph))
            )
            ;Изначально пытаемся решить задачу с максимальным количеством цветов
            (let ((answer (genetic-solve max-colours)))
                (if (null? answer)
                    '(#f)
                    (find-better-solution (- max-vertices-power 1) max-colours (cons max-colours answer))
                )
            )
        )
    )
)

(define (chromosome-to-answer chromosome)
  (build-list edges-amount (lambda (i) (list (vector-ref graph i) (vector-ref chromosome i)))))


(define graph (list->vector (read)))

;Количество ребер
(define edges-amount (vector-length graph))

(define max-colours (read))

;Проверка крайних случаев
(cond
    ( (< max-colours 0)
        (printf "#f~n")
        (exit 0)
    )
    ( (= edges-amount 0)
        (printf "~a~n~a~n~a" #t 0 '() )
        (exit 0)
    )
    ( (= max-colours 0)
        (printf "#f~n")
        (exit 0)
    )
)

;Переменная, хранящая список всех вершин, отсортированных по возрастанию
(define
    vertices
    (list->vector
        (sort
            (hash-keys (get-all-vertices (- edges-amount 1) (make-immutable-hash)))
            <
        )
    )
)

;Количество вершин
(define vertices-amount (vector-length vertices))

(define (max-vertex-power i ht mp)
    (cond
        ((< i 0) mp)
        (else
            (let*
                (
                    (edge (vector-ref graph i) )
                    (first-vertex (+ 1 (hash-ref ht (car edge) 0)) )
                    (second-vertex (+ 1 (hash-ref ht (cadr edge) 0)) )
                )
                (max-vertex-power
                    (- i 1)
                    (hash-set* ht (car edge) first-vertex (cadr edge) second-vertex)
                    (max mp first-vertex second-vertex)
                )
            )
        )
    )
)

;Максимальная степень вершин
(define max-vertices-power
    (max-vertex-power (- edges-amount 1) (make-immutable-hash) 0)
)

(let ((answer (solve-task)))
  ;(printf "2222~a2222" answer)
  (write (car answer))(newline)
  (cond ((car answer)
         (write (cadr answer))(newline)
         (write (chromosome-to-answer (cddr answer)))(newline))))
