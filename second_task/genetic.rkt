#lang scheme/base
(require scheme/list)
(require scheme/math)

(define population_size 20)                                         ;Размер популяции
(define max_generations 60)                                         ;Максимальное количество поколений
(define cross_probability .3)                                       ;Вероятность скрещивания
(define crossing_part (exact-round (* .8 population_size)))         ;Какая часть популяции участвует в скрещивании
(define mutation_probability .7)                                    ;Вероятность мутировании хромосомы
(define mutate_colour_prob .05)                                     ;Вероятность мутации одного цвета хромосомы
(define selection_part (exact-round (* .4 population_size)))        ;Какая часть популяции участвует в переходе в след. поколение
(define unmutable_part (exact-round (* .1 population_size)))        ;Какая часть популяции гарантированно не изменяется

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
;Сгенерировать начальную популяцию
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

; получить вектор, состоящий из количества конфликтов для каждой хромосомы популяции.
(define (get-conflicts-all-chromosomes population)
    (build-vector
        (vector-length population)
        (lambda (i) (get-amount-conflicts (vector-ref population i)))
    )
)

; скрещивание двух хросом (выбирается точка скрещивания и первая часть берется и второй хромосомы, а вторая из первой)
(define (cross-chromosome a b)
    (let ((cross_point (random edges-amount)))
        (build-vector
            edges-amount
            (lambda (i)
                (if (< i cross_point)
                    (vector-ref b i)
                    (vector-ref a i)
                )
            )
        )
    )
)

; Мутировать каждый цвет хромосомы с заданной вероятностью
(define (mutate-chromosome colours chromosome)
    (build-vector
        edges-amount
        (lambda (i)
            (cond
                ((< (random) mutate_colour_prob)
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
                (<= remaining_generations 0)                                            ;Если закончился лимит поколений - возвращаем результат
                (if (= 0 bestscore)
                    (vector-ref population best)
                    '()
                )
            )
            (
                (> bestscore 0)                                                         ;Если еще не получили раскраску - продолжаем
                (let*
                    (
                        (
                            next
                            (build-vector                                               ;Генерация следующего поколения
                                population_size
                                (lambda (i)
                                    (let
                                        (
                                            (
                                                chromosome
                                                (cond
                                                    ((< i unmutable_part)               ;Не меняем определунную часть хромосом
                                                        (vector-ref population i)
                                                    )
                                                    ((< (random) cross_probability)     ;Получение новой особи путем скрещивания старых
                                                        (cross-chromosome
                                                            (vector-ref population (random crossing_part))
                                                            (vector-ref population (random crossing_part))
                                                        )
                                                    )
                                                    (else                               ;Если не скрещиванием, то выбираем рандомно
                                                        (vector-ref population (random selection_part))
                                                    )
                                                )
                                            )
                                        )                                               ;Мутируем
                                        (if (and (>= i unmutable_part) (< (random) mutation_probability))
                                            (mutate-chromosome colours chromosome)
                                            chromosome
                                        )
                                    )
                                )
                            )
                        )
                    )
                    (if (< bestscore old_best)
                        (next-generation colours next bestscore max_generations)        ;Если результат улучшился, то обновляем счетчик поколений
                        (next-generation colours next old_best (- remaining_generations 1))
                    )
                )
            )
            (else
                (vector-ref population best)
            )
        )
    )
)

;Запуск генетического алгоритма
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

(define (find-better-solution-linear amnt-colours prev_answer)
    (let*
        (
            (answer (genetic-solve amnt-colours))
        )
        (if (null? answer)
            (cons #t prev_answer)
            (find-better-solution-linear (- amnt-colours 1) (cons amnt-colours answer))
        )
    )
)

;Решаем задачу в зависимости от того, можем ли мы раскрасить граф, как полный или нет.
(define (solve-task)
    (let ((colours-amount-for-complete-graph (- vertices-amount (modulo (+ vertices-amount 1) 2))))
        (if (>= max-colours colours-amount-for-complete-graph)
            ;(find-better-solution
            ;    (- max-vertices-power 1)
            ;    colours-amount-for-complete-graph
            ;    (cons colours-amount-for-complete-graph (tag-complete-graph colours-amount-for-complete-graph))
            ;)
            (
                find-better-solution-linear
                max-colours
                (cons colours-amount-for-complete-graph (tag-complete-graph colours-amount-for-complete-graph))
            )
            ;Изначально пытаемся решить задачу с максимальным количеством цветов
            (let ((answer (genetic-solve max-colours)))
                (if (null? answer)
                    '(#f)
                    ;(find-better-solution (- max-vertices-power 1) max-colours (cons max-colours answer))
                    (find-better-solution-linear (- max-colours 1) (cons max-colours answer))
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
