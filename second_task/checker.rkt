#lang scheme/base
(require racket/list)

;Из раскраски составляем список всех вершин графа
(define (get-vertexes-list colours)
  (define (loop unprocessed-vertexes res)
    (cond
      ((empty? unprocessed-vertexes) res)
      ((member (car unprocessed-vertexes) res) (loop (cdr unprocessed-vertexes) res))
      (else (loop (cdr unprocessed-vertexes) (cons (car unprocessed-vertexes) res)))
    )
  )
  (loop (flatten (map (lambda (x) (car x)) colours)) '())
)

;Вставляем в ассоциативный список(вершина->список цветов) очередной цвет, если такой цвет уже существует, то заменяем данную вершину на #f
(define (insert-into-assoc lst key value)
  (map (lambda(x) (if (equal? key (car x)) (if (member value (cdr x)) #f (cons (car x) (cons value (cdr x)))) x)) lst)
)

;Функция проверяющая является ли раскраска colours корректной при максимальном числе цветов max-amount
(define (check-colours colours max-amount)
  (define (loop colours-of-vertexes unique-colours unprocessed-colours)
    (cond
      ((> (length unique-colours) max-amount) #f) 
      ((empty? unprocessed-colours) #t)
      (else
        (let* 
          (
            (cur (car unprocessed-colours)) 
            (colours-with-added-first-vertex (insert-into-assoc colours-of-vertexes (car (car cur)) (cadr cur))) ; добавляем цвет в первую верш.
          ) 
          (if (not (member #f colours-with-added-first-vertex))                                                  ; проверяем не было ли совпадений
            (let ((colours-with-added-second-vertex (insert-into-assoc colours-with-added-first-vertex (cadr (car cur)) (cadr cur)))) 
              (if (not (member #f colours-with-added-second-vertex))
                (if (member (cadr cur) unique-colours)                                                           ;если новый цвет, добавляем его
                  (loop colours-with-added-second-vertex unique-colours (cdr unprocessed-colours))
                  (loop colours-with-added-second-vertex (cons (cadr cur) unique-colours) (cdr unprocessed-colours))
                )
                #f
              )
            )
            #f
          )
        )
      )
    )
  )
  (loop (map (lambda(x) (list x)) (get-vertexes-list colours)) '() colours)
)

;считывание сначала - ожидаемого результата, затем - то, что нам выдал генетический алгоритм.
;после этого проверяем совпали ли ответы и является ли раскраска корректной
(let ((expected-res (read))) 
  (if expected-res
    (let* ((expected-amount (read)) (answer-res (read)))
      (if (not answer-res) 
        #f
        (let* ((answer-amount (read)) (answer-colours (read))) 
          (and (= answer-amount expected-amount) (check-colours answer-colours answer-amount))
        )
      )
    )
    (let ((answer-res (read)))
      (equal? expected-res answer-res)
    )
  )
)
