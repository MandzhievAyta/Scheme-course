#lang scheme/base
(require racket/list)


(define (ask-patient-name)
  (printf "\nNext! Who are you?\n")
  (car(read))
)

(define (pick-random lst)
  (list-ref lst (random (length lst)))
)

(define (replace replacement-pairs word)
  (let ((replacement (assoc word replacement-pairs)))
    (cond
      ((eq? replacement #f) word)
      (else (cadr replacement))
    )
  )
)

(define (many-replace replacement-pairs lst)
  (cond
    ((null? replacement-pairs) lst)
    (else
      (map
        (lambda (w) (replace replacement-pairs w))
        lst
      )
    )
  )
)

(define (change-person phrase)
        (many-replace '((i you) (me you) (am are) (my your) (are am) (you i) (your my)) phrase)
)

(define (qualifier)
  (pick-random
    '(
      (you seem to think)
      (you feel that)
      (why do you believe)
      (why do you say)
      (what makes you feel)
      (it is weird that you think)
      (why do you suggest)
      (am i right that you claim)
    )
  )
)

(define (hedge sent keywords-list keywords-answers-struct is-first-iteration phrase-history)
  (pick-random
    '(
      (please go on)
      (many people have the same sorts of feelings)
      (many of my patients have told me the same thing)
      (please continue)
      (yes I m listening)
      (can you repeat please?)
      (hmm my assumptions proved to be true)
      (interesting... go on)
      (what is your opinion on this matter?)
      (what else can you add?)
    )
  )
)

(define keywords-answers-struct
  '(
    (
      (depressed suicide)
      (
        (when you feel depressed, go out for ice cream)
        (depression is a disease that can be treated)
      )
    )
    (
      (mother father parents)
      (
        (tell me more about your *)
        (why do you feel that way about your * ?)
      )
    )
  )
)

(define (get-keywords-list struct)
  (reverse
    (foldl
      (lambda (sublist init) (cons (list-ref sublist 0) init))
      '()
      struct
    )
  )
)

(define (check-keyword-entrance sent keywords-list is-first-iteration)
  (let ((flat-keywords-list (flatten keywords-list)))
    (ormap (lambda (word) (member word flat-keywords-list)) sent)
  )
)

(define (keyword-dependent-answer sent keywords-list keywords-answers-struct is-first-iteration phrase-history)

  (define (find-max-positions list)
    (define (loop value list result cur-pos)
      (if (empty? list)
        result
        (
          if (= value (car list))
            (loop value (cdr list) (cons cur-pos result) (+ 1 cur-pos))
            (loop value (cdr list) result (+ 1 cur-pos))
        )
      )
    )
    (let ((max-value (apply max list)))
      (if (= max-value 0)
        '()
        (loop max-value list '() 0)
      )
    )
  )
  (define (find-most-frequent-group-idx group-list sent)
    (if (or (null? sent) (null? group-list)) -1
      (let
        (
          (
            max-positions
            (
              find-max-positions
                (map
                  (
                    lambda (group) (
                      length (filter (lambda (word) (member word group)) sent)
                    )
                  )
                  group-list
                )
            )
          )
        )
        (
          if (null? max-positions) -1 (pick-random max-positions)
        )
      )
    )
  )
  
  (let*
    (
      (
        keywords-list
        (get-keywords-list keywords-answers-struct)
      )
      (
        most-frequent-group-idx
        (find-most-frequent-group-idx keywords-list sent)
      )
    )
    (
      if (= most-frequent-group-idx -1) '()
        (
          let*
          (
            (
              most-frequent-group
              (list-ref keywords-list most-frequent-group-idx)
            )
            (
              most-frequent-word-in-group-idx
              (find-most-frequent-group-idx (map list most-frequent-group) sent)
            )  
          )
          (
            many-replace (list(list '* (list-ref most-frequent-group most-frequent-word-in-group-idx))) (pick-random (cadr (list-ref keywords-answers-struct most-frequent-group-idx)))
          )
        )
    )
  )   
)

(define predicate-answer-struct
  (list
    (list
      (lambda (sent keywords-list is-first-iteration) (< (length sent) 3))
      2
      (lambda (sent keywords-list keywords-answers-struct is-first-iteration phrase-history) '(Can you say more ?))
    )
    (list
      check-keyword-entrance
      3
      keyword-dependent-answer
    )
    (list
      (lambda (sent keywords-list is-first-iteration) #t)
      1
      hedge
    )
    (list
      (lambda (sent keywords-list is-first-iteration) #t)
      1
      (lambda (sent keywords-list keywords-answers-struct is-first-iteration phrase-history) (append (qualifier) (change-person sent)))
    )
    (list
      (lambda (sent keywords-list is-first-iteration) (not is-first-iteration))
      1
      (lambda (sent keywords-list keywords-answers-struct is-first-iteration phrase-history) (append '(earlier you said that) (change-person (pick-random phrase-history))))
    )
  )
)

(define (get-answer-func phrase predicate-answer-struct is-first-iteration)
  (let*
    (
      (
        filtered-struct (filter (lambda (elem) ((car elem) phrase (get-keywords-list keywords-answers-struct) is-first-iteration)) predicate-answer-struct)
      )
      (
        max-weight (apply max (map (lambda (elem) (cadr elem)) filtered-struct))
      )
      (
        choise (pick-random (filter (lambda (elem) (= (cadr elem) max-weight)) filtered-struct))
      )
    )
    (list-ref choise 2)
  )
)


(define (visit-doctor)
  (define (doctor-driver-loop name phrase-history is-first-iteration)
    (define (reply user-response)
      (get-answer-func user-response predicate-answer-struct is-first-iteration)
    )

    (newline)
    (print '**)
    (let ((user-response (read)))
      (cond
        ((equal? user-response '(goodbye))
          (printf "Goodbye, ~a!\n" name)
          (print '(see you next week))
        )
        (else
          (print ((reply (flatten user-response)) user-response (get-keywords-list keywords-answers-struct) keywords-answers-struct is-first-iteration phrase-history))
          (doctor-driver-loop name (cons (flatten user-response) phrase-history) #f)
        )
      )
    )
  )

  (let ((name (ask-patient-name)))
    (
      if (equal? name 'suppertime)
        (print '(it is my suppertime wait me for 30 minutes))
        (begin
          (printf "Hello, ~a!\n" name)
          (print '(what seems to be the trouble?))
          (doctor-driver-loop name '() #t)
          (visit-doctor)
        )
    )
  )
)



(visit-doctor)



