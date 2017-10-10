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

(define (hedge)
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

(define (keyword-dependent-answer keywords-answers-struct sent)

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
        (reverse (loop max-value list '() 0))
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

(define (visit-doctor)
  (define (doctor-driver-loop name phrase-history is-first-iteration)
    (define (reply user-response)
      (let
        (
          (gen-case (if is-first-iteration (random 2) (random 3)))
          (keyword-answer (keyword-dependent-answer keywords-answers-struct user-response))
        )
        (
          if (not (null? keyword-answer))
            keyword-answer
            (cond
              ((= gen-case 0)
                (append (qualifier) (change-person user-response))
              )
              ((= gen-case 1) (hedge))
              ((= gen-case 2)
                (append
                  '(earlier you said that)
                  (change-person (pick-random phrase-history))
                )
              )
            )
        )
      )
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
          (print (reply (flatten user-response)))
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



