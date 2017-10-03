#lang scheme/base
(require racket/list)
(define (visit-doctor)
  (define (doctor-driver-loop name phrase-history is-first-iteration)
    (define (reply user-response)
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
              (why do you feel that way about your *?)
            )
          )
        )
      )

      (define (get-keywords-phrase user-response)
        (ormap
          (lambda (sublist) 
            (let
              (
                (founded-keyword
                  (ormap
                    (lambda (keyword) (if (member keyword user-response) keyword #f))
                    (car sublist)
                  )
                )
              )
              (
                if (not (equal? founded-keyword #f))
                  (many-replace (list(list '* founded-keyword)) (pick-random (cadr sublist)))
                  #f
              )
            )
          )
          keywords-answers-struct)
      )
      
      (let
        (
          (gen-case (if is-first-iteration (random 2) (random 3)))
          (keyword-answer (get-keywords-phrase user-response))
        )
        (
          if (not (equal? keyword-answer #f))
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

(define (ask-patient-name)
  (print '(next!))
  (print '(who are you?))
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
(visit-doctor)
