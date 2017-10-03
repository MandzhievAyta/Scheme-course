#lang scheme/base
(define (visit-doctor name)
  (define (doctor-driver-loop name)
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
      
      (cond
        ((fifty-fifty)
          (append (qualifier) (change-person user-response))
        )
        (else (hedge))
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
          (print (reply user-response))
          (doctor-driver-loop name)
        )
      )
    )
  )

  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  (doctor-driver-loop name)
)

(define (fifty-fifty)
  (= (random 2) 0)
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
(visit-doctor 'Ayta)
