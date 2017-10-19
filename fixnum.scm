
;; Small Scheme examples
;; Based on code in `Teach Yourself Scheme in Fixnum Days'

;; Display a time-appropriate greeting for |hour|

(define greet 
  (lambda(hour)
    (if (< hour 12)
	(display "Good Morning!")
	(display "Good Afternoon!"))
    (newline)))

(begin (greet 10))

