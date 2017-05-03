;;; appends new to the end of existing
(define (append-element existing new)
  (append existing (list new)))

;; (append-element (list 1 2 3) 4)
;; -> (1 2 3 4)

;;; returns #t if the list short is a subset of long:
(define (subset? long short)
  (and
   (>= (length long) (length short))
   (equal?
    short
    (sublist long 0 (length short)))))

;; (subset? (list 1 2) (list 1 2))
;; -> #t
;; (subset? (list 1 2 3 4) (list 1 2))
;; -> #t
;; (subset? (list 5 2 3 4) (list 1 2))
;; -> #f
;; (subset? '("" "hello-world") '("" "hello-world" "a"))
;; -> #f

;;; creates a string by joining the elements list by separator
(define (string-join elements separator)
  (define (join rest)
    (let ((first (car rest))
	  (others (cdr rest)))
      (if (null? others)
	  (list first)
	  (cons first
		(cons separator
		 (join others))))))

  (if (null? elements)
      ""
      (apply string-append (join elements))))

;; (string-join (list "a" "b" "c") "/")
;; -> "a/b/c"
;; (string-join '() "/")
;; -> ""
;; (string-join (list "a") "/")
;; -> "a"
