;;; appends new to the end of existing
(define (append-element existing new)
  (append existing (list new)))

;; (append-element (list 1 2 3) 4)
;; -> (1 2 3 4)

;;; returns #t if the list short is a subset of long:
(define (subset? long short)
  (equal?
   short
   (sublist long 0 (length short))))

;; (subset? (list 1 2 3 4) (list 1 2))
;; -> #t
;; (subset? (list 5 2 3 4) (list 1 2))
;; -> #f
