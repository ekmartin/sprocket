(define (make-middleware method url handler)
  (list method url handler))

(define (get-method middleware)
  (car middleware))

(define (get-url middleware)
  (cadr middleware))

(define (get-handler middleware)
  (caddr middleware))
