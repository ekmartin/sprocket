(define (make-middleware method path handler)
  (list method path handler))

(define (get-method middleware)
  (car middleware))

(define (get-path middleware)
  (cadr middleware))

(define (get-handler middleware)
  (caddr middleware))
