(cd "json.scm/")
(load "json-decode")
(load "json-encode")

(define (json-parse req)
  (json-decode (http-request-body req)))

(define (json-serialize req)
  (json-encode (http-request-body req)))
