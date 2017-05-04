(cd "json.scm/")
(load "json-decode")


(define (json-parser req)
  (json-decode (http-request-body req)))
