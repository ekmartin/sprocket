(cd "json.scm/")
(load "json-decode")
(load "json-encode")

(define (json-parse req)
  (json-decode (http-request-body req)))

(define (json-body-parser)
  (lambda (req)
   (let ((body (json-parse req))
	 ;;; gets procedure for updating req body
	 (modifier (record-modifier
		    (record-type-descriptor req)
		    'body)))
     ;;; update http request body
     (modifier req body))))

(define (json-serialize req)
  (json-encode (http-request-body req)))
