(cd "json.scm/")
(load "json-decode")
(load "json-encode")
(cd "..")

(define (json-parse req)
  (json-decode (http-request-body req)))

(define (json-body-parser)
  (lambda (req params)
   (let ((body (json-parse req))
	 ;;; gets procedure for updating req body
	 (modifier (record-modifier
		    (record-type-descriptor req)
		    'body)))
     ;;; update http request body
     (modifier req body))))
