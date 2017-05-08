(load "server.scm")

(define server (create-server))

;;; Middleware, called for each request:
(add-handler
 server
 (lambda (req params)
   (printf "-> request: ~A" req)))

(get server
     (lambda (req params) '(200 () "Hello World!"))
     '("hello-world"))

(post server
      (lambda (req params) '(200 () "\o/"))
      '("cats"))

;;; Error middleware example:
(add-error-handler
 server
 (lambda (req params err)
   (printf "-> error: ~A - in request: ~A" err req)))

(post server
      (lambda (req params)
	;; trigger an error:
	(unbound-procedure)
	'(200 () "this shouldn't be reached!"))
      '("trigger-error"))

;;; Simple handler example:
(get server
     (lambda (req params) "no more lists!")
     '("simple"))

;;; Static example:
(get server (serve-static "public") '("static"))

;;; Redirect example:
(get server
     (lambda (req params) (redirect "http://localhost:3000/hello-world"))
     '("redirect"))

(get server
     (lambda (req params) (redirect "http://localhost:3000/simple" 301))
     '("permanent-redirect"))

;;; Routing pattern examples:
(get server
     (lambda (req params)
       `(200 ()
	     ,(string-append
	       "We're buying cat number "
	       (number->string (car params)))))
     '("cats" number-arg "buy"))

(get server
     (lambda (req params)
       `(200 ()
	     ,(string-append
	       "We're buying the dog named "
	       (car params))))
     '("dogs" string-arg "buy"))

;;; Serve file example:
(get server
     (lambda (req params)
       (serve-file "public/file.txt"))
     '("serve-file"))

;;; JSON parser example:
(add-handler server
	     json-body-parser)

(post server
      (lambda (req)
        (let ((body (http-request-body req)))
          (printf "body: ~A" body)
          (string-append
           "First value: "
           (cdar (vector-ref body 0)))))
      "/insert")

(listen server 3000)
