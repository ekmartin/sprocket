(load "httpio.scm")
(load "middleware.scm")

#|
Initializes our web server.
|#
(define (create-server)
  ;;; Local variables
  (define error-handlers '())
  (define handlers '())

  ;;; Dispatchable procedures
  (define (add-handler handler #!optional path method)
    (let ((entry (create-entry handler path method)))
      (set! handlers
	    (append-element handlers entry))))

  (define (add-error-handler handler #!optional path method)
    (let ((entry (create-entry handler path method)))
      (set! error-handlers
	    (append-element error-handlers entry))))

  (define (listen tcp-port)
    (let ((socket (open-tcp-server-socket
		   tcp-port
		   (host-address-loopback))))
      (display "Listening to port ") (display tcp-port) (newline)
      (dynamic-wind
	  (lambda () unspecific)
	  (lambda ()
	    (do () ((channel-closed? socket))
	      (let ((port (tcp-server-connection-accept socket #t #f)))
		(dynamic-wind
		    (lambda () unspecific)
		    (lambda () (serve-request port))
		    (lambda () (close-port port))))))
	  (lambda () (channel-close socket)))))

  ;;; Private helper procedures
  (define (serve-request port)
    (let ((request (read-http-request port)))
      (call-with-current-continuation
       (lambda (cont)
	 (bind-condition-handler
	  (list condition-type:error)
	  ;; if the request processing errored, run it again,
	  ;; but this time with the error handlers:
	  (lambda (err)
	    (handle-request
	     (append-element error-handlers
			     (create-entry 500-handler))
	     request port err)
	    (cont '()))
	  ;; try processing it normally first though:
	  (lambda ()
	    (handle-request
	     (append-element handlers
			     (create-entry 404-handler))
	     request port)))))))

  (define (create-entry handler #!optional path method)
    (let ((url (if (default-object? path)
		   path
		   (->uri path))))
      (make-middleware method url handler)))

  (define (404-handler req)
    '(404 () "Page Not Found"))

  (define (500-handler req err)
    `(500 () ,(string-append
	       "Something went wrong: "
	       ;; TODO: should probably not leak this in production:
	       (condition/report-string err))))

  (define (create-response result)
    (receive
     (status headers body)
     (apply values result)
     (make-http-response HTTP/1.1 status
			 (http-status-description status)
			 headers body)))

  ;;; Check if the given middleware matches the request
  (define (match-middleware request middleware)
    (let ((method (get-method middleware))
	  (path (get-path middleware))
	  (handler (get-handler middleware))
	  (request-path (http-request-uri request))
	  (request-method (http-request-method request)))
      (let ((valid-method
	     (or (default-object? method)
		 (equal? method request-method)))
	    (valid-path
	     (or (default-object? path)
		 (equal? path request-path))))
	(and valid-method valid-path))))

  ;;; Wraps the results from simple response handlers (i.e. ones that
  ;;; return strings) in a (200 '() body) list
  (define (wrap-result result)
    (if (string? result)
	`(200 () ,result)
	result))

  (define (evaluate-handler handler request #!optional err)
    (wrap-result
     (if (default-object? err)
	 (handler request)
	 (handler request err))))

  (define (handle-request handler-list request port #!optional err)
    (let loop ((rest handler-list))
      (cond ((null? rest) 'done)
	    ((match-middleware request (car rest))
	     (begin
	       (let* ((middleware (car rest))
		      (method (get-method middleware))
		      (path (get-path middleware))
		      (handler (get-handler middleware)))
		 (let ((result (evaluate-handler handler request err)))
		   (if (list? result)
		       (write-http-response
			(create-response result)
			port)))
		 ;; go through the rest of the middleware, even if we've
		 ;; sent out a response:
		 (loop (cdr rest)))))
	    ;; this handler didn't match, so try the next one:
	    (else (loop (cdr rest))))))

  ;;; Dispatch on public procedures:
  (define (dispatch op)
    (case op
      ((listen) listen)
      ((add-handler) add-handler)
      ((add-error-handler) add-error-handler)
      (else (error "Unknown method: " op))))

  dispatch)

(define HTTP/1.1 (cons 1 1))

(define (append-element existing new)
  (append existing (list new)))

(define (listen server port)
  ((server 'listen) port))

(define (add-handler server handler #!optional path method)
  ((server 'add-handler) handler path method))

(define (add-error-handler server handler #!optional path method)
  ((server 'add-error-handler) handler path method))

;;; reads the string content at the given file path:
(define (read-file filename)
  (list->string
   (let ((port (open-input-file filename)))
     (let f ((x (read-char port)))
       (if (eof-object? x)
	   (begin
	     (close-input-port port)
	     '())
	   (cons x (f (read-char port))))))))

(define (post server handler #!optional path)
  (add-handler server handler path "POST"))

(define (get server handler #!optional path)
  (add-handler server handler path "GET"))

(define (put server handler #!optional path)
  (add-handler server handler path "PUT"))

(define (delete server handler #!optional path)
  (add-handler server handler path "DELETE"))

;;; Example:
(define server (create-server))

;;; Middleware, called for each request:
(add-handler
 server
 (lambda (req)
   (display "-> request: ")
   (display req)
   (newline)))

(get server
     (lambda (req) '(200 () "Hello World!"))
     "/hello-world")

(post server
      (lambda (req) '(200 () "\o/"))
      "/cats")

;;; Error middleware example:
(add-error-handler
 server
 (lambda (req err)
   (display "-> error: ")
   (display err)
   (display " - in request: ")
   (display req)
   (newline)))

(post server
      (lambda (req)
	;; trigger an error:
	(unbound-procedure)
	'(200 () "this shouldn't be reached!"))
      "/trigger-error")

;;; Simple handler example:"
(get server
     (lambda (req) "no more lists!")
     "/simple")

(listen server 3000)
