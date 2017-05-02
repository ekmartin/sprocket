(load "httpio.scm")
(load "middleware.scm")

#|
Initializes our web server.
|#
(define (create-server)
  ;;; Local variables
  (define handlers '())

  ;;; Dispatchable procedures
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
		    (lambda ()
		      (handle-request (read-http-request port) port))
		    (lambda ()
		      (close-port port))))))
	  (lambda () (channel-close socket)))))

  (define (add-handler handler #!optional path method)
    (let* ((url (if (default-object? path)
		    path
		    (->uri path)))
	   (entry (make-middleware method url handler)))
      (set! handlers (cons entry handlers))))

  ;;; Private helper procedures
  (define (404-handler req)
    '(404 () "Page Not Found"))

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

  (define (handle-request request port)
    (let loop ((rest handlers))
      (cond ((null? rest) 'done)
	    ((match-middleware request (car rest))
	     (begin
	       (let* ((middleware (car rest))
		      (method (get-method middleware))
		      (path (get-path middleware))
		      (handler (get-handler middleware)))
		 (let ((result (handler request)))
		   (if (list? result)
		       (write-http-response
			(create-response result)
			port)))
	       ;; go through the rest of the middleware, even if we've
	       ;; sent out a response:
	       (loop (cdr rest)))))
	    ;; this handler didn't match, so try the next one:
	    (else (loop (cdr rest))))))

  ;;; Initialize handlers with a default 404 handler:
  (add-handler 404-handler)

  ;;; Dispatch on public procedures:
  (define (dispatch op)
    (case op
      ((listen) listen)
      ((add-handler) add-handler)
      (else (error "Unknown method: " op))))

  dispatch)

(define HTTP/1.1 (cons 1 1))

(define (listen server port)
  ((server 'listen) port))

(define (add-handler server handler #!optional path method)
  ((server 'add-handler) handler path method))

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
(add-handler server
	     (lambda (req)
	       (display "-> request: ")
	       (display req)))

(get server
     (lambda (req) '(200 () "Hello World!"))
     "/hello-world")

(post server
      (lambda (req) '(200 () "\o/"))
      "/cats")

(listen server 3000)
