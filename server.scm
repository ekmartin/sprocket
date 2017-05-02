(load "httpio.scm")
(load "middleware.scm")

#|
Initializes our web server.
|#
(define (create-server)
  ;;; Local variables

  ;;; (method path) -> handler
  (define 404-handler
    ;; initialize handlers with a default 404 handler:
    (make-middleware
     '()
     '()
     (lambda (req)
       '(404 () "Page Not Found"))))

  (define handlers
    ;; Initialize handler
    (list 404-handler))

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

  (define (add-handler method path handler)
    (let ((entry (make-middleware
		  method (->uri path) handler)))
      (set! handlers (cons entry handlers))))

  ;;; Private helper procedures
  (define (wrap-handler handler)
    (lambda (req)
      (let ((result (handler req)))
	(if (list? result)
	    result
	    '()))))

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
	     (or (null? method)
		 (equal? method request-method)))
	    (valid-path
	     (or (null? path)
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
		   (if (not (null? result))
		       (write-http-response
			(create-response result)
			port))))
	       ;; go through the rest of the middleware, even if we've
	       ;; sent out a response:
	       (loop (cdr rest))))
	    ;; this handler didn't match, so try the next one:
	    (else (loop (cdr rest))))))

  (define (dispatch op)
    (case op
      ((listen) listen)
      ((add-handler) add-handler)
      (else (error "Unknown method: " op))))

  dispatch)

(define HTTP/1.1 (cons 1 1))

(define (listen server port)
  ((server 'listen) port))

(define (add-handler server method path handler)
  ((server 'add-handler) method path handler))

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

(define (post server path handler)
  (add-handler server "POST" path handler))

(define (get server path handler)
  (add-handler server "GET" path handler))

(define (put server path handler)
  (add-handler server "PUT" path handler))

(define (delete server path handler)
  (add-handler server "DELETE" path handler))

(define server (create-server))

(get server "/hello-world"
	     (lambda (req) '(200 () "Hello World!")))

(post server "/cats"
	     (lambda (req) '(200 () "\o/")))

(listen server 3000)
