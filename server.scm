(load "~/Documents/MIT Senior Year/6.905/6.945-project/httpio.scm")

#|
Initializes our web server.
|#
(define (create-server)
  ;;; Local variables

  ;;; (method path) -> handler
  (define handlers (make-equal-hash-table))

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
    (let ((url (->uri path)))
      (hash-table/put! handlers
		       (cons method url)
		       handler)))

  ;;; Private helper procedures
  (define (404-handler req)
    '(404 () "Page Not Found"))

  (define (wrap-handler handler)
    (lambda (req)
      ;; spread out the result with values,
      ;; so we can use (receive) on the result:
      (apply values (handler req))))

  (define (get-handler method path)
    (wrap-handler
     (hash-table/get
      handlers
      (cons method path)
      404-handler)))

  (define (handle-request request port)
    (let* ((path (http-request-uri request))
	   (method (http-request-method request))
	   (handler (get-handler method path)))
      (receive
       (status headers body)
       (handler request)
       (begin
	 (write-http-response
	  (make-http-response HTTP/1.1 status
			      (http-status-description status)
			      headers body)
	  port)))))

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
