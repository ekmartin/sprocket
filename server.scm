(load "httpio.scm")
(load "middleware.scm")
(load "utils.scm")

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
      (define (catch-error err)
	(log "-> error in request ~A: ~A" request err)
	(handle-request
	 (append-element error-handlers
			 (create-entry 500-handler))
	 request port err))

      (define (try-request)
	(handle-request
	 (append-element handlers (create-entry 404-handler))
	 request port))

      (if INTERNAL-DEBUG-ERRORS
	  ;; for debugging we don't want the error to be handled:
	  (try-request)
	  (call-with-current-continuation
	   (lambda (cont)
	     (bind-condition-handler
	      (list condition-type:error)
	      ;; if the request processing errored, run it again,
	      ;; but this time with the error handlers:
	      (lambda (err)
		(catch-error err)
		(cont '()))
	      ;; try processing it normally first though:
	      try-request))))))

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

  (define (match-path? request-url handler-url)
    (let ((request-path (uri-path request-url))
	  (handler-path (uri-path handler-url)))
      ;; check if handler-path is a subset of request-path:
      (subset? request-path handler-path)))

  ;;; Check if the given middleware matches the request
  (define (match-middleware? request middleware)
    (let ((method (get-method middleware))
	  (url (get-url middleware))
	  (handler (get-handler middleware))
	  (request-url (http-request-uri request))
	  (request-method (http-request-method request)))
      (let ((valid-method
	     (or (default-object? method)
		 (equal? method request-method)))
	    (valid-path
	     (or (default-object? url)
		 (match-path? request-url url))))
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

  (define (write-response result port)
    (let ((response (create-response result)))
      (log "-> writing response: ~A" response)
      (write-http-response response port)))

  (define (handle-request handler-list request port #!optional err)
    (let loop ((rest handler-list))
      (cond ((null? rest) 'done)
	    ((match-middleware? request (car rest))
	     (begin
	       (let* ((middleware (car rest))
		      (handler (get-handler middleware))
		      (result (evaluate-handler handler request err)))
		 (if (list? result)
		     (write-response result port)))
	       ;; go through the rest of the middleware, even if we've
	       ;; sent out a response:
	       ;; TODO: don't allow sending out more responses if
	       ;; we've already sent out one.
	       (loop (cdr rest))))
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

(define INTERNAL-DEBUG-ERRORS #f)
(define ENABLE-LOGGING #t)

;;; Public API

(define (listen server port)
  ((server 'listen) port))

(define (add-handler server handler #!optional path method)
  ((server 'add-handler) handler path method))

(define (add-error-handler server handler #!optional path method)
  ((server 'add-error-handler) handler path method))

(define (post server handler #!optional path)
  (add-handler server handler path "POST"))

(define (get server handler #!optional path)
  (add-handler server handler path "GET"))

(define (put server handler #!optional path)
  (add-handler server handler path "PUT"))

(define (delete server handler #!optional path)
  (add-handler server handler path "DELETE"))

;;; creates a middleware that serves static files
;;; at the folder at `path`
(define (serve-static path)
  (lambda (req)
    (let* ((static-path (uri-path (string->uri path)))
	   (full-request-path (uri-path (http-request-uri req)))
	   ;; if we get a request for /static/file.txt and path is
	   ;; /static, we only care about file.txt - so find that:
	   (request-path (sublist
			  full-request-path
			  (+ (length static-path) 1)
			  (length full-request-path))))
      (let* ((filename (path->file request-path))
	     (file-path (string-append path filename))
	     (content
	      (call-with-current-continuation
	       (lambda (cont)
		 (bind-condition-handler
		  (list condition-type:file-error)
		  ;; we don't want to blow up the server
		  ;; for not found files etc., so pass it on to the
		  ;; next middleware.
		  (lambda (err)
		    (log "-> error serving file ~A: ~A" file-path err)
		    (cont '()))
		  (lambda ()
		    (read-file file-path)))))))
	(if (string? content)
	    `(200 () ,content))))))

;;; Helper procedures

(define (log str . elements)
  ;; TODO: config option instead, see #9
  (if ENABLE-LOGGING
      (apply printf str elements)))

(define (path->file path)
  (string-append "/" (string-join path "/")))

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

;;; Simple handler example:
(get server
     (lambda (req) "no more lists!")
     "/simple")

;;; Static example:
(get server (serve-static "public") "/static")

(listen server 3000)
