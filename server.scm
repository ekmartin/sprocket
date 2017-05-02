(load "httpio.scm")

(define HTTP/1.1 (cons 1 1))

(define (start-server tcp-port)
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

(define (handle-request request port)
  (let* ((url (http-request-uri request))
	 (path (uri-path url))
	 ;; TODO: not sure why the first element
	 ;; of the list is empty here:
	 (filename (cadr path)))
    (write-http-response
     (make-http-response
      HTTP/1.1 200
      (http-status-description 200)
      '()
      ;; TODO: handle errors and return 500 response:
      (read-file filename))
     port)))

;;; reads the string content at the given file path:
(define (read-file filename)
  (list->string
   (let ((port (open-input-file filename)))
    (let f ((x (read-char p)))
      (if (eof-object? x)
	  (begin
	    (close-input-port p)
	    '())
	  (cons x (f (read-char p))))))))

(start-server 3003)
