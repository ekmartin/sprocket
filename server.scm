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
		    (display (read-http-request port))
		    (write-http-response
		     (make-http-response HTTP/1.1 200
		      (http-status-description 200)
		      '()
		      "Hello World")
		   port))
		  (lambda ()
		    (close-port port))))))
	(lambda () (channel-close socket)))))

(start-server 3000)
