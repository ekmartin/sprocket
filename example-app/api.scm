(cd "..")
(load "server.scm")
(cd "example-app")

(define HEADERS
  ;; give everyone access to our API:
  (list (make-http-header 'Access-Control-Allow-Origin "*")))

(define DB_FILE "db.json")
(define CAT_FACTS_URL
  (->uri "http://catfacts-api.appspot.com/api/facts"))

(define INTERNAL-DEBUG-ERRORS #f)
(define server (create-server))
(add-handler server (json-body-parser))

(define (read-db)
  (vector-ref
   (json-decode (read-file DB_FILE))
   0))

(define (write-db cats)
  (call-with-output-file
   DB_FILE
   (lambda (port)
     (let ((dump (json-encode cats)))
       (output-port/write-string port dump)
       (flush-output port)))))

(define (create-response output-string)
  (list 200 HEADERS output-string))

(define (list-cats req params)
  (create-response (read-file DB_FILE)))

(define (update-cat id updater)
  (let ((db (read-db)))
    (vector-map
     (lambda (cat)
       (if (eq? (cdr (assq 'id cat)) id)
	   (updater cat)
	   cat))
     db)))

(define (fetch-fact)
  (let ((body
	 (http-message-body (http-get CAT_FACTS_URL '()))))
    #|
    The API returns a response on the form:
    {
      "facts":
        [
          "Cats are now Britain's favourite pet: there are 7.7 million
           cats as opposed to 6.6 million dogs."
        ],
       "success": "true"
    }

    so we need to unwrap what we care about, which is the string:
    |#

    (vector-ref
     (cdar (vector-ref (json-decode body) 0))
     0)))

(define (random-fact req params)
  (create-response (fetch-fact)))

(define (random-cat req params)
  (let ((db (read-db)))
    (let ((cat (json-encode
		(vector-ref db
			    (random (vector-length db))))))
      (create-response cat))))

(define (favorite-cat req params)
  (let ((id (car params)))
    (let ((updated-cats
	   (update-cat
	    id
	    (lambda (cat)
	      (let ((updated (del-assq 'favorite cat))
		    (favorite (cdr (assq 'favorite cat))))
		(cons (cons 'favorite (not favorite)) updated))))))
      (write-db updated-cats)
      (create-response
       (string-append
	"Updated cat "
	(number->string id))))))

;;; Routes
(post server favorite-cat '("cats" number-arg "favorite"))
(get server random-cat '("cats" "random"))
(get server list-cats '("cats"))

(get server random-fact '("facts" "random"))

(get server (serve-static "images") '("images"))

(listen server 8080)
