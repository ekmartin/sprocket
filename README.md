# Sprocket
Sprocket is a developer friendly mit-scheme web framework.

## Installation
Because Sprocket relies on `httpio`, it requires at least mit-scheme v9.

```bash
$ git clone https://github.com/ekmartin/sprocket.git
```

Then in your Scheme project you can do:

```scheme
(cd "sprocket")
(load "server.scm")
```

## Usage
```scheme
;;; First create a new server instance:
(define server (create-server))

;;; Attach a handler:
(get server
     (lambda (req params) '(200 () "Hello World!"))
     '("hello-world"))

;;; And finally, start the server on port 3000:
(listen server 3000)
```

## API
### Core
Sprocket lets you build applications as a set of middleware that
turns a request into a response. The basic
building block for creating new middleware is `add-handler`:

**add-handler** *server* *handler* *[path]* *[method]*

Example:
```scheme
(add-handler server
  (lambda (req params)
    (printf "-> request: ~A" req)))
```

Sprocket also provides a set of helper procedures that makes
it easier to define new handlers:

**get** *server* *handler* *[path]*

**post** *server* *handler* *[path]*

**put** *server* *handler* *[path]*

**delete** *server* *handler* *[path]*

Example:
```scheme
(post server
  (lambda (req params) '(200 () "Let's add a cat!"))
  '("cats"))
```

In a similar vein, Sprocket also lets you define
middleware that are only called in case of errors:

**add-error-handler** *server* *handler* *[path]* *[method]*

Example:
```scheme
(add-error-handler
  server
  (lambda (req params err)
    (printf "-> error: ~A - in request: ~A" err req)))
```
Similar to express's bodyParser.json(), Sprocket allows you to
take in json data and parse it into a Scheme data structure,
making use of `json-decode` from https://github.com/joeltg/json.scm.
You can make use of this functionality by calling:

**add-handler** *server* *json-body-parser*

Example:
```scheme
(post server
      (lambda (req)
        (let ((body (http-request-body req)))
          (printf "body: ~A" body)
          (string-append
           "First value: "
           (cdar (vector-ref body 0)))))
      "/insert")
```

### Utilities
#### Static Files
Not all requests require handlers - sometimes you just
want to return a file. Sprocket provides a `serve-static`
helper for this:

**serve-static** *path*

Example:

```scheme
(get server (serve-static "public") '("static"))
```

This would for example cause Sprocket to respond to
a `/static/file.txt` request with the contents of
`./public/file.txt`. In the case where Sprocket
fails to read file at `path`, it turns over control
to the next middleware, and logs the error.

#### Redirects
`redirect` returns a response that redirects the client
to the given location.

**redirect** *location* *[status = 302]*

Example:
```scheme
(get server
  (lambda (req params)
    (redirect "http://localhost:3000/hello-world"))
  '("redirect"))
```

#### Routing Parameters
The path argument to `add-handler` is a list where each
element matches a forward slash separated segment of a URL.
This can either be a string, or one of the symbols `number-arg`
and `string-arg`. The last two matches arbitrary values of their
respective types, and passes a list of the matched parameters
to the request handler.

Examples:
```scheme
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
```

#### JSON Parsing
`json-body-parser` takes in the existing request body as
json, converts it into a Scheme data structure, and
updates the request body with the new body.

See Below:
```scheme
(define (json-body-parser)
  (lambda (req)
   (let ((body (json-parse req))
	 ;;; gets procedure for updating req body
	 (modifier (record-modifier
		    (record-type-descriptor req)
		    'body)))
     ;;; update http request body
     (modifier req body))))
```
