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
     "/hello-world")

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
  "/cats")
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

### Utilities
#### Static Files
Not all requests require handlers - sometimes you just
want to return a file. Sprocket provides a `serve-static`
helper for this:

**serve-static** *path*

Example:

```scheme
(get server (serve-static "public") "/static")
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
  "/redirect")
```
