#lang racket

(require racket/tcp
         racket/port
         net/url
         net/uri-codec
         racket/string)

;; Parse request path and query parameters
(define (parse-request-line line)
  (define parts (string-split line " "))
  (if (>= (length parts) 2)
      (let* ([raw-url (second parts)]
             [url-obj (string->url raw-url)]
             [path (map path/param-path (url-path url-obj))]
             [query (url-query url-obj)])
        (values path query))
      (values '() '())))

;; Perform calculation
(define (calculate x y op)
  (with-handlers ([exn:fail? (lambda (_) "Invalid input!")])
    (define a (string->number x))
    (define b (string->number y))
    (cond
      [(equal? op "+") (number->string (+ a b))]
      [(equal? op "-") (number->string (- a b))]
      [(equal? op "*") (number->string (* a b))]
      [(equal? op "/") (if (zero? b) "Cannot divide by zero!" (number->string (/ a b)))]
      [else "Unknown operator."])))

;; CSS styling
(define css-style "
  <style>
    body {
      font-family: 'Segoe UI', sans-serif;
      background-color: #f0f4f8;
      text-align: center;
      padding: 2rem;
    }
    h1 {
      color: #333;
    }
    form {
      margin: 2rem auto;
      padding: 1.5rem;
      border-radius: 10px;
      background: #fff;
      width: 300px;
      box-shadow: 0 4px 10px rgba(0,0,0,0.1);
    }
    input[type='text'], select {
      width: 90%;
      padding: 0.5rem;
      margin-bottom: 1rem;
      font-size: 1rem;
      border-radius: 5px;
      border: 1px solid #ccc;
    }
    input[type='submit'] {
      padding: 0.6rem 1.5rem;
      font-size: 1rem;
      background-color: #007BFF;
      color: white;
      border: none;
      border-radius: 6px;
      cursor: pointer;
    }
    input[type='submit']:hover {
      background-color: #0056b3;
    }
    .result {
      margin-top: 2rem;
      font-size: 1.3rem;
      font-weight: bold;
      color: #2e7d32;
    }
    .error {
      color: #c62828;
    }
    a {
      display: block;
      margin-top: 1.5rem;
      color: #007BFF;
      text-decoration: none;
    }
  </style>
")

;; Route logic
(define (route path query)
  (cond
    [(equal? path '("calc"))
     (string-append
      "<h1>Web Calculator</h1>"
      "<form action='/result' method='get'>
        <input type='text' name='x' placeholder='First number' required>
        <select name='op'>
          <option value='+'>+</option>
          <option value='-'>-</option>
          <option value='*'>*</option>
          <option value='/'>/</option>
        </select>
        <input type='text' name='y' placeholder='Second number' required>
        <input type='submit' value='Calculate'>
      </form>")]

    [(equal? path '("result"))
     (define x (cdr (assoc 'x query)))
     (define y (cdr (assoc 'y query)))
     (define op (cdr (assoc 'op query)))
     (define result
       (if (and x y op)
           (calculate x y op)
           "Missing input!"))
     (string-append
      "<h1>Result</h1>"
      (format "<div class='~a'>~a</div>"
              (if (or (string-contains? result "Invalid")
                      (string-contains? result "zero"))
                  "error"
                  "result")
              result)
      "<a href='/calc'>🔁 Back to Calculator</a>")]

    [else
     "<h1>404 - Page Not Found</h1><a href='/calc'>Go to Calculator</a>"]))

;; Client connection handler
(define (handle-client in out)
  (define request-line (read-line in 'any))
  (printf "Request: ~a\n" request-line)

  ;; Skip headers
  (let loop ()
    (define line (read-line in 'any))
    (unless (equal? line "")
      (loop)))

  ;; Process request
  (define-values (path query) (parse-request-line request-line))
  (define body (route path query))

  ;; Send HTTP response
  (display
   (string-append
    "HTTP/1.1 200 OK\r\n"
    "Content-Type: text/html\r\n\r\n"
    "<html><head><meta charset='UTF-8'>" css-style "</head><body>"
    body
    "</body></html>")
   out)
  (flush-output out))

;; Server starter
(define listener (tcp-listen 8080))
(printf "📟 Calculator App running at: http://localhost:8080/calc\n")

(define (serve-forever)
  (let loop ()
    (define-values (in out) (tcp-accept listener))
    (thread (lambda ()
              (handle-client in out)
              (close-input-port in)
              (close-output-port out)))
    (loop)))

(serve-forever)
