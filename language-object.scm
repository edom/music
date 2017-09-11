; object language

#|
(object
    (method (hello) (display "hello") (newline))
    (method (echo message) (display message) (newline))
)
|#

(define-syntax object-alist
    (syntax-rules ()
        (
            (_)
            '()
        )
        (
            (_ ((name param ...) body ...) rest ...)
            (cons
                (cons 'name (lambda (param ...) body ...))
                (object-alist rest ...)
            )
        )
    )
)

(define-syntax object
    (syntax-rules ()
        (
            (_ arg ...)
            (alist->object (object-alist arg ...))
        )
    )
)

(define hh
    (object-alist
        ((hello) (display "hello") (newline))
        ((echo message) (display message) (newline))
    )
)

(define (alist->object alist)
    (lambda (method-name . method-args)
        (define pair (assoc method-name alist))
        (if (not pair) (error "This object does not have method named" method-name))
        (apply (cdr pair) method-args)
    )
)

(define my-object
    (lambda (method-name . method-args)
        (case method-name
            ((hello) (apply (lambda () (display "hello") (newline)) method-args))
            ((echo) (apply (lambda (message) (display message) (newline)) method-args))
            (else (error "This object does not have method named" method-name))
        )
    )
)

(my-object 'hello)
(my-object 'echo "world")

; (define ob2 (alist->object hh))
(define ob2
    (object
        ((hello) (display "hello") (newline))
        ((echo message) (display message) (newline))
    )
)

(ob2 'hello)
(ob2 'echo "world")
