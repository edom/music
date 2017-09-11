#|
Explodes an alist into defines.

For example:

(explode alist to
    key-0
    (key-1 or #f)
)
|#
(define-syntax explode
    (syntax-rules (to or)
        (
            (_ alist to)
            (begin)
        )
        (
            (_ alist to (name or default))
            (define name (cdr (or (assoc 'name alist) (cons 'name default))))
        )
        (
            (_ alist to name)
            (define name (cdr (or (assoc 'name alist) (error "explode: missing key" 'name))))
        )
        (
            (_ alist to head rest ...)
            (begin
                (explode alist to head)
                (explode alist to rest ...)
            )
        )
    )
)
