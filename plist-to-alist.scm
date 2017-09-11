(define (plist->alist lst)
    "Transform a property list to an association list."
    (define (length>=2 lst) (and (pair? lst) (pair? (cdr lst))))
    (cond
        ((length>=2 lst)
            (let
                (
                    (key (car lst))
                    (value (cadr lst))
                    (rest (cddr lst))
                )
                (cons
                    (cons key value)
                    (plist->alist rest)
                )
            )
        )
        (else lst)
    )
)
