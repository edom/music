(define-module (native)
    #:export (
        *max-sample-byte-count*
        *max-error-message-byte-count*
        slurp-file-s16
    )
)

(use-modules
    (rnrs bytevectors)
    (system foreign))

(define libnative (dynamic-link "libnative"))

(define slurp_file_s16
    (pointer->procedure int
        (dynamic-func "slurp_file_s16" libnative)
        (list '* '* size_t '* size_t '*)
    )
)

(define debug_print_to_stdout (dynamic-func "debug_print_to_stdout" libnative))

(define *max-sample-byte-count* (make-parameter 1048576))
(define *max-error-message-byte-count* (make-parameter 1024))
(define *debug-print* (make-parameter
    (if (getenv "Debug")
        debug_print_to_stdout
        %null-pointer)
    )
)

(define (slurp-file-s16 path)
"
Load a 1-channel audio sample file whose path is `path`.

Return a bytevector containing signed 16-bit two's complement integer samples.
"
    (define initial-element 0)
    (define buf-max (*max-sample-byte-count*))
    (define err-max (*max-error-message-byte-count*))
    (define buffer (make-bytevector buf-max initial-element))
    (define err-msg (make-bytevector err-max initial-element))
    (define debug-print (*debug-print*))
    (slurp_file_s16
        debug-print
        (bytevector->pointer err-msg) err-max
        (bytevector->pointer buffer) buf-max
        (string->pointer path)
    )
    (let
        (
            (message (pointer->string (bytevector->pointer err-msg)))
        )
        (if (equal? message "") buffer (error message))
    )
)
