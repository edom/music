; https://www.gnu.org/software/guile-ncurses/manual/html_node/Using-the-Guile-Curses-module.html
(use-modules
    (ncurses curses)

    (native)
)

(slurp-file-s16 "drum-samples/99/Samples/kick-acoustic01.wav")
(slurp-file-s16 "drum-samples/99/Samples/snare-big.wav")
(slurp-file-s16 "drum-samples/99/Samples/crash-acoustic.wav")

#|
(define (main)
    (define stdscr (initscr))
    (addstr stdscr "Hello World!!!")
    (refresh stdscr)
    (getch stdscr)
    (endwin)
)

(main)
|#

#|
(use-modules (language gui gtk))

(define (main)
    (define w (window))
    (define v (vertical))
    (define b-0 (button))
    (define b-1 (button))
    (set-title w "Foo App")
    (on-close w (lambda (widget) (main-quit)))
    (set-label b-0 "Hello")
    (on-click b-0 (lambda (widget) (close w)))
    (set-label b-1 "World")
    (set-outer-padding v 16)
    (set-spacing v 8)
    (adopt v b-0)
    (adopt v b-1)
    (adopt w v)
    (show-all w)
    (main-loop)
)

(main)

(load "language/explode.scm")
(load "plist-to-alist.scm")

; TODO children should infer parent-id
(define window-spec
    '(
        id window-root
        type window
        title "This is a window title"
        on-close (begin
            (display "Quitting.")
            (newline)
            (quit)
        )
        children (
            (
                id vpaned-0
                parent-id window-root
                type vpaned
                children (
                    (
                        id button-close
                        parent-id vpaned-0
                        type button
                        label "Close"
                        on-click (begin
                            (destroy 'window-root)
                        )
                    )
                    (
                        id button-close-2
                        parent-id vpaned-0
                        type button
                        label "Close 2"
                        on-click (begin
                            (destroy 'window-root)
                        )
                    )
                )
            )
        )
    )
)

(define table-of-id (make-hash-table 64))

(define (interpret plist)
    (define alist (plist->alist plist))
    (explode alist to
        type
        (children or '())
        (id or #f)
        (parent-id or #f)
        (label or "Unlabeled")
        (title or "Untitled")
        ; how do we do nothing in Scheme?
        (on-click or #t)
        (on-close or #t)
    )
    (define (make-0 . args)
        (define instance (apply make args))
        (if id (hash-set! table-of-id id instance))
        (if parent-id (gtk-container-add (get-element-by-id parent-id) instance))
        instance
    )
    (define (get-element-by-id id)
        (or
            (hash-ref table-of-id id)
            (error "There is no element with id " id)
        )
    )
    (define (handler body)
        (eval
            `(lambda (widget)
                (define get-element-by-id ,get-element-by-id)
                (define (destroy what)
                    (cond
                        ((symbol? what)
                            (,gtk-widget-destroy (get-element-by-id what))
                        )
                        (else
                            (,gtk-widget-destroy what)
                        )
                    )
                )
                (define quit ,gtk-main-quit)
                ,body
                #t
            )
            (interaction-environment)
        )
    )
    (case type
        ((window)
            (let
                (
                    (instance (make-0 <gtk-window> #:type 'toplevel))
                )
                (gtype-instance-signal-connect instance 'hide (lambda (widget) (gtk-widget-destroy widget) #t))
                (gtype-instance-signal-connect instance 'destroy (handler on-close))
                (gtk-window-set-title instance title)
                (gtk-widget-show-all instance)
            )
        )
        ((button)
            (let
                (
                    (instance (make-0 <gtk-button> #:label label))
                )
                (gtype-instance-signal-connect instance 'clicked (handler on-click))
                (gtk-widget-show-all instance)
            )
        )
        ((vpaned)
            (let
                (
                    (instance (make-0 <gtk-vpaned>))
                )
                (gtk-widget-show-all instance)
            )
        )
        (else (error "Unknown type" type))
    )
    (for-each interpret children)
)

; https://www.gnu.org/software/guile-gnome/docs/gtk/html/Overview.html#Overview

(define (main)
    ;(define window (make <gtk-window> #:type 'toplevel))
    ;(define button (make <gtk-button> #:label "Hello, World!"))
    ;(gtk-container-set-border-width window 10)
    ;(gtk-container-add window button)
    ;(gtype-instance-signal-connect button 'clicked (lambda (b) (gtk-main-quit)))
    ;(gtk-widget-show-all window)
    (interpret window-spec)
    (gtk-main)
)

(main)

(display (plist->alist window-spec))
(newline)

|#
