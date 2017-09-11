; This module (and the API exported by this module) is licensed under BSD 3.
; but the (gnome *) modules are licensed under GPL 2,
; so the end app has to be licensed under GPL 2.

(define-module (language gui gtk)
    #:replace
    (

        ; main loop
        main-loop
        main-quit

        ; widget creation
        window
        button
        vertical

        ; widget hierarchy
        adopt
        close ; clashes with guile core
        show
        show-all

        ; widget attributes
        set-outer-padding
        set-label ; clashes with (gnome gtk)
        set-spacing
        set-title

        ; widget event handling
        on-close
        on-click

    )
)

(use-modules
    (oop goops)
    (gnome gobject)
    (gnome gtk)
)

; Guile has defined `quit` already.
(define (main-loop)
    "Call the main loop. Use `main-quit` to tell the main loop to return."
    (gtk-main)
)

(define (main-quit)
    "Quit the main loop."
    (gtk-main-quit)
)

(define (window)
    "Create a visible window."
    (define instance (make <gtk-window> #:type 'toplevel))
    instance
)

(define (button)
    "Create a button."
    (define instance (make <gtk-button>))
    instance
)

(define (vertical)
    "Create a container that lays out its children vertically (from top to bottom)."
    (define instance (make <gtk-vbox>))
    instance
)

(define (adopt parent child)
    "`(adopt parent child)` adds `child` to `parent`."
    (gtk-container-add parent child)
)

(define (close widget)
    (gtk-widget-destroy widget)
)

(define (show widget)
    "Show a widget, but not its descendants. See also `show-all`."
    (gtk-widget-show widget)
)

; GTK widgets are invisible when they are just created.
(define (show-all widget)
    "Show a widget and all its descendants recursively."
    (gtk-widget-show-all widget)
)

(define (set-outer-padding widget pixel)
    "This is like CSS margin, but this does not collapse."
    (gtk-container-set-border-width widget pixel)
)

(define (set-label widget text)
    (gtk-button-set-label widget text)
)

(define (set-spacing widget pixel)
    (gtk-box-set-spacing widget pixel)
)

(define (set-title widget text)
    (gtk-window-set-title widget text)
)

(define (on-close widget handler)
    "Set what to do when the widget is closed. The `handler` parameter is a lambda accepting one parameter: the widget."
    (gtype-instance-signal-connect widget 'destroy (lambda (widget) (handler widget)))
)

(define (on-click widget handler)
    (gtype-instance-signal-connect widget 'clicked (lambda (widget) (handler widget)))
)
