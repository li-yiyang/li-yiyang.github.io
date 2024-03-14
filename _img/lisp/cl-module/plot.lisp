(defpackage #:cl-module/plot
  (:use :clim :clim-lisp)
  (:export #:init-plot
           #:plot-fill-rect
           #:plot-size-rect
           #:clear-plot
           #:pen-width
           #:plot-frame-rect
           #:plot-line
           #:show-plot
           #:plot-string
           #:plot-string-bold
           #:plot-string-italic
           #:plot-mouse-down))

(in-package :cl-module/plot)

(define-application-frame plot ()
  ()
  (:panes 
   (display :application
            :min-width 100 :min-height 100
            :width     400 :height     400))
  (:layouts 
   (:default
    (vertically () display))))

(define-plot-command (com-quit :menu t) ()
  (frame-exit *application-frame*))

(define-plot-command (com-clear :menu t) ()
  (window-clear (get-frame-pane *application-frame* 'display)))

(defmacro with-in-plot-pane ((pane &key (coordinate :origin)) &body body)
  "Bind var `pane' with `*plot-frame*' display pane.
  The `coordinate' could be:
  + `:origin' (default) for top-left corner positioned
  + `:local' for centered positioned"
  (if (eq coordinate :local)
      (alexandria:with-gensyms (w h)
        `(let* ((,pane (get-frame-pane *plot-frame* 'display)))
           (multiple-value-bind (,w ,h)
               (window-inside-size ,pane)
             (with-translation (,pane (round ,w 2) (round ,h 2))
               ,@body))))
      `(let ((,pane (get-frame-pane *plot-frame* 'display)))
         ,@body)))

;; The global plot window object
(defparameter *plot-frame* nil
  "The graphic frame to plot.")

;; Initialize a standard plot window
(defun init-plot (&key (width 400) (height 400))
  "Creates a graphics window, return a window stream."
  (setf *plot-frame*
        (make-application-frame 'plot :width width :height height)))

(defun show-plot (&key (background t) (name "PLOT"))
  "Open the `*plot-frame*' window."
  (unless *plot-frame*
    (warn "Plot not initilized yet, auto inited.")
    (init-plot))
  (flet ((run () (run-frame-top-level *plot-frame*)))
    (if background
        (bt:make-thread #'run
                        :name name
                        :initial-bindings
                        `((*default-server-path* . ',*default-server-path*)))
        (run))))

(defun clear-plot ()
  "Clears the graphics window."
  (window-clear (get-frame-pane *plot-frame* 'display)))

;; Colors: use CLIM color system

;; Graphing Settings
(defparameter *default-pen-width* 1
  "Default pen witdth (unit by px).")

(defun pen-width (px)
  "Sets the pen drawing width."
  (setf *default-pen-width* px))

;; Graphing Methods
(defun plot-fill-rect (x y xsize ysize color)
  "Fills a rectangle with `color'."
  (with-in-plot-pane (pane)
    (draw-rectangle* pane x y (+ x xsize) (+ y ysize)
                     :ink color :filled t
                     :line-thickness 0)))

(defun plot-size-rect (x y xsize ysize max-width &key (color +black+))
  "Plots a rectangle with `max-width'."
  (let* ((width (min xsize max-width))
         (scale (/ width xsize)))
    (with-in-plot-pane (pane)
      (draw-rectangle* pane x y (+ x (* scale xsize)) (+ y (* scale ysize))
                       :line-thickness 0 :ink color))))

(defun plot-frame-rect (x y xsize ysize
                        &key (color +black+) (pen-width *default-pen-width*))
  "Plots a framed rectangle."
  (with-in-plot-pane (pane)
    (draw-rectangle* pane x y (+ x xsize) (+ y ysize)
                     :line-thickness pen-width
                     :ink color
                     :filled nil)))

(defun plot-line (x1 y1 x2 y2
                  &key (color +black+) (pen-width *default-pen-width*))
  "Plots a line between two points."
  (with-in-plot-pane (pane)
    (draw-line* pane x1 y1 x2 y2
                :line-thickness pen-width
                :ink color)))

(defun plot-string (x y str &key (size 10))
  "Plots a string at position (x y)."
  (with-in-plot-pane (pane)
    (draw-text* pane str x y :text-size size)))

(defun plot-string-bold (x y str &key (size 10) (color +black+))
  "Plots a bold string at posistion (x y). "
  (with-in-plot-pane (pane)
    (draw-text* pane str x y
                :text-face :bold
                :text-size size
                :ink color)))

(defun plot-string-italic (x y str &key (size 10) (color +black+))
  "Plots a italic string at posistion (x y). "
  (with-in-plot-pane (pane)
    (draw-text* pane str x y
                :text-face :italic
                :text-size size
                :ink color)))

;; Inputs
(defun plot-mouse-down ()
  "Returns position of mouse click."
  (with-in-plot-pane (pane)
    (tracking-pointer (pane)
      (:pointer-button-press
       (event)
       (return
         (values (pointer-event-x event)
                 (pointer-event-y event)))))))
