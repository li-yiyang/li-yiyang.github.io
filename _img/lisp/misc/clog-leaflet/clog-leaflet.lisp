(defpackage #:clog-leaflet
  (:use #:cl #:clog)
  (:export init-leaflet))

(in-package :clog-leaflet)

;;; Implementation - Load Leaflet Lib

(defparameter *leaflet-css-path*
  "https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"
  "Path/URL to Leaflet CSS file.")

(defparameter *leaflet-js-path*
  "https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"
  "Path/URL to Leaflet Javascript file.")

(defparameter *leaflet-namespace*
  "window.LeafletNameSpace"
  "Name Space to store the Leaflet objects.")

(defun init-leaflet (body)
  "Load the Leaflet Javascripts and opens Leaflet Namespace. 
Only called once on first attach."
  (check-type body clog:clog-obj)
  (load-css    (html-document body)
               *leaflet-css-path* :load-only-once T)
  (load-script (html-document body)
               *leaflet-js-path*  :load-only-once T :wait-for-load NIL)
  (js-execute body (format NIL "~A = {};" *leaflet-namespace*)))

;;; Helper Function

(defgeneric ->js (obj)
  (:documentation "Convert basic Common Lisp Datatype into JS data."))

(defmethod ->js (obj)
  "Default will be simply FORMAT function."
  (format NIL "~A" obj))

(defmethod ->js ((obj string))
  "Trun String into JS String."
  (format NIL "~S" obj))

(defmethod ->js ((obj list))
  "Trun List into JS Array."
  (format NIL "[~A]"
          (reduce (lambda (converted new)
                    (format NIL "~A, ~A" converted  (->js new)))
                  (rest obj) :initial-value (->js (first obj)))))

(defun plist->js (plst)
  "Convert plist (:p v) into JS code {p: v}."
  (declare (list plst))
  (labels ((iter (lst)
             (if (null lst)
                 ""
                 (format NIL "~A: ~A, ~A"
                         (string-downcase (string (first lst)))
                         (->js (second lst))
                         (iter (cddr lst))))))
    (format NIL "{~A}" (iter plst))))

;;; Implementation - clog-leaflet-map

(defclass clog-leaflet-map (clog-div)
  ((options :initarg :options
            :initform '(:latitute 0 :longtitute 0 :zoom 1)
            :reader options))
  (:documentation "Leaflet map Object. "))

(defgeneric create-leaflet-map (clog-obj &key options style class html-id)
  (:documentation "Create a new clog-leaflet-map as child of CLOG-OBJ."))

(defmethod create-leaflet-map ((obj clog:clog-obj)
                               &key (options '(:zoom 13
                                               :center (0 0)))
                                 (style NIL)
                                 (class NIL)
                                 (html-id NIL))
  (let ((new-div (create-div obj :html-id html-id
                                 :class class
                                 :style style)))
    (setf (width new-div) "400px"
          (height new-div) "400px")
    (attach-leaflet-map new-div options)
    (change-class new-div 'clog-leaflet-map)))

(defun attach-leaflet-map (obj options)
  "Attach Leaflet to OBJ, which should be clog-leaflet-map object."
  (let ((id (html-id obj)))
    (js-execute obj (format NIL "~A['MAP~A'] = L.map('~A', ~A);"
                            *leaflet-namespace* id id (plist->js options)))))

;;; Helper functions for Leaflet map
(defmethod ->js ((obj clog-leaflet-map))
  "Turn OBJ into JS variable name."
  (format NIL "~A['MAP~A']" *leaflet-namespace* (html-id obj)))
