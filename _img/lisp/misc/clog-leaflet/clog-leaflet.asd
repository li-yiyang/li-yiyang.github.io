;;; clog-leaflet.asd

(asdf:defsystem #:clog-leaflet
  :description "The CLOG wrapper for Leaflet."
  :author "凉凉 <https://github.com/li-yiyang>"
  :license "MIT"
  :version "0.0.0"
  :depends-on (#:clog)
  :components ((:file "clog-leaflet")))
