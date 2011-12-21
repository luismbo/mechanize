(cl:in-package :cl-user)

(defpackage :mechanize
  (:use :cl :iterate)
  (:nicknames #:mech)
  (:shadow #:get)
  (:import-from :alexandria
                #:with-unique-names
                #:once-only
                #:switch)
  (:export
   #:*agent*
   #:user-agent-aliases
   #:get
   #:reload
   #:back))

(defpackage :mechanize-user
  (:use :cl :mechanize)
  (:shadowing-import-from :mechanize #:get))
