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
   ;; Agent
   #:*agent*
   #:agent
   #:user-agent-aliases
   #:get
   #:reload
   #:back
   #:last-response
   #:saving-excursion

   ;; Responses
   #:response
   #:get-header
   #:response-header
   #:status-code-of
   #:method-of
   #:uri-of
   #:status-of
   #:headers-of
   #:content-of

   #:file
   #:page
   #:html-page
   #:xml-page
   #:xhtml-page

   ;; Parsing and Querying
   #:form
   #:forms
   #:link
   #:links
   #:html-element
   #:query
   #:xquery
   #:inner-text
   ))

(defpackage :mechanize-user
  (:use :cl :mechanize)
  (:shadowing-import-from :mechanize #:get))
