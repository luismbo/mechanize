(cl:in-package :cl-user)

(defpackage :mechanize
  (:use :cl :iterate)
  (:nicknames #:mech)
  (:shadow #:get)
  (:import-from :alexandria
                #:length=
                #:make-keyword
                #:once-only
                #:switch
                #:with-unique-names)
  (:export
   ;; Agent
   #:*agent*
   #:absolute-uri
   #:agent
   #:back
   #:click
   #:get
   #:last-response
   #:post
   #:reload
   #:saving-excursion
   #:submit
   #:user-agent-aliases
   #:with-agent

   ;; Responses
   #:content-of
   #:get-header
   #:headers-of
   #:method-of
   #:response
   #:response-header
   #:status-code-of
   #:status-of
   #:uri-of

   #:file
   #:html-page
   #:page
   #:xhtml-page
   #:xml-page

   ;; Parsing and Querying
   #:action-of
   #:form
   #:forms
   #:href-of
   #:html-element
   #:inner-text
   #:link
   #:links
   #:method-of
   #:name-of
   #:query
   #:xquery))

(defpackage :mechanize-user
  (:use :cl :mechanize)
  (:shadowing-import-from :mechanize #:get))
