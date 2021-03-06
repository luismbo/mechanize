(in-package :mechanize)

(defclass html-element (cxml-dom::element) ())
(defclass form (html-element) ())

(defclass link (html-element) ())

(defmethod href-of ((link link))
  (dom:get-attribute link "href"))

(defparameter *text-snippet-length* 15)
(defparameter *text-snippet-marker* "...")

(defun string-snippet (string)
  (let ((snippet (subseq string 0 (min *text-snippet-length* (length string))))
        (marker-length (length *text-snippet-marker*)))
    (if (> (length string) (- *text-snippet-length* marker-length))
        (replace snippet *text-snippet-marker*
                 :start1 (- (length snippet) marker-length))
        snippet)))

(defmethod print-object ((object html-element) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A ~S"
            (dom:tag-name object)
            (string-snippet (inner-text object)) )))

(defmethod print-object ((object link) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S ~A"
            (string-snippet (inner-text object))
            (href-of object))))

(defmethod action-of ((object form))
  (dom:get-attribute object "action"))

(defmethod method-of ((object form))
  (let ((method (dom:get-attribute object "method")))
    (if (string= "" method)
        :get
        (make-keyword (string-upcase method)))))

(defmethod name-of ((object form))
  (dom:get-attribute object "name"))

(defmethod print-object ((object form) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A => ~A ~A"
            (name-of object)
            (method-of object)
            (action-of object))))

(defmethod describe-object ((form form) stream)
  (format stream "Form: ~A~%" (name-of form))
  (format stream "Action: ~A ~A~%" (method-of form) (action-of form))
  (terpri stream)
  (dolist (input (query "input" form))
    (format stream "   ~S ~S ~S~%"
            (dom:get-attribute input "type")
            (dom:get-attribute input "name")
            (dom:get-attribute input "value"))))

(defun process-html-document (document)
  (iter (for el :in-dom document)
    (when (dom:element-p el)
      (switch (el :key #'dom:tag-name :test #'string-equal)
        ("form" (change-class el 'form))
        ("a" (change-class el 'link))
        (t (change-class el 'html-element)))))
  document)

;;; using DOM instead of STP for compatibility with css-selectors.
(defun parse-response (response)
  (let ((subtype (nth-value 1 (drakma:get-content-type (headers-of response)))))
    (cond ((string-equal subtype "html")
           (values (process-html-document
                    (chtml:parse (content-of response)
                                 (cxml-dom:make-dom-builder)))
                   'html-page))
          ((member subtype
                   '("xml" "xhtml+xml" "vnd.wap.xhtml+xml")
                   :test #'string-equal)
           (values (cxml:parse (content-of response)
                               (cxml-dom:make-dom-builder))
                   'xml-page)))))

(defun inner-text (object &optional splice stream)
  (let ((object* (etypecase object
                   (page (content-of object))
                   ((or dom:element dom:document list) object))))
    (buildnode:text-of-dom-snippet object* splice stream)))
