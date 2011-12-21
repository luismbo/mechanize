(in-package :mechanize)

(defclass html-element (cxml-dom::element) ())
(defclass form (html-element) ())
(defclass link (html-element) ())

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
