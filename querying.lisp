(in-package :mechanize)

(defun query (query &optional (object (last-response)))
  (query-object object :css query))

(defun xquery (query &optional (object (last-response)))
  (query-object object :xpath query))

(defmethod query-object ((object page) method query)
  (query-object (dom:first-child (content-of object)) method query))

(defmethod query-object (object (method (eql :css)) query)
  (css-selectors:query query object))

(defmethod query-object (object (method (eql :xpath)) query)
  (xpath:with-namespaces ((nil (dom:namespace-uri object)))
    (xpath:map-node-set->list #'identity (xpath:evaluate query object))))

(defun filter-elements (elements regex &key (key #'identity))
  (remove-if-not (lambda (el)
                   (ppcre:scan regex el))
                 elements
                 :key key))

(defun links (&key href text (node (last-response)))
  (let ((links (query "a" node)))
    (when text
      (setf links (filter-elements links text :key #'inner-text)))
    (when href
      (setf links (filter-elements links href :key #'href-of)))
    links))

(defun link (&key text href (node (last-response)))
  (let ((list (links :text text :href href :node node)))
    (unless (length= 1 list)
      (warn "Found ~S links matching :text ~S. Returning first."
            (length list) text))
    (first list)))

(defun forms (&key (node (last-response)))
  (query "form" node))
