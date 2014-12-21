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

(defun find-matching-node (object-kind finder criteria)
  (let ((matches (apply finder criteria)))
    (cond ((null matches)
           (error "couldn't find any ~As matching ~S."
                  object-kind criteria))
          ((not (length= 1 matches))
           (warn "found ~S ~As matching ~S. Returning first."
                 (length matches) object-kind criteria)))
    (first matches)))

(defun link (&rest criteria &key text href (node (last-response)))
  (declare (ignore text href node))
  (find-matching-node "link" #'links criteria))

(defun forms (&key action name method (node (last-response)))
  (let ((forms (query "form" node)))
    (when method
      (setf forms (remove method forms :key #'method-of :test-not #'eq)))
    (when action
      (setf forms (filter-elements forms action :key #'action-of)))
    (when name
      (setf forms (filter-elements forms name :key #'name-of)))
    forms))

(defun form (&rest criteria &key action name method (node (last-response)))
  (declare (ignore action name method node))
  (find-matching-node "form" #'forms criteria))
