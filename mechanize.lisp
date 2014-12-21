(in-package :mechanize)

;;;; Main Agent Class

(defclass agent ()
  ((user-agent
    :initarg :user-agent
    :accessor user-agent-of)
   ;; holds the last few PAGEs.
   (history
    :initform (make-instance 'history)
    :reader history-of)
   (cookie-jar
    :initarg :cookie-jar
    :initform (make-instance 'drakma:cookie-jar)
    :accessor cookie-jar-of))
  (:default-initargs :user-agent :mechanize))

(defmethod initialize-instance :after ((self agent) &key user-agent)
  (when (keywordp user-agent)
    (setf (user-agent-of self) (find-user-agent-string user-agent))))

(defmacro with-agent ((name &rest initargs) &body body)
  `(let* ((*agent* (make-instance 'agent ,@initargs))
          (,name *agent*))
     ,@body))

;;; we can get non-page responses. Either save those too, somewhere,
;;; or change this to LAST-PAGE.
(defun last-response (&optional (agent *agent*))
  (last-page (history-of agent)))

(defun call-with-saved-excursion (agent fn)
  (with-slots (history) agent
    (let ((current-history history))
      (setf history (copy-history current-history))
      (unwind-protect
           (funcall fn)
        (setf history current-history)))))

(defmacro saving-excursion ((&optional (agent '*agent*)) &body body)
  `(call-with-saved-excursion ,agent (lambda () ,@body)))

;;; FIXME: messed up history
(defun back (&optional (n 1) (agent *agent*))
  (let ((page (loop repeat (1+ n)
                    for last = (pop-page (history-of agent))
                    finally (return last))))
    (agent-request agent (method-of page) (uri-of page))))

(defun reload (&optional (agent *agent*))
  (let ((page (last-response agent)))
    (agent-request agent (method-of page) (uri-of page))))


;;;; Responses

(defclass response ()
  ((uri :initarg :uri :reader uri-of)
   (method :initarg :method :reader method-of)
   (status-code :initarg :status-code :reader status-code-of)
   (status :initarg :status :reader status-of)
   (headers :initarg :headers :reader headers-of)
   (content :initarg :content :reader content-of)))

(defmethod print-object ((object response) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (status-code status uri) object
      (format stream "[~A ~A] ~A" status-code status uri))))

(defmethod response-header ((response response) name &key (test #'equalp))
  (cdr (assoc name (headers-of response) :test test)))

(defun get-header (name &key (response (last-response)) (test #'equalp))
  (response-header response name :test test))

;;; Not yet convinced about the usefulness of all this... Is it just a
;;; sorry excuse to use CHANGE-CLASS? :-)
(defclass file (response) ())
(defclass page (file) ())
(defclass html-page (page) ())
(defclass xml-page (page) ())
(defclass xhtml-page (xml-page html-page) ())

(defmethod query-object ((object page) method query)
  (query-object (dom:first-child (content-of object)) method query))


;;;; Agent Operations

(defparameter *agent* (make-instance 'agent))

#+todo
(define-condition http-response-error (simple-error)
  ())

;; TODO: reuse connection, history
(defun agent-request (agent method uri &rest drakma-args)
  ;; FIXME: do want a black- or a white-list?
  (assert (not (getf drakma-args :method)) ()
          "Method is already implied, don't specify another one.")
  (multiple-value-bind (body status-code headers uri* stream closep status)
      (apply #'drakma:http-request
             uri
             :method method
             :cookie-jar (cookie-jar-of agent)
             :additional-headers (when (last-response agent)
                                   (acons "Referer" (uri-of (last-response agent))
                                          nil))
             ;; give precedence to (some) user-specified args.
             (append drakma-args
                     (list :user-agent (user-agent-of agent)
                           :auto-referer t)))
    (declare (ignore stream closep))
    ;; TODO: streamed responses. (to e.g. save directly to disk)
    (let ((response (make-instance 'response
                                   :uri uri*
                                   :method method
                                   :content body
                                   :headers headers
                                   :status-code status-code
                                   :status status)))
      (multiple-value-bind (parsed-content new-response-class)
          (parse-response response)
        (when parsed-content
          (setf (slot-value response 'content) parsed-content)
          (change-class response new-response-class)))
      (when (typep response 'page)
        (push-page (history-of agent) response))
      response)))

;;; TODO: maybe define a macro that shows the extra drakma options we
;;; allow in the arguments?
(defun get (uri &rest args &key (agent *agent*) &allow-other-keys)
  (remf args :agent)
  (apply #'agent-request agent :get uri args))

(defun post (uri &rest args &key (agent *agent*) &allow-other-keys)
  (remf args :agent)
  (apply #'agent-request agent :post uri args))

(defun absolute-uri (uri &key (agent *agent*) (page (last-response agent)))
  (puri:enough-uri uri (uri-of page)))

(defmethod click ((link link) &key (agent *agent*))
  (get (absolute-uri (href-of link) :agent agent)
       :agent agent))

(defmethod submit ((form form) parameters &key (agent *agent*))
  (let* ((default (mapcar (lambda (input)
                            (cons (dom:get-attribute input "name")
                                  (dom:get-attribute input "value")))
                          (query "input" form)))
         (custom (loop for (param value) on parameters by #'cddr
                       collect (cons (etypecase param
                                       (keyword (string-downcase param))
                                       (string param))
                                     value)))
         (parameters (remove-duplicates (append default custom)
                                        :test #'string= :key #'car))
         (uri (absolute-uri (action-of form) :agent agent)))
    (ecase (method-of form)
      (:get
       (setf (puri:uri-query uri)
             ;; FIXME: manage encodings.
             (drakma::alist-to-url-encoded-string parameters :latin-1))
       (get uri :agent agent))
      (:post
       (post uri :agent agent :parameters parameters)))))
