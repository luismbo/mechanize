(in-package :mechanize)

(defparameter *default-maximum-history-size* 10)

(defclass history ()
  ((maximum-size :initform *default-maximum-history-size*
                 :initarg :maximum-size :accessor maximum-size-of)
   (stack :initform nil :accessor stack-of)))

#+TODO
(defmethod print-object ((self history) stream)
  (print-unreadable-object (self stream :type t)
    ))

#+TODO
(defmethod describe-object ((self history) stream))

(defmethod pop-page ((history history))
  (pop (stack-of history)))

(defmethod last-page ((history history))
  (first (stack-of history)))

(defmethod last-pages ((history history) n)
  (loop repeat n
        for el in (stack-of history)
        collect el))

(defmethod push-page ((history history) page)
  (setf (stack-of history)
        (cons page (last-pages history (1- (maximum-size-of history))))))

(defmethod visitedp ((history history) page)
  (member page (stack-of history)))

(defmethod visitedp ((history history) (uri puri:uri))
  (member uri (stack-of history) :key #'uri-of))

(defmethod visitedp ((history history) (uri string))
  (visitedp history (puri:uri uri)))

(defmethod clear ((history history))
  (setf (stack-of history) nil))

(defun copy-history (history)
  (make-instance 'history
                 :maximum-size (maximum-size-of history)
                 :history (copy-list (stack-of history))))
