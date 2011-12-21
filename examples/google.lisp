(in-package :mechanize-user)

(defun google ()
  (with-agent (agent :user-agent :safari)
    (get "http://google.com")
    (submit (form :name "f") :q "Common Lisp")
    (links)))
