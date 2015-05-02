(defsystem :mechanize
  :depends-on (:alexandria
               :drakma
               :cxml
               :cxml-dom
               :closure-html
               :cl-ppcre
               :puri
               :iterate
               :buildnode
               :css-selectors
               :trivial-features
               :trivial-mimes
               :uiop
               :xpath)
  :serial t
  :components
  ((:file "package")
   (:file "history")
   (:file "user-agents")
   (:file "parsing")
   (:file "querying")
   (:file "mechanize")))
