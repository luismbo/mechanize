(defsystem :mechanize-tests
  :depends-on (:mechanize
               :toot
               :hu.dwim.stefil)
  :components
  ((:module "tests"
    :serial t
    :components
    ((:file "package")
     (:file "foo")))))
