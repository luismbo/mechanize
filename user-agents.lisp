(in-package :mechanize)

(defparameter *user-agent-aliases*
  `(:mechanize ,(format nil "Mechanize~@[/~A~] (~A~@[ ~A~]; ~A;~@[ ~A;~] https://github.com/luismbo/mechanize/)"
                        (ignore-errors
                         (asdf:component-version (asdf:find-system :mechanize)))
                        (lisp-implementation-type)
                        (lisp-implementation-version)
                        (software-type)
                        (software-version))
    :firefox-linux #1="Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.2.1) Gecko/20100122 firefox/3.6.1"
    :firefox-mac "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6; en-US; rv:1.9.2) Gecko/20100115 Firefox/3.6"
    :firefox #1#
    :ie6 "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1)"
    :ie7 "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 1.1.4322; .NET CLR 2.0.50727)"
    :ie8 "Mozilla/5.0 (compatible; MSIE 8.0; Windows NT 5.1; Trident/4.0; .NET CLR 1.1.4322; .NET CLR 2.0.50727)"
    :ie9 "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0)"
    :iphone "Mozilla/5.0 (iPhone; U; CPU like Mac OS X; en) AppleWebKit/420+ (KHTML, like Gecko) Version/3.0 Mobile/1C28 Safari/419.3"
    :konqueror "Mozilla/5.0 (compatible; Konqueror/3; Linux)"
    :mozilla-linux "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.4) Gecko/20030624"
    :mozilla-mac "Mozilla/5.0 (Macintosh; U; PPC Mac OS X Mach-O; en-US; rv:1.4a) Gecko/20030401"
    :mozilla-win #2="Mozilla/5.0 (Windows; U; Windows NT 5.0; en-US; rv:1.4b) Gecko/20030516 Mozilla Firebird/0.6"
    :mozilla #2#
    :safari "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_7_2) AppleWebKit/534.51.22 (KHTML like Gecko) Version/5.1.1 Safari/534.51.22"
    :safari4 "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_6_2; de-at) AppleWebKit/531.21.8 (KHTML like Gecko) Version/4.0.4 Safari/531.21.10"
    ))

(defun user-agent-aliases ()
  (loop for (key value) on *user-agent-aliases* by #'cddr collect key))

(defun find-user-agent-string (alias)
  (or (getf *user-agent-aliases* alias)
      (error "Couldn't find user agent: ~S" alias)))
