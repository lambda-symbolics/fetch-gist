;;; fetch-gist --- fetch a web page and convert its HTML to Markdown. -*- Mode: Lisp; -*-
(asdf:defsystem #:fetch-gist
  :description "Fetch a web page over HTTP and convert its HTML body to Markdown."
  :version     "0.1.0"
  :author      "fetch-gist contributors"
  :license     "MIT"
  :serial      t
  :depends-on  (#:dexador #:plump)
  :components  ((:module     "src"
                 :serial     t
                 :components ((:file "package")
                              (:file "fetch")
                              (:file "markdown")
                              (:file "cli"))))
  :in-order-to ((test-op (test-op #:fetch-gist/tests))))

(asdf:defsystem #:fetch-gist/tests
  :description "Test suite for fetch-gist."
  :author      "fetch-gist contributors"
  :license     "MIT"
  :serial      t
  :depends-on  (#:fetch-gist
                #:rove)
  :components  ((:module     "tests"
                 :serial     t
                 :components ((:file "package")
                              (:file "suite")
                              (:file "fetch")
                              (:file "markdown"))))
  :perform     (asdf:test-op (operation component)
                 (declare (ignore operation))
                 (uiop:symbol-call '#:rove '#:run component)))
