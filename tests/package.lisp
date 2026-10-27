(defpackage #:fetch-gist.tests
  (:documentation "The fetch-gist test suite.")
  (:use #:cl)
  (:import-from #:fetch-gist
                #:html-content-type-p
                #:markdown-content-type-p
                #:plain-text-content-type-p
                #:markdown-from-string
                #:markdown-from-file
                #:markdown-from-url
                #:fetch-error-url
                #:fetch-status-error
                #:fetch-status-error-status
                #:fetch-content-type-error
                #:fetch-content-type-error-content-type)
  (:import-from #:fetch-gist.markdown
                #:html->markdown
                #:markdown-source-error)
  (:import-from #:rove
                #:deftest
                #:ok
                #:signals))
