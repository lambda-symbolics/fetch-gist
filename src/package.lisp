;;;; -- Packages --

(defpackage #:fetch-gist
  (:documentation "Fetch a web page over HTTP and return its body as Markdown.")
  (:use #:cl)
  (:export #:fetch-url
           #:fetch-url-to-string
           #:html-content-type-p
           #:markdown-content-type-p
           #:plain-text-content-type-p
           #:markdown-from-string
           #:markdown-from-file
           #:markdown-from-url
           #:*fetch-timeout*
           #:fetch-error
           #:fetch-error-url
           #:fetch-status-error
           #:fetch-status-error-status
           #:fetch-content-type-error
           #:fetch-content-type-error-content-type))

(defpackage #:fetch-gist.markdown
  (:documentation "Convert HTML parsed by Plump to Markdown.")
  (:use #:cl)
  (:import-from #:plump
                #:parse
                #:node-p
                #:nesting-node-p
                #:text-node-p
                #:element-p
                #:children
                #:text
                #:tag-name
                #:attributes)
  (:export #:html->markdown
           #:markdown-source-error
           #:markdown-source-error-source
           #:*inline-elements*
           #:*block-elements*))

(defpackage #:fetch-gist.cli
  (:documentation "The fetch-gist command line program.")
  (:use #:cl)
  (:import-from #:fetch-gist
                #:markdown-from-url)
  (:export #:main))
