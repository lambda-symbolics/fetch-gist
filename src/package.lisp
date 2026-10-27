;;;; package.lisp --- package definition for fetch-gist. -*- Mode: Lisp; -*-
(defpackage #:fetch-gist
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
           #:*inline-elements*
           #:*block-elements*))

(defpackage #:fetch-gist.cli
  (:use #:cl #:fetch-gist #:fetch-gist.markdown)
  (:export #:main))
