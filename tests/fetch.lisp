(in-package #:fetch-gist.tests)

(deftest content-type/html-is-recognized
  (ok (html-content-type-p "text/html; charset=utf-8"))
  (ok (html-content-type-p "application/xhtml+xml"))
  (ok (not (html-content-type-p "text/plain"))))

(deftest content-type/markdown-is-recognized-explicitly
  (ok (markdown-content-type-p "text/markdown; charset=utf-8"))
  (ok (markdown-content-type-p "application/markdown"))
  (ok (not (markdown-content-type-p "text/plain"))))

(deftest content-type/plain-text-is-recognized
  (ok (plain-text-content-type-p "text/plain"))
  (ok (plain-text-content-type-p "text/plain; charset=utf-8"))
  (ok (not (plain-text-content-type-p "text/html"))))

(deftest markdown-from-url/converts-fetched-html
  (let ((original-fetch-url (symbol-function 'fetch-gist:fetch-url))
        (headers            (make-hash-table :test #'equal)))
    (setf (gethash "content-type" headers) "text/html; charset=utf-8")
    (unwind-protect
         (progn
           (setf (symbol-function 'fetch-gist:fetch-url)
                 (lambda (url)
                   (declare (ignore url))
                   (values (read-fixture "pages/example-documentation.html")
                           200 headers nil)))
           (ok (string= (markdown-from-url "https://example.test/documentation")
                        (read-fixture "pages/example-documentation.md"))))
      (setf (symbol-function 'fetch-gist:fetch-url) original-fetch-url))))

(deftest markdown-from-url/returns-plain-text-unchanged
  (let ((original-fetch-url (symbol-function 'fetch-gist:fetch-url))
        (headers            (make-hash-table :test #'equal)))
    (setf (gethash "content-type" headers) "text/plain; charset=utf-8")
    (unwind-protect
         (progn
           (setf (symbol-function 'fetch-gist:fetch-url)
                 (lambda (url)
                   (declare (ignore url))
                   (values "# llms.txt sample body" 200 headers nil)))
           (ok (string= (markdown-from-url "https://example.test/llms.txt")
                        "# llms.txt sample body")))
      (setf (symbol-function 'fetch-gist:fetch-url) original-fetch-url))))

(deftest markdown-from-url/signals-the-failed-status
  (let ((original-fetch-url (symbol-function 'fetch-gist:fetch-url))
        (headers            (make-hash-table :test #'equal)))
    (setf (gethash "content-type" headers) "text/html")
    (unwind-protect
         (progn
           (setf (symbol-function 'fetch-gist:fetch-url)
                 (lambda (url)
                   (declare (ignore url))
                   (values "" 304 headers nil)))
           (let ((condition (handler-case (markdown-from-url "https://example.test/cached")
                              (fetch-status-error (condition)
                                condition))))
             (ok (typep condition 'fetch-status-error))
             (ok (= (fetch-status-error-status condition) 304))
             (ok (string= (fetch-error-url condition) "https://example.test/cached"))))
      (setf (symbol-function 'fetch-gist:fetch-url) original-fetch-url))))

(deftest markdown-from-url/signals-an-unsupported-content-type
  (let ((original-fetch-url (symbol-function 'fetch-gist:fetch-url))
        (headers            (make-hash-table :test #'equal)))
    (setf (gethash "content-type" headers) "image/png")
    (unwind-protect
         (progn
           (setf (symbol-function 'fetch-gist:fetch-url)
                 (lambda (url)
                   (declare (ignore url))
                   (values "" 200 headers nil)))
           (let ((condition (handler-case (markdown-from-url "https://example.test/logo.png")
                              (fetch-content-type-error (condition)
                                condition))))
             (ok (typep condition 'fetch-content-type-error))
             (ok (string= (fetch-content-type-error-content-type condition) "image/png"))))
      (setf (symbol-function 'fetch-gist:fetch-url) original-fetch-url))))
