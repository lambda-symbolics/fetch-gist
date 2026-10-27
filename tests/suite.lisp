(in-package #:fetch-gist.tests)

(defvar *fixture-directory*
  (merge-pathnames "tests/fixtures/"
                    (asdf:system-source-directory "fetch-gist/tests"))
  "The directory that holds the HTML and Markdown test fixtures.")

(defun read-fixture (filename)
  "Return the contents of the fixture file FILENAME as a string."
  (uiop:read-file-string (merge-pathnames filename *fixture-directory*)))

(defun convert-fixture (name)
  "Return the Markdown for the HTML fixture NAME, given without its .html type."
  (html->markdown (read-fixture (format nil "~A.html" name))))

(defun lines (&rest strings)
  "Return STRINGS joined by newlines, with a final newline."
  (format nil "~{~A~^~%~}~%" strings))
