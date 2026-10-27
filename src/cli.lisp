(in-package #:fetch-gist.cli)

(defun usage (&optional (stream *standard-output*))
  "Write the command line usage summary to STREAM."
  (format stream "Usage: fetch-gist <url>~%")
  (format stream "       fetch-gist --help~%"))

(defun main (&rest args)
  "Write the Markdown for the one URL in ARGS to standard output, then quit.

With no arguments, or with help, --help, or -h, print the usage instead.
Exit with status 1 when the arguments are wrong or the fetch fails."
  (cond
    ((or (null args)
         (member (first args) '("help" "--help" "-h") :test #'string=))
     (usage (if (null args) *error-output* *standard-output*))
     (uiop:quit (if (null args) 1 0)))
    ((not (null (rest args)))
     (format *error-output* "fetch-gist: expected one URL~%")
     (usage *error-output*)
     (uiop:quit 1))
    (t
     (handler-case
         (write-string (markdown-from-url (first args)))
       (error (condition)
         (format *error-output* "fetch-gist: ~A~%" condition)
         (uiop:quit 1))))))
