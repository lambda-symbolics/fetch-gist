(in-package #:fetch-gist)

(defparameter *fetch-timeout* 10
  "The read timeout, in seconds, used for HTTP requests.")

(defparameter *fetch-accept-header*
  "text/markdown, text/plain;q=0.9, text/html;q=0.9, application/xhtml+xml;q=0.9"
  "The media types requested from HTTP servers.")

(defun fetch-url (url)
  "Fetch URL and return the Dexador response values.

The values are the response body, status code, headers, and final URI. The
body is always decoded to a string using its response charset."
  (dex:get url
           :headers `(("Accept" . ,*fetch-accept-header*))
           :read-timeout *fetch-timeout*
           :force-string t))

(defun fetch-url-to-string (url)
  "Fetch URL and return its decoded response body as a string."
  (nth-value 0 (fetch-url url)))

(defun content-type-media-type (content-type)
  (when content-type
    (string-downcase
     (string-trim '(#\Space #\Tab)
                  (subseq content-type
                          0 (or (position #\; content-type)
                                (length content-type)))))))

(defun html-content-type-p (content-type)
  "Return true when CONTENT-TYPE identifies an HTML response."
  (member (content-type-media-type content-type)
          '("text/html" "application/xhtml+xml")
          :test #'string=))

(defun markdown-content-type-p (content-type)
  "Return true when CONTENT-TYPE explicitly identifies Markdown."
  (member (content-type-media-type content-type)
          '("text/markdown" "text/x-markdown" "application/markdown")
          :test #'string=))

(defun plain-text-content-type-p (content-type)
  "Return true when CONTENT-TYPE identifies plain text."
  (member (content-type-media-type content-type)
          '("text/plain")
          :test #'string=))

(defun response-header (headers name)
  (gethash (string-downcase name) headers))

(defun markdown-from-string (html)
  "Convert an HTML string to Markdown."
  (fetch-gist.markdown:html->markdown html))

(defun markdown-from-file (pathname)
  "Read PATHNAME as HTML and convert it to Markdown."
  (markdown-from-string (uiop:read-file-string pathname)))

(defun markdown-from-url (url)
  "Fetch URL and return its Markdown representation.

Explicitly served Markdown and plain text are returned unchanged. HTML is
converted; responses with other content types are rejected."
  (multiple-value-bind (body status headers uri)
      (fetch-url url)
    (declare (ignore uri))
    (unless (<= 200 status 299)
      (error "Fetching ~A returned HTTP status ~D" url status))
    (let ((content-type (response-header headers "content-type")))
      (cond
        ((markdown-content-type-p content-type) body)
        ((plain-text-content-type-p content-type) body)
        ((html-content-type-p content-type) (markdown-from-string body))
        (t
           (error "Unsupported content type for ~A: ~A" url content-type))))))
