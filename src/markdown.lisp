(in-package #:fetch-gist.markdown)

(defparameter *inline-elements*
  '("a" "b" "code" "del" "em" "i" "s" "strong" "kbd"))

(defparameter *block-elements*
  '("blockquote" "div" "h1" "h2" "h3" "h4" "h5" "h6" "hr"
    "li" "ol" "p" "pre" "table" "ul"))

(defparameter *skipped-elements*
  '("head" "style" "script" "svg" "colgroup" "col" "caption" "template"))

(defstruct markdown-context
  stream
  last-character
  (newline-count 0)
  pending-space
  (pre-depth 0)
  (blockquote-depth 0)
  blockquote-started
  list-stack)

(defun emit-char (context character)
  (write-char character (markdown-context-stream context))
  (setf (markdown-context-last-character context) character
        (markdown-context-newline-count context)
        (if (char= character #\Newline)
            (1+ (markdown-context-newline-count context))
            0)))

(defun emit-string (context string)
  (loop for character across string do
    (emit-char context character)))

(defun ensure-newlines (context count)
  (when (markdown-context-last-character context)
    (loop while (< (markdown-context-newline-count context) count) do
      (emit-char context #\Newline))))

(defun begin-block (context)
  (setf (markdown-context-pending-space context) nil)
  (ensure-newlines context 2))

(defun end-block (context)
  (setf (markdown-context-pending-space context) nil)
  (ensure-newlines context 2))

(defun emit-text (context text)
  (if (plusp (markdown-context-pre-depth context))
      (emit-string context text)
      (loop for character across text do
        (if (find character '(#\Space #\Tab #\Newline #\Return))
            (setf (markdown-context-pending-space context) t)
            (progn
              (when (and (markdown-context-pending-space context)
                         (markdown-context-last-character context)
                         (not (char= (markdown-context-last-character context)
                                     #\Newline))
                         (not (char= (markdown-context-last-character context)
                                     #\Space)))
                (emit-char context #\Space))
              (emit-char context character)
              (setf (markdown-context-pending-space context) nil))))))

(defun flush-inline-space (context)
  (when (markdown-context-pending-space context)
    (emit-char context #\Space)
    (setf (markdown-context-pending-space context) nil)))

(defun indent-string (context)
  (make-string (* 2 (max 0 (1- (length (markdown-context-list-stack context)))))
               :initial-element #\Space))

(defun continuation-indent (context)
  (make-string (* 2 (length (markdown-context-list-stack context)))
               :initial-element #\Space))

(defmacro with-captured-output ((context) &body body)
  (let ((stream         (gensym "STREAM"))
        (captured       (gensym "CAPTURED"))
        (last-character (gensym "LAST-CHARACTER"))
        (newline-count  (gensym "NEWLINE-COUNT"))
        (pending-space  (gensym "PENDING-SPACE")))
    `(let ((,stream (markdown-context-stream ,context))
           (,last-character (markdown-context-last-character ,context))
           (,newline-count (markdown-context-newline-count ,context))
           (,pending-space (markdown-context-pending-space ,context)))
       (with-output-to-string (,captured)
         (setf (markdown-context-stream ,context) ,captured
               (markdown-context-last-character ,context) nil
               (markdown-context-newline-count ,context) 0
               (markdown-context-pending-space ,context) nil)
         (unwind-protect (progn ,@body)
           (setf (markdown-context-stream ,context) ,stream
                 (markdown-context-last-character ,context) ,last-character
                 (markdown-context-newline-count ,context) ,newline-count
                 (markdown-context-pending-space ,context) ,pending-space))))))

(defun node-children (node)
  (when (nesting-node-p node)
    (coerce (children node) 'list)))

(defun node-tag (node)
  (string-downcase (tag-name node)))

(defun node-attr (node name)
  (let ((attributes (attributes node)))
    (loop for key being the hash-keys of attributes
          when (string-equal key name)
            return (gethash key attributes))))

(defvar *element-handlers* (make-hash-table :test #'equal))

(defmacro define-element (tag (context node) &body body)
  `(setf (gethash ,tag *element-handlers*)
         (lambda (,context ,node)
           (declare (ignorable ,context ,node))
           ,@body)))

(defun render-node (context node)
  (cond
    ((text-node-p node)
     (emit-text context (text node)))
    ((element-p node)
     (render-element context node))
    ((nesting-node-p node)
     (render-children context node))
    (t
     nil)))

(defun render-children (context node)
  (dolist (child (node-children node))
    (render-node context child)))

(defun render-element (context node)
  (let ((handler (gethash (node-tag node) *element-handlers*)))
    (if handler
        (funcall handler context node)
        (render-children context node))))

(defun render-nodes (context source)
  (if (listp source)
      (dolist (node source)
        (render-node context node))
      (render-node context source)))

(defun render-skipped (context node)
  (declare (ignore context node))
  nil)

(defun heading-level (node)
  (let ((tag (node-tag node)))
    (and (= (length tag) 2)
         (char= (char tag 0) #\h)
         (digit-char-p (char tag 1)))))

(defun block-content-p (node)
  (and (element-p node)
       (member (node-tag node) *block-elements* :test #'string=)))

(defun first-content-node (node)
  (find-if (lambda (child)
             (or (element-p child)
                 (and (text-node-p child)
                      (notevery (lambda (character)
                                  (find character '(#\Space #\Tab #\Newline #\Return)))
                                (text child)))))
           (node-children node)))

(defun begin-blockquote-line (context)
  (if (markdown-context-blockquote-started context)
      (ensure-newlines context 2)
      (setf (markdown-context-blockquote-started context) t))
  (emit-string context "> "))

(defun begin-prose-block (context)
  (if (plusp (markdown-context-blockquote-depth context))
      (begin-blockquote-line context)
      (begin-block context)))

(defun end-prose-block (context)
  (unless (plusp (markdown-context-blockquote-depth context))
    (end-block context)))

(defun render-heading (context node)
  (begin-prose-block context)
  (emit-string context (make-string (heading-level node)
                                    :initial-element #\#))
  (emit-string context " ")
  (render-children context node)
  (end-prose-block context))

(defun render-block (context node)
  (begin-prose-block context)
  (render-children context node)
  (end-prose-block context))

(defun render-blockquote (context node)
  (begin-block context)
  (let ((started (markdown-context-blockquote-started context))
        (content (first-content-node node)))
    (setf (markdown-context-blockquote-depth context)
          (1+ (markdown-context-blockquote-depth context))
          (markdown-context-blockquote-started context) nil)
    (unless (block-content-p content)
      (emit-string context "> ")
      (setf (markdown-context-blockquote-started context) t))
    (render-children context node)
    (setf (markdown-context-blockquote-depth context)
          (1- (markdown-context-blockquote-depth context))
          (markdown-context-blockquote-started context) started)
    (end-block context)))

(defun render-list (context node)
  (if (or (markdown-context-list-stack context)
          (plusp (markdown-context-blockquote-depth context)))
      (progn
        (setf (markdown-context-pending-space context) nil)
        (ensure-newlines context 1))
      (begin-block context))
  (push (cons (node-tag node) 0) (markdown-context-list-stack context))
  (render-children context node)
  (pop (markdown-context-list-stack context))
  (if (or (markdown-context-list-stack context)
          (plusp (markdown-context-blockquote-depth context)))
      (progn
        (setf (markdown-context-pending-space context) nil)
        (ensure-newlines context 1))
      (end-block context)))

(defun render-list-item (context node)
  (setf (markdown-context-pending-space context) nil)
  (ensure-newlines context 1)
  (when (plusp (markdown-context-blockquote-depth context))
    (emit-string context "> "))
  (let ((entry (first (markdown-context-list-stack context))))
    (emit-string context (indent-string context))
    (cond
      ((and entry (string= (car entry) "ol"))
       (incf (cdr entry))
       (emit-string context (format nil "~D. " (cdr entry))))
      (t
       (emit-string context "- "))))
  (render-children context node)
  (setf (markdown-context-pending-space context) nil)
  (ensure-newlines context 1))

(defun render-pre (context node)
  (begin-block context)
  (emit-string context "```")
  (emit-char context #\Newline)
  (incf (markdown-context-pre-depth context))
  (render-children context node)
  (decf (markdown-context-pre-depth context))
  (emit-char context #\Newline)
  (emit-string context "```")
  (end-block context))

(defun render-inline (context node marker)
  (flush-inline-space context)
  (emit-string context marker)
  (render-children context node)
  (emit-string context marker))

(defun render-code (context node)
  (if (plusp (markdown-context-pre-depth context))
      (render-children context node)
      (render-inline context node "`")))

(defun render-link (context node)
  (let ((href (node-attr node "href")))
    (if href
        (progn
          (flush-inline-space context)
          (emit-string context "[")
          (render-children context node)
          (emit-string context (format nil "](~A)" href)))
        (render-children context node))))

(defun render-image (context node)
  (emit-string context
               (format nil "![~A](~A)"
                       (or (node-attr node "alt") "")
                       (or (node-attr node "src") ""))))

(defun render-break (context node)
  (declare (ignore node))
  (emit-string context "  ")
  (emit-char context #\Newline)
  (emit-string context (continuation-indent context)))

(defun render-rule (context node)
  (declare (ignore node))
  (begin-block context)
  (emit-string context "---")
  (end-block context))

(defstruct markdown-table
  (header nil :type list)
  (rows   nil :type list))

(defstruct markdown-table-state
  (table-stack nil :type list)
  (table-row   nil :type list))

(defun table-cell-text (string)
  (with-output-to-string (out)
    (let ((pending nil))
      (loop for character across string do
        (cond
          ((find character '(#\Space #\Tab #\Newline #\Return))
           (setf pending t))
          (t
           (when pending
             (write-char #\Space out))
           (setf pending nil)
           (write-char character out)))))))

(defun escape-table-cell (string)
  (with-output-to-string (out)
    (loop for character across string do
      (when (char= character #\|)
        (write-char #\\ out))
      (write-char character out))))

(defun table-column-widths (rows)
  (let ((columns (loop for row in rows maximize (length row))))
    (loop for index from 0 below columns
          collect (max 3 (loop for row in rows
                               maximize (length (or (nth index row) "")))))))

(defun format-table-row (out cells widths)
  (write-char #\| out)
  (loop for value in cells
        for width in widths
        do (format out " ~VA |" width value))
  (terpri out))

(defun markdown-table-format (table)
  (let* ((header (mapcar #'escape-table-cell
                         (reverse (markdown-table-header table))))
         (rows   (mapcar (lambda (row) (mapcar #'escape-table-cell row))
                         (reverse (markdown-table-rows table))))
         (widths (table-column-widths (cons header rows))))
    (if (null widths)
        ""
        (with-output-to-string (out)
          (format-table-row out
                            (or header
                                (make-list (length widths) :initial-element ""))
                            widths)
          (format-table-row out
                            (mapcar (lambda (width)
                                      (make-string width :initial-element #\-))
                                    widths)
                            widths)
          (dolist (row rows)
            (format-table-row out row widths))))))

(defun render-table (context node)
  (begin-block context)
  (let* ((table (make-markdown-table))
         (state (make-markdown-table-state :table-stack (list table))))
    (collect-table-rows context (node-children node) state)
    (emit-string context (markdown-table-format table)))
  (end-block context))

(defun collect-table-rows (context children state)
  (dolist (child children)
    (when (element-p child)
      (let ((tag (node-tag child)))
        (cond
          ((string= tag "tr")
           (collect-table-row context child state))
          ((member tag '("thead" "tbody" "tfoot") :test #'string=)
           (collect-table-rows context (node-children child) state)))))))

(defun collect-table-row (context node state)
  (collect-table-cells context (node-children node) state)
  (let ((row   (nreverse (markdown-table-state-table-row state)))
        (table (first (markdown-table-state-table-stack state))))
    (setf (markdown-table-state-table-row state) nil)
    (when (and table row)
      (push row (markdown-table-rows table)))))

(defun collect-table-cells (context children state)
  (dolist (child children)
    (when (element-p child)
      (let ((tag (node-tag child)))
        (cond
          ((string= tag "th")
           (collect-table-cell context child state t))
          ((string= tag "td")
           (collect-table-cell context child state nil)))))))

(defun collect-table-cell (context node state headerp)
  (let ((value (table-cell-text
                (with-captured-output (context)
                  (render-children context node))))
        (table (and state (first (markdown-table-state-table-stack state)))))
    (cond
      ((null table)
       (emit-string context value))
      (headerp
       (push value (markdown-table-header table)))
      (t
       (push value (markdown-table-state-table-row state))))))

(dolist (tag *skipped-elements*)
  (setf (gethash tag *element-handlers*) #'render-skipped))

(define-element "p" (context node) (render-block context node))

(define-element "div" (context node) (render-block context node))

(dolist (level '(1 2 3 4 5 6))
  (setf (gethash (format nil "h~D" level) *element-handlers*) #'render-heading))

(define-element "ul" (context node) (render-list context node))

(define-element "ol" (context node) (render-list context node))

(define-element "li" (context node) (render-list-item context node))

(define-element "blockquote" (context node) (render-blockquote context node))

(define-element "pre" (context node) (render-pre context node))

(define-element "strong" (context node) (render-inline context node "**"))

(define-element "b" (context node) (render-inline context node "**"))

(define-element "em" (context node) (render-inline context node "*"))

(define-element "i" (context node) (render-inline context node "*"))

(define-element "del" (context node) (render-inline context node "~~"))

(define-element "s" (context node) (render-inline context node "~~"))

(define-element "code" (context node) (render-code context node))

(define-element "kbd" (context node) (render-code context node))

(define-element "a" (context node) (render-link context node))

(define-element "img" (context node) (render-image context node))

(define-element "br" (context node) (render-break context node))

(define-element "hr" (context node) (render-rule context node))

(define-element "table" (context node) (render-table context node))

(define-element "tr" (context node) (collect-table-row context node nil))

(define-element "th" (context node) (collect-table-cell context node nil t))

(define-element "td" (context node) (collect-table-cell context node nil nil))

(defun markdown-source-nodes (source)
  (cond
    ((null source)
     nil)
    ((listp source)
     source)
    ((node-p source)
     source)
    ((or (stringp source) (pathnamep source) (streamp source))
     (parse source))
    (t
     (error "Cannot convert ~S to Markdown." source))))

(defun html->markdown (source)
  (let ((result
          (with-output-to-string (stream)
            (let ((context (make-markdown-context :stream           stream
                                                  :newline-count    0
                                                  :pre-depth        0
                                                  :blockquote-depth 0)))
              (render-nodes context (markdown-source-nodes source))
              (when (and (markdown-context-last-character context)
                         (not (char= (markdown-context-last-character context)
                                     #\Newline)))
                (emit-char context #\Newline))))))
    (concatenate 'string
                 (string-right-trim '(#\Newline) result)
                 (string #\Newline))))
