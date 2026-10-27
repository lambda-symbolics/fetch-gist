(in-package #:fetch-gist.markdown)

(defparameter *inline-elements*
  '("a" "b" "code" "del" "em" "i" "s" "strong" "kbd")
  "The tags of the inline elements the converter writes with Markdown markup.")

(defparameter *block-elements*
  '("blockquote" "div" "h1" "h2" "h3" "h4" "h5" "h6" "hr"
    "li" "ol" "p" "pre" "table" "ul")
  "The tags of the elements that start their content on a new line.")

(defparameter *skipped-elements*
  '("head" "style" "script" "svg" "colgroup" "col" "caption" "template")
  "The tags of the elements whose content the converter leaves out.")

(defstruct markdown-context
  "The output stream and layout state of one HTML to Markdown conversion."
  stream
  last-character
  (newline-count 0)
  pending-space
  (pre-depth 0)
  (blockquote-depth 0)
  blockquote-started
  list-stack)

(defun emit-char (context character)
  "Write CHARACTER to the output of CONTEXT and track the newlines that end it."
  (write-char character (markdown-context-stream context))
  (setf (markdown-context-last-character context) character
        (markdown-context-newline-count context)
        (if (char= character #\Newline)
            (1+ (markdown-context-newline-count context))
            0)))

(defun emit-string (context string)
  "Write each character of STRING to the output of CONTEXT."
  (loop for character across string do
    (emit-char context character)))

(defun ensure-newlines (context count)
  "Write newlines until the output of CONTEXT ends in COUNT of them.

Write nothing while the output is empty."
  (when (markdown-context-last-character context)
    (loop while (< (markdown-context-newline-count context) count) do
      (emit-char context #\Newline))))

(defun begin-block (context)
  "Start a block element in CONTEXT on a new paragraph."
  (setf (markdown-context-pending-space context) nil)
  (ensure-newlines context 2))

(defun end-block (context)
  "End a block element in CONTEXT with a blank line."
  (setf (markdown-context-pending-space context) nil)
  (ensure-newlines context 2))

(defun emit-text (context text)
  "Write the text of a text node to CONTEXT.

Inside a pre element, write TEXT as it is. Elsewhere, write each run of
whitespace as one space, but none at the start of a line or after a space."
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
  "Write the pending space in CONTEXT, if there is one."
  (when (markdown-context-pending-space context)
    (emit-char context #\Space)
    (setf (markdown-context-pending-space context) nil)))

(defun indent-string (context)
  "Return the spaces that indent a list item marker at the list depth of CONTEXT."
  (make-string (* 2 (max 0 (1- (length (markdown-context-list-stack context)))))
               :initial-element #\Space))

(defun continuation-indent (context)
  "Return the spaces that indent a continued line inside the list items of CONTEXT."
  (make-string (* 2 (length (markdown-context-list-stack context)))
               :initial-element #\Space))

(defmacro with-captured-output ((context) &body body)
  "Run BODY with the output of CONTEXT sent to a fresh string, and return that string.

The expansion evaluates CONTEXT more than once. The stream and line state of
CONTEXT are restored afterward, also when BODY exits nonlocally."
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
  "Return the children of the Plump NODE as a list, or NIL when it cannot have children."
  (when (nesting-node-p node)
    (coerce (children node) 'list)))

(defun node-tag (node)
  "Return the lowercase tag name of the Plump element NODE."
  (string-downcase (tag-name node)))

(defun node-attr (node name)
  "Return the value of the attribute NAME of the Plump element NODE, ignoring case."
  (let ((attributes (attributes node)))
    (loop for key being the hash-keys of attributes
          when (string-equal key name)
            return (gethash key attributes))))

(defvar *element-handlers* (make-hash-table :test #'equal)
  "The handler functions that render elements, keyed by lowercase tag name.")

(defmacro define-element (tag (context node) &body body)
  "Define BODY as the handler for elements with the tag TAG.

BODY runs with CONTEXT and NODE bound to the Markdown context and the element."
  `(setf (gethash ,tag *element-handlers*)
         (lambda (,context ,node)
           (declare (ignorable ,context ,node))
           ,@body)))

(defun render-node (context node)
  "Write the Markdown for the Plump NODE to CONTEXT."
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
  "Write the Markdown for each child of NODE to CONTEXT."
  (dolist (child (node-children node))
    (render-node context child)))

(defun render-element (context node)
  "Write the Markdown for the element NODE with the handler for its tag.

An element without a handler renders as its children."
  (let ((handler (gethash (node-tag node) *element-handlers*)))
    (if handler
        (funcall handler context node)
        (render-children context node))))

(defun render-nodes (context source)
  "Write the Markdown for SOURCE, a Plump node or a list of them, to CONTEXT."
  (if (listp source)
      (dolist (node source)
        (render-node context node))
      (render-node context source)))

(defun render-skipped (context node)
  "Write nothing for an element whose content does not belong in the Markdown."
  (declare (ignore context node))
  nil)

(defun heading-level (node)
  "Return the digit in the heading tag of NODE, such as 2 for h2, or NIL for other tags."
  (let ((tag (node-tag node)))
    (and (= (length tag) 2)
         (char= (char tag 0) #\h)
         (digit-char-p (char tag 1)))))

(defun block-content-p (node)
  "Return true when NODE is a block element."
  (and (element-p node)
       (member (node-tag node) *block-elements* :test #'string=)))

(defun first-content-node (node)
  "Return the first child of NODE that is an element or holds text other than whitespace."
  (find-if (lambda (child)
             (or (element-p child)
                 (and (text-node-p child)
                      (notevery (lambda (character)
                                  (find character '(#\Space #\Tab #\Newline #\Return)))
                                (text child)))))
           (node-children node)))

(defun begin-blockquote-line (context)
  "Start a quoted paragraph in CONTEXT and write its > marker."
  (if (markdown-context-blockquote-started context)
      (ensure-newlines context 2)
      (setf (markdown-context-blockquote-started context) t))
  (emit-string context "> "))

(defun begin-prose-block (context)
  "Start a paragraph or heading in CONTEXT, inside a blockquote when one is open."
  (if (plusp (markdown-context-blockquote-depth context))
      (begin-blockquote-line context)
      (begin-block context)))

(defun end-prose-block (context)
  "End a paragraph or heading in CONTEXT outside a blockquote."
  (unless (plusp (markdown-context-blockquote-depth context))
    (end-block context)))

(defun render-heading (context node)
  "Write the heading element NODE to CONTEXT as an ATX heading."
  (begin-prose-block context)
  (emit-string context (make-string (heading-level node)
                                    :initial-element #\#))
  (emit-string context " ")
  (render-children context node)
  (end-prose-block context))

(defun render-block (context node)
  "Write the block element NODE to CONTEXT as a paragraph."
  (begin-prose-block context)
  (render-children context node)
  (end-prose-block context))

(defun render-blockquote (context node)
  "Write the blockquote element NODE to CONTEXT with each paragraph marked by >."
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
  "Write the list element NODE to CONTEXT, nested inside any open list."
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
  "Write the list item NODE to CONTEXT with a bullet or the next number of its list."
  (setf (markdown-context-pending-space context) nil)
  (ensure-newlines context 1)
  (when (plusp (markdown-context-blockquote-depth context))
    (emit-string context "> "))
  (let ((entry (first (markdown-context-list-stack context))))
    (emit-string context (indent-string context))
    (cond
      ((and entry (string= (first entry) "ol"))
       (incf (rest entry))
       (emit-string context (format nil "~D. " (rest entry))))
      (t
       (emit-string context "- "))))
  (render-children context node)
  (setf (markdown-context-pending-space context) nil)
  (ensure-newlines context 1))

(defun render-pre (context node)
  "Write the pre element NODE to CONTEXT as a fenced code block."
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
  "Write the inline element NODE to CONTEXT between two copies of MARKER."
  (flush-inline-space context)
  (emit-string context marker)
  (render-children context node)
  (emit-string context marker))

(defun render-code (context node)
  "Write the code element NODE to CONTEXT as inline code, or as plain text inside a pre element."
  (if (plusp (markdown-context-pre-depth context))
      (render-children context node)
      (render-inline context node "`")))

(defun render-link (context node)
  "Write the link element NODE to CONTEXT as a Markdown link.

A link without an href attribute renders as its text."
  (let ((href (node-attr node "href")))
    (if href
        (progn
          (flush-inline-space context)
          (emit-string context "[")
          (render-children context node)
          (emit-string context (format nil "](~A)" href)))
        (render-children context node))))

(defun render-image (context node)
  "Write the image element NODE to CONTEXT as a Markdown image."
  (emit-string context
               (format nil "![~A](~A)"
                       (or (node-attr node "alt") "")
                       (or (node-attr node "src") ""))))

(defun render-break (context node)
  "Write a hard line break to CONTEXT, indented to continue the current list item."
  (declare (ignore node))
  (emit-string context "  ")
  (emit-char context #\Newline)
  (emit-string context (continuation-indent context)))

(defun render-rule (context node)
  "Write a thematic break to CONTEXT."
  (declare (ignore node))
  (begin-block context)
  (emit-string context "---")
  (end-block context))

(defstruct markdown-table
  "The header cells and body rows of one table, each in reverse order."
  (header nil :type list)
  (rows   nil :type list))

(defstruct markdown-table-state
  "The open tables, innermost first, and the cells of the row being collected."
  (table-stack nil :type list)
  (table-row   nil :type list))

(defun table-cell-text (string)
  "Return STRING with each run of whitespace collapsed into one space."
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
  "Return STRING with each | escaped for a pipe table cell."
  (with-output-to-string (out)
    (loop for character across string do
      (when (char= character #\|)
        (write-char #\\ out))
      (write-char character out))))

(defun table-column-widths (rows)
  "Return the width of each column of ROWS, at least 3."
  (let ((columns (loop for row in rows maximize (length row))))
    (loop for index from 0 below columns
          collect (max 3 (loop for row in rows
                               maximize (length (or (nth index row) "")))))))

(defun format-table-row (out cells widths)
  "Write one pipe table row of CELLS to OUT, each padded to its width in WIDTHS."
  (write-char #\| out)
  (loop for value in cells
        for width in widths
        do (format out " ~VA |" width value))
  (terpri out))

(defun markdown-table-format (table)
  "Return TABLE as a pipe table string, or an empty string when it has no cells.

A table without a header row gets one with empty cells."
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
  "Write the table element NODE to CONTEXT as a pipe table."
  (begin-block context)
  (let* ((table (make-markdown-table))
         (state (make-markdown-table-state :table-stack (list table))))
    (collect-table-rows context (node-children node) state)
    (emit-string context (markdown-table-format table)))
  (end-block context))

(defun collect-table-rows (context children state)
  "Add the rows among CHILDREN, including those in thead, tbody, and tfoot, to the table in STATE."
  (dolist (child children)
    (when (element-p child)
      (let ((tag (node-tag child)))
        (cond
          ((string= tag "tr")
           (collect-table-row context child state))
          ((member tag '("thead" "tbody" "tfoot") :test #'string=)
           (collect-table-rows context (node-children child) state)))))))

(defun collect-table-row (context node state)
  "Add the row element NODE and its cells to the table in STATE."
  (collect-table-cells context (node-children node) state)
  (let ((row   (nreverse (markdown-table-state-table-row state)))
        (table (first (markdown-table-state-table-stack state))))
    (setf (markdown-table-state-table-row state) nil)
    (when (and table row)
      (push row (markdown-table-rows table)))))

(defun collect-table-cells (context children state)
  "Add the th and td elements among CHILDREN to the current row in STATE."
  (dolist (child children)
    (when (element-p child)
      (let ((tag (node-tag child)))
        (cond
          ((string= tag "th")
           (collect-table-cell context child :state state :header-p t))
          ((string= tag "td")
           (collect-table-cell context child :state state)))))))

(defun collect-table-cell (context node &key state header-p)
  "Add the text of the cell element NODE to the table in STATE.

When HEADER-P is true, add it to the header row. Without a table in STATE,
write the text to CONTEXT."
  (let ((value (table-cell-text
                (with-captured-output (context)
                  (render-children context node))))
        (table (and state (first (markdown-table-state-table-stack state)))))
    (cond
      ((null table)
       (emit-string context value))
      (header-p
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

(define-element "th" (context node) (collect-table-cell context node :header-p t))

(define-element "td" (context node) (collect-table-cell context node))

(defun markdown-source-nodes (source)
  "Return the Plump nodes for SOURCE, parsing it when it is a string, pathname, or stream.

Signal MARKDOWN-SOURCE-ERROR for any other kind of object."
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
     (error 'markdown-source-error :source source))))

(defun html->markdown (source)
  "Convert SOURCE to Markdown and return it as a string ending in one newline.

SOURCE is HTML text, a pathname or stream to read it from, a Plump node, or
a list of Plump nodes."
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

(define-condition markdown-source-error (error)
  ((source
    :initarg       :source
    :reader        markdown-source-error-source
    :documentation "The object HTML->MARKDOWN was given."))
  (:report (lambda (condition stream)
             (format stream "Cannot convert ~S to Markdown."
                     (markdown-source-error-source condition))))
  (:documentation "Signalled when HTML->MARKDOWN gets a source other than HTML text, a pathname, a stream, or Plump nodes."))
