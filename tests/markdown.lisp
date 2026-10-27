(in-package #:fetch-gist.tests)

(deftest markdown/documentation-page-converts-to-markdown
  (ok (string= (html->markdown
                (read-fixture "pages/example-documentation.html"))
               (read-fixture "pages/example-documentation.md"))))

(deftest markdown-from-string/converts-html
  (ok (string= (markdown-from-string "<p>Hello <strong>world</strong>.</p>")
               (concatenate 'string "Hello **world**." (string #\Newline)))))

(deftest markdown-from-file/converts-html
  (ok (string= (markdown-from-file
                (merge-pathnames "pages/example-documentation.html"
                                 *fixture-directory*))
               (read-fixture "pages/example-documentation.md"))))

(deftest markdown/accepts-strings-streams-pathnames-and-nodes
  (let* ((html     (read-fixture "pages/example-documentation.html"))
         (expected (read-fixture "pages/example-documentation.md"))
         (pathname (merge-pathnames "pages/example-documentation.html"
                                    *fixture-directory*))
         (root     (plump:parse html)))
    (ok (string= (html->markdown html) expected))
    (ok (string= (with-input-from-string (stream html)
                   (html->markdown stream))
                 expected))
    (ok (string= (html->markdown pathname) expected))
    (ok (string= (html->markdown root) expected))
    (ok (string= (html->markdown (coerce (plump:children root) 'list))
                 expected))))

(deftest markdown/rejects-an-unconvertible-source
  (ok (signals (html->markdown 42) 'markdown-source-error)))

(deftest markdown/real-world-document-converts-to-markdown
  (ok (string= (convert-fixture "11-real-world")
               (lines "# Welcome"
                      ""
                      "This is a [link](https://example.com)."
                      ""
                      "- First item"
                      "- Second item"))))

(deftest markdown/entities-are-decoded
  (ok (string= (convert-fixture "08-entities")
               (lines (format nil "Tom & Jerry <3 ~C A" #\Copyright_Sign)))))

(deftest markdown/plain-text-is-preserved
  (ok (string= (convert-fixture "01-plain-text") (lines "hello world"))))

(deftest markdown/element-content-becomes-a-block
  (ok (string= (convert-fixture "02-single-element") (lines "hello"))))

(deftest markdown/attributes-are-kept-on-links
  (ok (string= (convert-fixture "04-attributes")
               (lines "[link](https://example.com)"))))

(deftest markdown/doctypes-and-instructions-are-dropped
  (ok (string= (convert-fixture "07-special") (string #\Newline))))

(deftest markdown/void-elements-become-hard-breaks-and-images
  (ok (string= (convert-fixture "03-void-elements")
               (lines "  " "![](x.png)"))))

(deftest markdown/uppercase-markup-is-normalized
  (ok (string= (convert-fixture "05-whitespace-case") (lines "hi"))))

(deftest markdown/unclosed-elements-are-recovered
  (ok (string= (convert-fixture "06-nested") (lines "*x*"))))

(deftest markdown/inline-markup-is-preserved
  (ok (string= (convert-fixture "09-mixed") (lines "hi **there**!"))))

(deftest markdown/malformed-input-recovers-as-text
  (ok (string= (convert-fixture "10-malformed")
               (lines "unclosed paragraph 5 < 10 and > 3 \";<script>"))))

(deftest markdown/empty-document-has-no-content
  (ok (string= (convert-fixture "empty") (string #\Newline))))

(deftest markdown/headings-are-converted-by-level
  (ok (string= (markdown-from-string "<h1>a</h1><h2>b</h2><h6>c</h6>")
               (lines "# a" "" "## b" "" "###### c"))))

(deftest markdown/nested-lists-are-indented
  (ok (string= (markdown-from-string
                "<ul><li>one<ul><li>a</li><li>b</li></ul></li><li>two</li></ul>")
               (lines "- one" "  - a" "  - b" "- two")))
  (ok (string= (markdown-from-string
                "<ol><li>one<ol><li>a</li><li>b</li></ol></li><li>two</li></ol>")
               (lines "1. one" "  1. a" "  2. b" "2. two"))))

(deftest markdown/hard-breaks-keep-the-list-indent
  (ok (string= (markdown-from-string "<ul><li>one<br>two</li><li>three</li></ul>")
               (lines "- one  " "  two" "- three"))))

(deftest markdown/rules-become-thematic-breaks
  (ok (string= (markdown-from-string "<p>before</p><hr><p>after</p>")
               (lines "before" "" "---" "" "after"))))

(deftest markdown/blockquote-paragraphs-are-continued
  (ok (string= (markdown-from-string
                "<blockquote><p>First.</p><p>Second.</p></blockquote>")
               (lines "> First." "" "> Second.")))
  (ok (string= (markdown-from-string "<blockquote>quoted</blockquote>")
               (lines "> quoted")))
  (ok (string= (markdown-from-string "<blockquote><ul><li>item</li></ul></blockquote>")
               (lines "> - item"))))

(deftest markdown/links-without-href-keep-their-text
  (ok (string= (markdown-from-string "<p>see <a>here</a> now</p>")
               (lines "see here now"))))

(deftest markdown/skipped-elements-drop-their-content
  (ok (string= (markdown-from-string
                "<p>keep</p><svg><path d=\"M0\"/></svg><script>x</script>")
               (lines "keep"))))

(deftest markdown/code-inside-pre-is-not-rewrapped
  (ok (string= (markdown-from-string "<pre><code>if (a &lt; b) { }</code></pre>")
               (lines "```" "if (a < b) { }" "```"))))

(deftest markdown/table-converts-to-a-pipe-table
  (ok (string= (html->markdown (read-fixture "pages/example-table.html"))
               (read-fixture "pages/example-table.md"))))

(deftest markdown/table-without-a-header-gets-an-empty-one
  (ok (string= (markdown-from-string
                "<table><tr><td>a</td><td>b</td></tr><tr><td>c</td><td>d</td></tr></table>")
               (lines "|     |     |"
                      "| --- | --- |"
                      "| a   | b   |"
                      "| c   | d   |"))))

(deftest markdown/table-escapes-pipes-and-flattens-blocks
  (ok (string= (markdown-from-string
                "<table><tr><th>a | b</th></tr>
                 <tr><td><p>one</p><p>two</p></td></tr></table>")
               (lines "| a \\| b  |"
                      "| ------- |"
                      "| one two |"))))
