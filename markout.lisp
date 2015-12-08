(in-package #:markout)

(defun markout (tree stream)
  "Outputs the TREE to STREAM.  TREE is supposed to be a cons tree of keywords
and strings.  When a list starts with a keyword, it is treated as a tagname.
Any further keyword/value pairs in that list are treated as attributes for the
tag.  After that, only strings and sublists are allowed.  Strings are output as
they are.  Sublists are treated recursively.  Lists that start with a string are
flattened into the surrounding level.  This includes the toplevel, so that you
can represent an HTML page as a two-element list of the doctype declaration and
the html tree.

  Example:

  \(\"<!DOCTYPE html>\"
   \(:html \(:head \(:title \"Hello\"\)\)
          \(:body \(:h1 \"Hello!\"\)
                 \"This is text.\"
                 \"More text.\"
                 \(\"Also possible like this.\"
                  \(:a :href \"http://example.org\"
                      :class \"example-link\"
                      \"A link\"\)\)\)\)\)"
  (ctypecase tree
    (null (values))
    (string (princ tree stream)
            (values))
    (cons (markout-node tree stream))))

(defun markout-node (tree stream)
  (let ((head (first tree)))
    (ctypecase head
      (null (values))
      (string (markout-list tree stream))
      (keyword (let ((tag (string-downcase (symbol-name head))))
                 (princ #\< stream)
                 (princ tag stream)
                 (markout-attributes (rest tree) stream)
                 (princ "</" stream)
                 (princ tag stream)
                 (princ #\> stream)
                 (values))))))

(defun markout-attributes (tree stream)
  (loop :for list := tree :then (cddr list)
        :for (maybe-key maybe-val . tail) := list
        :while (and (keywordp maybe-key)
                    (stringp maybe-val))
        :do (princ #\space stream)
            (princ (string-downcase (symbol-name maybe-key)) stream)
            (princ #\= stream)
            (prin1 maybe-val stream)
        :finally (princ #\> stream)
                 (markout-list list stream)))

(defun markout-list (tree stream)
  (dolist (e tree)
    (markout e stream)))
