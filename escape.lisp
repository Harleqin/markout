(in-package #:markout)

(defun escape (string)
  "Returns a string derived from STRING with escaping for HTML.  Always copies."
  ;; A bit more than the original string length is a good approximation.
  (let ((s (make-array (ceiling (* 10/9 (length string)))
                       :element-type 'character
                       :fill-pointer 0
                       :adjustable t)))
    (with-output-to-string (out s)
      (loop :for old-pos := 0 :then (1+ pos)
            :for pos := (position-if #'char-needs-escape-p string :start old-pos)
            :do (write-sequence string out :start old-pos :end pos)
            :while pos
            :do (let* ((char (char string pos))
                       (replacement (cdr (assoc char '((#\& . "&amp;")
                                                       (#\< . "&lt;")
                                                       (#\> . "&gt;")
                                                       (#\" . "&quot;"))))))
                  (if replacement
                      (write-sequence replacement out)
                      (princ-all out "&#" (char-code char) #\;)))))
    s))

(defun char-needs-escape-p (char)
  (or (> (char-code char) 127)
      (find char "&<>\"" :test #'char=)))

(defmacro princ-all (stream &rest forms)
  `(progn ,@(loop :for form :in forms
                  :collect `(princ ,form ,stream))))
