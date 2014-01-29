(TeX-add-style-hook "tkz-graph"
  (function
   (lambda ()
     (TeX-add-symbols
     '("Vertex" ["Options"] TeX-arg-tkz-graph-vertex)
     ))))

(defun TeX-arg-tkz-graph-vertex (optional &optional prompt)
  "Prompt for label of a vertex."
  (let ((lab (read-input "Label: ")))
    (insert "{" lab "}")
    (indent-according-to-mode)))
