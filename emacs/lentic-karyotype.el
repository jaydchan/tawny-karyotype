(require 'f)
(require 'lentic-latex-code)

(defun lentic-karyotype-tex-for-clj (file)
  (concat
   "../../../latex/"
   (file-name-sans-extension
    (file-name-nondirectory
     file)) ".tex"))

(defun lentic-karyotype-init ()
  (lentic-m-oset
   (lentic-clojure-to-latex-new)
   :lentic-file
   (lentic-karyotype-tex-for-clj (buffer-file-name))))

(add-to-list 'lentic-init-functions
             'lentic-karyotype-init)

(provide 'lentic-karyotype)
