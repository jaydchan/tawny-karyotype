(require 'lentic-latex-code)

(defun lentic-karyotype-init ()
  (lentic-m-oset
   (lentic-clojure-to-latex-new)
   :lentic-file
   (concat
    "../../../latex/"
    (file-name-sans-extension
     (file-name-nondirectory
      (buffer-file-name))) ".tex")))

(add-to-list 'lentic-init-functions
             'lentic-karyotype-init)

(provide 'lentic-karyotype)
