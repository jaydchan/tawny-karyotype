(require 'load-relative)
(require 'commander)
(load-relative "../emacs/lentic-karyotype")

(setq debug-on-error t)

(defun gensource-and-report (file init)
  (message "Cloning %s..."
           file)
  (let ((config
         (lentic-batch-clone-and-save-with-config
          file init)))
    (message "Cloning %s...done" file)
    (message "Returned config %s" config)
    ;;(message "For %s generated %s."
    ;;file
    ;;         (oref config :lentic-file)))
    ))

(defun gensource-gen-if-necessary (file)
  (let* ((target
          (lentic-karyotype-tex-for-clj
           file))
         (locked
          (or (file-locked-p file)
              (file-locked-p target))))
    (if locked
        (message "Skiping %s due to lock %s" file locked)
      (when (file-newer-than-file-p file target)
        (gensource-and-report file 'lentic-karyotype-init)))))

(defun build/gen-src ()
  (mapc 'gensource-gen-if-necessary
        (directory-files "./src/ncl/karyotype/" t ".*clj")))

(commander
 (command "gen-src" "Generate Latex From Clj" build/gen-src))
