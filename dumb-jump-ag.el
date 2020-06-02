(require 'dumb-jump)

(defgroup dumb-jump-ag nil
  "Ag subsystem for dumb-jump."
  :group 'dumb-jump)

(defcustom dumb-jump-ag-cmd "ag"
  "The the path to the silver searcher."
  :type 'string)

(defcustom dumb-jump-ag-search-args nil
  "Appends the passed arguments to the ag search function."
  :type '(repeat string))

(defclass dumb-jump-ag (dumb-jump-searcher)
  ((found-ag-p :type boolean :allocation :class)
   (ag-query :initform nil :type (or null string))
   (symbol :initform 'ag)))

(cl-defmethod dumb-jump-check-usable :before ((searcher dumb-jump-ag))
  "Check if ag is installed."
  (with-temp-buffer
    (shell-command (concat dumb-jump-ag-cmd " --version") t)
    (setf (oref searcher found-ag-p)
          (and (search-forward "ag version" nil t) t))))

(cl-defmethod dumb-jump-check-usable ((searcher dumb-jump-ag))
  "Check if ag is installed."
  (oref searcher found-ag-p))

(cl-defmethod dumb-jump-generate-command ((searcher dumb-jump-ag))
  "Generate command for ag."
  (let ((args dumb-jump-ag-search-args)
        (query (dumb-jump-table-query
                dumb-jump-language-file-exts
                `((:language ,(oref searcher language)))
                :agtype :ext)))
    (push (mapconcat (lambda (regexp)
                       (dumb-jump-populate-regexp
                        searcher regexp))
                     (oref searcher regexps) "|")
          args)
    (dolist (exclude (oref searcher excludes))
      (push (replace-regexp-in-string
             (regexp-quote (oref searcher root))
             "" exclude)
            args)
      (push "--ignore" args))
    (if (and (not (oref searcher ag-query))
             (assq :agtype query))
        (dolist (type (delete-dups (alist-get :agtype query)))
          (push (concat "--" type) args))
      (push (or (oref searcher ag-query)
                (mapconcat (apply-partially #'concat "\\.")
                           `("(" ,@(alist-get :ext query) ")$")
                           "|"))
            args)
      (push "-G" args))
    (when (string= (file-name-extension (oref searcher current-file)) "gz")
      (push "--search-zip" args))
    (append (list dumb-jump-ag-cmd "--nocolor" "--nogroup")
            args (list (oref searcher root)))))

(cl-defmethod dumb-jump-populate-regexp ((searcher dumb-jump-ag) regex)
  (cl-call-next-method
   searcher
   (cl-substitute "(?![a-zA-Z0-9\\?\\*-])" 'word-boundary regex)))

(provide 'dumb-jump-ag)

;; dumb-jump-ag.el ends here
