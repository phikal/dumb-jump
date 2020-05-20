(defgroup dumb-jump-git-grep nil
  "git-grep subsystem for dumb-jump."
  :group 'dumb-jump)

(defcustom dumb-jump-git-cmd "git"
  "The the path to git grep."
  :type 'string)

(defcustom dumb-jump-git-grep-search-untracked t
  "If non-nil Dumb Jump will also search untracked files when using searcher git-grep."
  :type 'boolean)

(defcustom dumb-jump-git-grep-search-args nil
  "Appends the passed arguments to the git-grep search function."
  :type '(repeat string))

(defclass dumb-jump-git-grep (dumb-jump-searcher)
  ((found-git-grep-p :type boolean :allocation :class)
   (symbol :initform 'git-grep)))

(cl-defmethod dumb-jump-check-usable :before ((searcher dumb-jump-git-grep))
  "Check if git grep is installed."
  (with-temp-buffer
    (shell-command (concat dumb-jump-git-cmd " grep") t)
    (setf (oref searcher found-git-grep-p)
          (and (search-forward "fatal: no pattern given" nil t) t))))

(cl-defmethod dumb-jump-check-usable ((searcher dumb-jump-git-grep))
  "Check if git grep is installed."
  (oref searcher found-git-grep-p))

(cl-defmethod dumb-jump-generate-command ((searcher dumb-jump-git-grep))
  (let ((args dumb-jump-git-grep-search-args)
        (exts (dumb-jump-table-query-1
               dumb-jump-language-file-exts
               `((:language ,(oref searcher language)))
               :ext)))
    (dolist (exclude (oref searcher excludes))
      (push (concat ":(exclude)" exclude)
            args))
    (dolist (ext exts)
      (push (format "%s*.%s" (oref searcher root) ext)
            args))
    (push "--" args)
    (dolist (regexp (oref searcher regexps))
      (push (dumb-jump-populate-regexp searcher regexp)
            args)
      (push "-e" args))
    (when dumb-jump-git-grep-search-untracked
      (push "--untracked" args))
    (nconc (list dumb-jump-git-cmd "--no-pager" "grep"
                 "--full-name" "--color=never" "-nEI")
           args)))

(provide 'dumb-jump-git-grep)

;; dumb-jump-git-grep.el ends here
