(require 'dumb-jump)

(defgroup dumb-jump-rg nil
  "Ripgrep subsystem for dumb-jump."
  :group 'dumb-jump)

(defcustom dumb-jump-rg-cmd "rg"
  "The the path to ripgrep.  By default assumes it is in path.  If not found fallbacks to grep."
  :type 'string)

(defcustom dumb-jump-rg-search-args nil
  "Appends the passed arguments to the rg search function."
  :type '(repeat string))

(defclass dumb-jump-rg (dumb-jump-searcher)
  ((symbol :initform 'rg)))

(cl-defmethod dumb-jump-check-usable ((_searcher dumb-jump-rg))
  "Check if rg is installed."
  (let (ok1 ok2)
    (with-temp-buffer                   ;ensure minimal version
      (shell-command (concat dumb-jump-rg-cmd " --version") t)
      (save-match-data
        (when (search-forward-regexp "ripgrep \\([0-9]+\\.[0-9]+\\).*" nil t)
          (setq ok1 (version<= "0.10" (match-string 1))))))
    (with-temp-buffer                   ;ensure PCRE2
      (shell-command (concat dumb-jump-rg-cmd " --pcre2 -q .") t)
      (save-match-data
        (setq ok2 (not (looking-at-p "PCRE2 is not available in this build")))))
    (and ok1 ok2)))

(cl-defmethod dumb-jump-generate-command ((searcher dumb-jump-rg))
  (let ((args dumb-jump-rg-search-args)
        (query (dumb-jump-table-query-1
                dumb-jump-language-file-exts
                `((:language ,(oref searcher language)))
                :rgtype)))
    (push (mapconcat (lambda (regexp)
                       (dumb-jump-populate-regexp searcher regexp))
                     (oref searcher regexps) "|")
          args)
    (dolist (exclude (oref searcher excludes))
      (push (concat "!" (replace-regexp-in-string
                         (regexp-quote (oref searcher root))
                         "" exclude))
            args)
      (push "-g" args))
    (dolist (type query)
      (push type args)
      (push "--type" args))
    (nconc (list dumb-jump-rg-cmd
                 "--color" "never" "--no-heading"
                 "--line-number" "--pcre2" "-U")
           args (list (oref searcher root)))))

(cl-defmethod dumb-jump-populate-regexp ((searcher dumb-jump-rg) regex)
  (cl-call-next-method
   searcher
   (cons (replace-regexp-in-string "\\`-" "[-]" (car regex))
                             (cdr regex))))

(provide 'dumb-jump-rg)

;; dumb-jump-rg.el ends here
