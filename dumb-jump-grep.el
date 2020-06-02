(defgroup dumb-jump-grep nil
  "Grep subsystem for dumb-jump."
  :group 'dumb-jump)

(defcustom dumb-jump-grep-cmd grep-program
  "The path to grep."
  :type 'string)

(defcustom dumb-jump-zgrep-cmd "zgrep"
  "The path to grep to use for gzipped files."
  :type 'string)

(defcustom dumb-jump-grep-prefix "LANG=C"
  "Prefix to grep command."
  :type 'string)

(defcustom dumb-jump-grep-args
  '("-R"                                ; Recursive
    "-E"                                ; Extended
    "-n"                                ; show line numbers
    )
  "Grep command args."
  :type '(repeat string))

(defclass dumb-jump-grep (dumb-jump-searcher)
  ((version :initform nil :allocation :class)
   (symbol :initform 'grep)))

(cl-defmethod dumb-jump-check-usable ((searcher dumb-jump-grep))
  "Check if grep is installed, and determine it's variant."
  (with-temp-buffer
    (shell-command (concat dumb-jump-grep-cmd " --version") t)
    (cond ((search-forward "GNU grep" nil t)
           (setf (oref searcher version) 'gnu))
          ((search-forward-regexp "[0-9]+\\.[0-9]+" nil t)
           (setf (oref searcher version) 'bsd))
          (t nil))))

(cl-defmethod dumb-jump-parse-response ((searcher dumb-jump-grep))
  "Parse response into `dumb-jump-target' structs.
Warnings produced by grep are ignored."
  (cl-loop initially (goto-char (point-min))
     ;; FIXME: fails is locale is not english
     ;; FIXME: fails if "No such file or" is part of the line
     until (eobp)
     unless (search-forward-regexp "^grep:\\|No such file or" (line-end-position) t)
     when (dumb-jump-parse-response-line searcher)
     collect it
     do (forward-line)))

(cl-defmethod dumb-jump-generate-command ((searcher dumb-jump-grep))
  (let ((args)
        (query (dumb-jump-table-query-1 dumb-jump-language-file-exts
                                        `((:language ,(oref searcher language)))
                                        :ext)))
    (dolist (regexp (oref searcher regexps))
      (push (dumb-jump-populate-regexp searcher regexp)
            args)
      (push "-e" args))
    (dolist (exclude (oref searcher excludes))
      (push (format "--exclude-dir=%s" exclude) args))
    (dolist (ext query)
      (push (format "--include=*.%s" ext) args))
    (nconc (list (if (string= (file-name-extension (oref searcher current-file)) "gz")
                     dumb-jump-zgrep-cmd dumb-jump-grep-cmd))
           dumb-jump-grep-args args (list (oref searcher root)))))

(provide 'dumb-jump-grep)

;; dumb-jump-grep.el ends here
