(require 'dumb-jump)
(require 'dumb-jump-git-grep)
(require 'dumb-jump-ag)

(defclass dumb-jump-git-grep+ag (dumb-jump-ag dumb-jump-git-grep)
  ;; no symbol extra, because the rules are the same as for ag
  ())

(cl-defmethod dumb-jump-check-usable ((searcher dumb-jump-git-grep+ag))
  "Check if both ag and git grep is installed."
  (and (oref searcher found-git-grep-p)
       (oref searcher found-ag-p)))

(cl-defmethod dumb-jump-generate-command :before ((searcher dumb-jump-git-grep+ag))
  (with-temp-buffer
    (let ((default-directory (oref searcher root)))
      (call-process dumb-jump-git-cmd
                    nil t nil
                    "grep" "--full-name" "-F" "-c"
                    (oref searcher search) (oref searcher root)))
    (goto-char (point-min))
    (let (files)
      (while (not (eobp))
        (when (looking-at (rx bos (group (*? nonl)) ":"))
          ;; FIXME: the files may contain symbols that could be
          ;; confused for parts of a regular expression. this should
          ;; be escaped
          (push (file-relative-name
                 (expand-file-name (match-string 1)
                                   (oref searcher root))
                 (oref searcher root))
                files)))
      (setf (oref searcher ag-query)
            (mapconcat #'identity `("(" ,@files ")") "|")))))

(provide 'dumb-jump-git-grep+ag)

;; dumb-jump-git-grep+ag.el ends here
