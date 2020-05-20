(require 'dumb-jump-grep)

(defgroup dumb-jump-gnu-grep nil
  "GNU Grep subsystem for dumb-jump."
  :group 'dumb-jump)

(defcustom dumb-jump-gnu-grep-args dumb-jump-grep-args
  "Grep command args."
  :type '(repeat string))

(defclass dumb-jump-gnu-grep (dumb-jump-grep)
  ((version :initform 'gnu)
   (symbol :initform 'gnu-grep)))

(cl-defmethod dumb-jump-check-usable :after ((searcher dumb-jump-gnu-grep))
  "Check if GNU grep is installed"
  (eq (oref searcher version) 'gnu))

(cl-defmethod dumb-jump-populate-regexp ((searcher dumb-jump-gnu-grep) regex)
  (mapcar (lambda (part)
            (if (stringp part)
                (replace-regexp-in-string "\\\\s" "[[:space:]]" part)
              part))
          (cl-call-next-method searcher regex)))

(provide 'dumb-jump-gnu-grep)

;; dumb-jump-gnu-grep.el ends here
