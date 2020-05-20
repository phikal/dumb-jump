;;; dumb-jump.el --- Jump to definition for 40+ languages without configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2019 jack angers
;; Author: jack angers and contributors
;; Url: https://github.com/jacktasia/dumb-jump
;; Version: 0.5.3
;; Package-Requires: ((emacs "25.1"))
;; Keywords: programming

;; Dumb Jump is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Dumb Jump is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Dumb Jump.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; Dumb Jump is an Emacs "jump to definition" package with support for 40+
;; programming languages that favors "just working" over speed or accuracy. This
;; means minimal -- and ideally zero -- configuration with absolutely no stored
;; indexes (TAGS) or persistent background processes. Dumb Jump performs best
;; with The Silver Searcher (ag) or ripgrep (rg) installed.

;;; Code:

(require 'ring)
(require 'etags)
(require 'cl-lib)
(require 'eieio)
(require 'tramp)
(require 'xref)

(require 'dumb-jump-db)

(defgroup dumb-jump nil
  "Easily jump to project function and variable definitions"
  :group 'tools
  :group 'convenience)


;;; User Options

(defcustom dumb-jump-prefer-searcher nil
  "The preferred searcher to use 'ag, 'rg, 'git-grep, 'gnu-grep,or 'grep.
If nil then the most optimal searcher will be chosen at runtime."
  :type '(choice (const :tag "Best Available" nil)
          (const :tag "ag" dumb-jump-ag)
          (const :tag "rg" dumb-jump-rg)
          (const :tag "grep" dumb-jump-gnu-grep)
          (const :tag "git grep" dumb-jump-git-grep)
          (const :tag "git grep + ag" dumb-jump-git-grep+ag)))

(defcustom dumb-jump-force-searcher nil
  "Forcibly use searcher: 'ag, 'rg, 'git-grep, 'gnu-grep, or 'grep.
Set to nil to not force anything and use `dumb-jump-prefer-searcher'
or most optimal searcher."
  :type '(choice (const :tag "Best Available" nil)
          (const :tag "ag" dumb-jump-ag)
          (const :tag "rg" dumb-jump-rg)
          (const :tag "grep" dumb-jump-gnu-grep)
          (const :tag "git grep" dumb-jump-git-grep)
          (const :tag "git grep + ag" dumb-jump-git-grep+ag)))



(defcustom dumb-jump-fallback-regex '("\\b" term word-boundary)
  "Regular expression to use when more complex ones fail."
  :type 'string)

(defcustom dumb-jump-fallback-search t
  "If nothing is found with normal search fallback to searching the fallback regex."
  :type 'boolean)

(defcustom dumb-jump-functions-only nil
  "Should we only jump to functions?"
  :type 'boolean)
(defcustom dumb-jump-ignore-context nil
  "If non-nil Dumb Jump will ignore the context of point when jumping."
  :type 'boolean)

(defcustom dumb-jump-project-denoters
  '(".dumbjump" ".projectile" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".svn" "Makefile" "PkgInfo" "-pkg.el")
  "Files and directories that signify a directory is a project root."
  :type '(repeat (string :tag "Name")))

(defcustom dumb-jump-default-project "~"
  "The default project to search within if a project root is not found."
  :type 'string)

(defcustom dumb-jump-project nil
  "The project to search within if normal denoters will not work.
This should only be needed in the rarest of cases."
  :type 'string)

(defcustom dumb-jump-aggressive nil
  "If non-nil, jump aggressively with the possibility of a false positive.
If `nil` always show list of more than 1 match."
  :type 'boolean)

(defcustom dumb-jump-debug nil
  "If nil-nil, will print helpful debug information."
  :type 'boolean)


;;; Searcher Baseclass and Methods

(defclass dumb-jump-searcher ()
  ((search :initarg :search :type string)
   (root :initarg :root :type string)
   (current-line :initform (line-number-at-pos)
                 :type integer)
   (current-file :initform
                 (if (buffer-file-name)
                     (expand-file-name (buffer-file-name))
                   (buffer-file-name))
                 :type string)
   (right-context :initform nil :type (or null string))
   (left-context :initform nil :type (or null string))
   (comment-syntax :type string)
   (marker :initform (point-marker))
   (language :type symbol)
   (excludes :type (list-of string))
   (includes :type (list-of string))
   (results :type (list-of dumb-jump-target))
   (regexps :type list)
   (symbol :type symbol :allocation :class)
   (options :initform nil :type (list-of symbol)))
  "Abstract base-class for all searchers."
  :abstract t)

(cl-defgeneric dumb-jump-check-usable (searcher)
  "Check if searcher is installed and usable.")

(cl-defgeneric dumb-jump-parse-response (searcher)
  "Parse results of query.
The result must be a list of `dumb-jump-target' objects."
  (cl-loop initially (goto-char (point-min))
     until (eobp)
     when (dumb-jump-parse-response-line searcher)
     collect it
     do (forward-line)))

(cl-defgeneric dumb-jump-generate-command (searcher)
  "Generate command to be executed.
The command must be a list of arguments, starting with the
binary name.")

(cl-defmethod dumb-jump-generate-command :before ((searcher dumb-jump-searcher))
  (with-slots (language symbol (right right-context) (left left-context))
      searcher
    (unless (slot-boundp searcher 'regexps)
      (let ((query `((:language ,language)
                     (:supports ,symbol memq))))
        ;; check if query should be limited by a type of rule (function,
        ;; variable, class, ...).
        (cond (dumb-jump-functions-only
               (push '(:type function) query))
              ((not dumb-jump-ignore-context)
               (let ((res (dumb-jump-table-query-1
                           dumb-jump-language-contexts
                           `((:language ,language)
                             (:right ,right string-match-p)
                             (:left ,left string-match-p))
                           :type))
                     type)
                 ;; only set a type, if the query is unambiguous. this is
                 ;; tested by checking if removing all instances of the first
                 ;; element of the list, results in a empty list.
                 (when (and (not (delq (setq type (car res)) res))
                            type)
                   (push (list :type type) query)))))
        (setf (oref searcher regexps)
              (or (dumb-jump-table-query-1 dumb-jump-find-rules
                                           query :regex)
                  (and dumb-jump-fallback-search
                       (list dumb-jump-fallback-regex))))))))

(cl-defgeneric dumb-jump-populate-regexp (searcher regexp)
  "Generate a regular expression for SEARCHER from REGEXP.
REGEXP is a list of strings and symbols that will be processed
and then concatenated into a single string."
  (cl-sublis `((term . ,(oref searcher search))
               (word-boundary . "($|[^a-zA-Z0-9\\?\\*-])")
               (space . "[[:space:]]"))
             regexp))

(cl-defmethod dumb-jump-populate-regexp :around ((searcher dumb-jump-searcher) regexp)
  (cl-assert (listp regexp))
  (let ((parts (cl-call-next-method searcher regexp)))
    (cl-assert (cl-every #'stringp parts))
    (apply #'concat parts)))


;;; Utility Functions

(defun dumb-jump-search-dwim ()
  "Attempt to guess what to search."
  (cond ((region-active-p)
         (buffer-substring-no-properties (region-beginning)
                                         (region-end)))
        ((version< emacs-version "24.4")
         (thing-at-point 'symbol))
        (t (thing-at-point 'symbol t))))

(defun dumb-jump-git-root-p (dir)
  "Check if DIR is a git root directory."
  (file-directory-p (expand-file-name ".git" dir)))

(defun dumb-jump-get-project-root (&optional filepath no-default)
  "Attempt to find project root directory.
Keep looking at the parent dir of FILEPATH until a denoter
file/dir is found."
  (let* ((filepath (or filepath
                       (buffer-file-name)
                       default-directory))
         (name (or dumb-jump-project
                   (locate-dominating-file filepath #'dumb-jump-get-config))))
    (if no-default
        (and name (file-name-as-directory (expand-file-name name)))
      (let ((name (or name dumb-jump-default-project)))
        (file-name-as-directory (expand-file-name name))))))

(defun dumb-jump-get-language-from-mode ()
  "Extract the language from the 'major-mode' name."
  (cl-case major-mode
    (emacs-lisp-mode 'elisp)
    (sh 'shell)
    (cperl-mode 'perl)
    (matlab-mode 'matlab)
    (octave-mode 'matlab)))

(defsubst dumb-jump-get-language-by-filename (&optional file)
  "Get the programming language from the FILE."
  (let* ((file (or file
                   (and (buffer-file-name)
                        (file-name-nondirectory (buffer-file-name)))
                   (buffer-name)))
         (filename (if (string= (file-name-extension file) "gz")
                       (file-name-sans-extension file)
                     file))
         (result (cl-find (file-name-extension filename)
                          dumb-jump-language-file-exts
                          :key (lambda (x) (plist-get x :ext))
                          :test #'string=)))
    (plist-get result :language)))

(defsubst dumb-jump-get-mode-base-name ()
  "Get the base name of the mode."
  (let ((name (symbol-name major-mode)))
    (and (string-suffix-p "-mode" name)
         (substring name 0 -5))))

(defsubst dumb-jump-get-language ()
  "Get language from FILE extension and then fallback to using `major-mode' name."
  (or (dumb-jump-get-language-from-mode)
      (dumb-jump-get-language-by-filename)
      (dumb-jump-get-mode-base-name)
      (error "Could not find rules for current buffer")))

(defconst dumb-jump-process-regexp-alist
  '((clojure . "\\`.*?/\\(.+\\)$")
    (ruby . "\\`\\(?:.*::\\|:\\)\\(.*?\\)\\'")
    (crystal . "\\`:\\(.+\\)")
    (systemverilog . "\\``\\(.+\\)"))
  "Alist of regular expressions to process symbols.
Each regular expression must has a group number 1, that matches
the actual symbol name.")

(defun dumb-jump-process-symbol (symbol lang)
  "Process SYMBOL of language LANG at point.
For instance, clojure needs namespace part removed."
  (save-match-data
    (let ((regexp (cdr (assoc lang dumb-jump-process-regexp-alist))))
      (if (and regexp (string-match regexp symbol))
          (match-string 1 symbol)
        symbol))))

(defun dumb-jump-get-lang-by-shell-contents (&optional buffer)
  "Return languages in BUFFER by checking if file extension is mentioned."
  (with-current-buffer (or buffer (current-buffer))
    (catch 'found
      (save-excursion
        (dolist (lang dumb-jump-language-file-exts)
          (goto-char (point-max))
          (when (search-backward-regexp
                 (concat "\\." (plist-get lang :ext) "\\b")
                 nil t)
            (throw 'found (plist-get lang :language))))))))

(defsubst dumb-jump-check-queries (queries ent)
  "Check if all entry ENT satisfies all QUERIES.
See `dumb-jump-table-query' for more details on the structure of
QUERIES."
  (catch 'fail
    (dolist (query queries t)
      (unless (and (nth 1 query)
                   (plist-get ent (nth 0 query))
                   (funcall (or (nth 2 query) #'eq)
                            (nth 1 query)
                            (plist-get ent (nth 0 query))))
        (throw 'fail nil)))))

(defun dumb-jump-table-query (table queries &rest fields)
  "Query TABLE using QUERIES.
TABLE must be a list of plists. QUERIES is a list of lists, where
the first element specifies a field to be tested, the second
value is the value to be tested and an optional third element
defines a relation. If the relation is nil, default to `eq'. The
remaining arguments FIELDS list all FIELDS that the query should
return. The result is an alist, whose keys are the values of
FIELDS, while the values are the non-nil results of the query.

For example

    (dumb-jump-table-query dumb-jump-find-rules
                           '((:language c++)
                             (:supports git-grep memq))
                           :regex :type)

will return all :regex and :type fields from
`dumb-jump-find-rules', where the language of the entry is c++
and the :supports field has git-grep as a member. It should look
something like this:

    ((:regex ...)
     (:type ...))"
  (let ((results (mapcar #'list fields)))
    (dolist (ent table)
      (when (dumb-jump-check-queries queries ent)
        (dolist (field fields)
          (when (plist-get ent field)
            (push (plist-get ent field)
                  (alist-get field results))))))
    results))

(defun dumb-jump-table-query-1 (table queries field)
  "Shorthand variant of `dumb-jump-table-query'.
Useful if the result should only contain one FIELD. See
`dumb-jump-table-query', for more details on TABLE and QUERIES."
  (cdar (dumb-jump-table-query table queries field)))


;;; Configurations

(defun dumb-jump-get-config (dir)
  "Attempt to find configuration file in DIR.
Checks if any file name from `dumb-jump-project-denoters' can be
found in the current directory, except if there is a file named
\".dumbjumpignore\". If nothing was found, nil is returned."
  (unless (file-exists-p (expand-file-name ".dumbjumpignore" dir))
    (cl-loop for denoter in dumb-jump-project-denoters
       when (file-exists-p (expand-file-name denoter dir))
       return it)))

(defun dumb-jump-load-config (searcher &optional config-file)
  "Load CONFIG-FILE data into SEARCHER.
In case CONFIG-FILE is nil, default to \".dumbjump\" in the root
project directory."
  (cl-assert (cl-typep searcher 'dumb-jump-searcher))
  (let ((lang (if (memq major-mode '(shell-mode eshell-mode))
                  (dumb-jump-get-lang-by-shell-contents)
                (dumb-jump-get-language)))
        include exclude
        (file (expand-file-name (or config-file ".dumbjump")
                                (oref searcher root))))
    (with-temp-buffer
      (when (file-exists-p file)
        (insert-file-contents file)
        (let* ((root (oref searcher root))
               (local-root (if (file-remote-p root)
                               (tramp-file-name-localname
                                (tramp-dissect-file-name
                                 root))
                             root)))
          (while (not (eobp))
            (cond ((looking-at "^language \\\(.+\\\)")
                   (setq lang (intern (match-string 1))))
                  ((looking-at "^\\+\\(.+\\)")
                   (push (expand-file-name (match-string 1) local-root)
                         include))
                  ((looking-at "^-/?\\(.+\\)")
                   (push (expand-file-name (match-string 1) local-root)
                         exclude)))
            (forward-line)))))
    (setf (oref searcher language) lang
          (oref searcher excludes) (delete-dups exclude)
          (oref searcher includes)
          (delete-dups (cons (oref searcher root)
                             include)) )
    (let ((syntax (assq lang dumb-jump-comments-alist)))
      (when syntax
        (setf (oref searcher comment-syntax)
              (regexp-quote (cdr syntax)))))
    (let* ((symbol (oref searcher search))
           (search (dumb-jump-process-symbol symbol lang)))
      (setf (oref searcher search) search))))


;;; Query Constructor

(defun dumb-jump-load-context (searcher)
  "Set the context values for SEARCHER."
  (cl-assert (cl-typep searcher 'dumb-jump-searcher))
  (save-match-data
    (with-slots (search) searcher
      (when (looking-at (concat "^\\(.*\\)"
                                (regexp-quote search)
                                "\\(.*?\\)$"))
        (unless (string= (match-string 1) "")
          (setf (oref searcher left-context) (match-string 1)))
        (unless (string= (match-string 2) "")
          (setf (oref searcher right-context) (match-string 2)))))))

(defun dumb-jump-pick-searcher (prompt)
  "Initialise and return a searcher.
If PROMPT is non-nil, set PROMPT as search term. Otherwise try to
locate the symbol at point."
  (let ((try '(dumb-jump-ag                 ; ordered by preference
               dumb-jump-rg
               dumb-jump-gnu-grep
               dumb-jump-grep))
        (root (dumb-jump-get-project-root))
        (search (or prompt (dumb-jump-search-dwim)
                    (user-error "No symbol at point"))))
    (when (dumb-jump-git-root-p root)
      (push 'dumb-jump-git-grep try))
    (when dumb-jump-prefer-searcher
      (push dumb-jump-prefer-searcher try))
    (when dumb-jump-force-searcher
      (setq try (list dumb-jump-force-searcher)))
    (catch 'found
      (let (searcher)
        (dolist (class try)
          (require class)
          (setq searcher (make-instance class
                                        :search search
                                        :root root))
          (when (dumb-jump-check-usable searcher)
            (dumb-jump-load-config searcher)
            (unless prompt
              (dumb-jump-load-context searcher))
            (throw 'found searcher)))
        (error "No usable search tool found")))))

(defun dumb-jump-query (searcher)
  "Run query using SEARCHER."
  (cl-assert (cl-typep searcher 'dumb-jump-searcher))
  (let* ((buf (generate-new-buffer " *dumb-jump-query*"))
         (cmd (dumb-jump-generate-command searcher))
         (process-environment (cons "TERM" process-environment))
         (default-directory (oref searcher root)))
    (cl-assert (listp cmd))
    (cl-assert (cl-every #'stringp cmd))
    (message "Query command: %S" cmd)
    (with-current-buffer buf
      (apply #'process-file (car cmd) nil t nil (cdr cmd))
      (goto-char (point-min))
      (prog1 (dumb-jump-process-results searcher)
        (unless dumb-jump-debug
          (kill-buffer))))))


;;; Result Handling

(cl-defstruct dumb-jump-target path line column context)

(defun dumb-jump-parse-response-line (searcher)
  "Return a `dumb-jump-target' or nil, if line has no data.
SEARCHER is an instance of `dumb-jump-seacher'.
The match data is modified by this function."
  (cl-assert (cl-typep searcher 'dumb-jump-searcher))
  (cl-assert (= (line-beginning-position) (point)))
  (with-slots (root search current-file current-line comment-syntax)
      searcher
    (and (looking-at (concat "^\\(.*\\)\\(?:^\\|:\\)\\([0-9]+\\):\\(\\(.*?\\)\\("
                             (regexp-quote search) "\\).*\\)[[:space:]]*$"))
         ;; if the regular expression was accepted, we have found a
         ;; file-path, a line number and the "context", ie. the entire
         ;; line that was matched.
         (let ((path (expand-file-name
                      (or (match-string 1) current-file)
                      root))
               (line (string-to-number (match-string 2))))
           (and
            ;; check if variable mention is not in a comment
            (if (slot-boundp searcher 'comment-syntax)
                (not (string-match-p comment-syntax (match-string 4)))
              t)
            ;; ignore the result, if it is on the same place where the
            ;; started the query.
            (not (and (file-equal-p path current-file)
                      (= line current-line)))
            ;; target passed all heuristics. return it to the parser.
            ;; TODO: add column
            (make-dumb-jump-target :path path :line line
                                   :column (- (match-beginning 5)
                                              (match-beginning 3))
                                   :context (match-string 3)))))))

(defun dumb-jump-process-results (searcher)
  "Process results in buffer using SEARCHER.
Depending on the dynamic context, this means sorting and
filtering the raw data, and then passing it on to
`dumb-jump-handle-results'."
  (cl-assert (cl-typep searcher 'dumb-jump-searcher))
  (let ((parsed (save-match-data
                  (delete-dups
                   (dumb-jump-parse-response searcher)))))
    (when (< 1 (length parsed))
      (cl-callf2 dumb-jump-sort-results searcher parsed))
    parsed))

(defun dumb-jump-sort-results (searcher parsed)
  "Sort PARSED using SEARCHER."
  (cl-assert (cl-typep searcher 'dumb-jump-searcher))
  (cl-assert (cl-every #'dumb-jump-target-p parsed))
  ;; NB: `sort' is a stable sorting algorithm
  (cl-labels ((order-by-lines (right left)
                (and (file-equal-p (dumb-jump-target-path right)
                                   (dumb-jump-target-path left))
                     (< (dumb-jump-target-line right)
                        (dumb-jump-target-line left))))
              (prefer-closer-line (right left)
                (and (file-equal-p (dumb-jump-target-path left)
                                   (oref searcher current-file))
                     (file-equal-p (dumb-jump-target-path right)
                                   (oref searcher current-file))
                     (< (abs (- (oref searcher current-line)
                                (dumb-jump-target-line right)))
                        (abs (- (oref searcher current-line)
                                (dumb-jump-target-line left))))))
              (relative-path (target)
                (let* ((path (dumb-jump-target-path target))
                       (here (oref searcher current-file))
                       (base (file-name-directory here)))
                  (file-relative-name path base)))
              (file-distance (file)
                (let ((count 0))
                  (while (setq file (file-name-directory file))
	                (setq file (substring file 0 -1)
                          count (1+ count)))
                  count))
              (prefer-closer-file (right left)
                (< (file-distance (relative-path right))
                   (file-distance (relative-path left))))
              (prefer-external (right left)
                (and (file-equal-p (dumb-jump-target-path left)
                                   (oref searcher current-file))
                     (not (file-equal-p (dumb-jump-target-path right)
                                        (oref searcher current-file)))))
              (prefer-local (right left)
                (prefer-external left right)))
    ;; by default, all matches within a file should be sorted by their
    ;; line number.
    (cl-callf sort parsed #'order-by-lines)
    ;; prefer matches closer to the line where `dumb-jump-go' was
    ;; invoked, to those further away.
    (cl-callf sort parsed #'prefer-closer-line)
    ;; prefer files closer to the file system where `dumb-jump-go' was
    ;; invoked, to those further away.
    (cl-callf sort parsed #'prefer-closer-file)
    ;; in case requested, external matches (ie. a match outside of the
    ;; current file) should be listed before file-local matches.
    (if (memq 'external (oref searcher options))
        (cl-callf sort parsed #'prefer-external)
      (cl-callf sort parsed #'prefer-local))
    ;; the "aggressive" mode tells us to only return one result.
    ;; either way, the result should be converted back from a vector
    ;; to a list.
    (if dumb-jump-aggressive
        (list (car parsed))
      parsed)))


;;; Xref Backend

(cl-defmethod xref-backend-definitions ((_backend (eql dumb-jump)) prompt)
  (let* ((inhibit-message (not dumb-jump-debug))
         (searcher (dumb-jump-pick-searcher prompt))
         candidates)
    (dolist (target (dumb-jump-query searcher))
      (push (xref-make
             (format "%s:%d"
                     (file-name-nondirectory
                      (dumb-jump-target-path target))
                     (dumb-jump-target-line target))
             (xref-make-file-location
              (dumb-jump-target-path target)
              (dumb-jump-target-line target)
              (dumb-jump-target-column target)))
            candidates))
    candidates))

(defun dumb-jump-xref-activate ()
  "Function to activate xref backend."
  (and (dumb-jump-get-project-root nil t)
       'dumb-jump))

(provide 'dumb-jump)

;;; dumb-jump.el ends here
