;;; clojure-mode.el --- Major mode for Clojure code -*- lexical-binding: t; -*-

;; Copyright © 2007-2013 Jeffrey Chu, Lennart Staflin, Phil Hagelberg
;; Copyright © 2013-2023 Bozhidar Batsov, Artur Malabarba, Magnar Sveen
;;
;; Authors: Jeffrey Chu <jochu0@gmail.com>
;;       Lennart Staflin <lenst@lysator.liu.se>
;;       Phil Hagelberg <technomancy@gmail.com>
;;       Bozhidar Batsov <bozhidar@batsov.dev>
;;       Artur Malabarba <bruce.connor.am@gmail.com>
;;       Magnar Sveen <magnars@gmail.com>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: https://github.com/clojure-emacs/clojure-mode
;; Keywords: languages clojure clojurescript lisp
;; Version: 5.17.0
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides font-lock, indentation, navigation and basic refactoring for the
;; Clojure programming language (https://clojure.org).

;; Using clojure-mode with paredit or smartparens is highly recommended.

;; Here are some example configurations:

;;   ;; require or autoload paredit-mode
;;   (add-hook 'clojure-mode-hook #'paredit-mode)

;;   ;; require or autoload smartparens
;;   (add-hook 'clojure-mode-hook #'smartparens-strict-mode)

;; See inf-clojure (https://github.com/clojure-emacs/inf-clojure) for
;; basic interaction with Clojure subprocesses.

;; See CIDER (https://github.com/clojure-emacs/cider) for
;; better interaction with subprocesses via nREPL.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:


(defvar calculate-lisp-indent-last-sexp)
(defvar delete-pair-blink-delay)
(defvar font-lock-beg)
(defvar font-lock-end)
(defvar paredit-space-for-delimiter-predicates)
(defvar paredit-version)
(defvar paredit-mode)

(require 'cl-lib)
(require 'imenu)
(require 'newcomment)
(require 'thingatpt)
(require 'subr-x)
(require 'lisp-mnt)

(declare-function lisp-fill-paragraph  "lisp-mode" (&optional justify))

(defgroup clojure nil
  "Major mode for editing Clojure code."
  :prefix "clojure-"
  :group 'languages
  :link '(url-link :tag "GitHub" "https://github.com/clojure-emacs/clojure-mode")
  :link '(emacs-commentary-link :tag "Commentary" "clojure-mode"))

(defconst clojure-mode-version
  (eval-when-compile
    (lm-version (or load-file-name buffer-file-name)))
  "The current version of `clojure-mode'.")

(defcustom clojure-docstring-fill-column fill-column
  "Value of `fill-column' to use when filling a docstring."
  :type 'integer
  :safe 'integerp)

(defcustom clojure-docstring-fill-prefix-width 2
  "Width of `fill-prefix' when filling a docstring.
The default value conforms with the de facto convention for
Clojure docstrings, aligning the second line with the opening
double quotes on the third column."
  :type 'integer
  :safe 'integerp)

(defcustom clojure-omit-space-between-tag-and-delimiters '(?\[ ?\{ ?\()
  "Allowed opening delimiter characters after a reader literal tag.
For example, \[ is allowed in :db/id[:db.part/user]."
  :type '(set (const :tag "[" ?\[)
              (const :tag "{" ?\{)
              (const :tag "(" ?\()
              (const :tag "\"" ?\"))
  :safe (lambda (value)
          (and (listp value)
               (cl-every 'characterp value))))

(defcustom clojure-directory-prefixes
  '("\\`clj[scxd]?\\.")
  "A list of directory prefixes used by `clojure-expected-ns'.
The prefixes are used to generate the correct namespace."
  :type '(repeat string)
  :package-version '(clojure-mode . "5.14.0")
  :safe (lambda (value)
          (and (listp value)
               (cl-every 'stringp value))))

(defvar clojure-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map prog-mode-map)
    (easy-menu-define clojure-mode-menu map "Clojure Mode Menu"
      '("Clojure"
        ("ns forms"
         ["Insert ns form at the top" clojure-insert-ns-form]
         ["Insert ns form here" clojure-insert-ns-form-at-point])))
    map)
  "Keymap for Clojure mode.")

(defvar clojure-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Initialize ASCII charset as symbol syntax
    (modify-syntax-entry '(0 . 127) "_" table)

    ;; Word syntax
    (modify-syntax-entry '(?0 . ?9) "w" table)
    (modify-syntax-entry '(?a . ?z) "w" table)
    (modify-syntax-entry '(?A . ?Z) "w" table)

    ;; Whitespace
    (modify-syntax-entry ?\s " " table)
    (modify-syntax-entry ?\xa0 " " table) ; non-breaking space
    (modify-syntax-entry ?\t " " table)
    (modify-syntax-entry ?\f " " table)
    ;; Setting commas as whitespace makes functions like `delete-trailing-whitespace' behave unexpectedly (#561)
    (modify-syntax-entry ?, "." table)

    ;; Delimiters
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)

    ;; Prefix chars
    (modify-syntax-entry ?` "'" table)
    (modify-syntax-entry ?~ "'" table)
    (modify-syntax-entry ?^ "'" table)
    (modify-syntax-entry ?@ "'" table)
;;    (modify-syntax-entry ?? "_ p" table) ; ? is a prefix outside symbols - LG: this broke my indentation impl, error on (scan-sexps ... -1)
    (modify-syntax-entry ?# "_ p" table) ; # is allowed inside keywords (#399)
    (modify-syntax-entry ?' "_ p" table) ; ' is allowed anywhere but the start of symbols

    ;; Others
    (modify-syntax-entry ?\; "<" table) ; comment start
    (modify-syntax-entry ?\n ">" table) ; comment end
    (modify-syntax-entry ?\" "\"" table) ; string
    (modify-syntax-entry ?\\ "\\" table) ; escape

    table)
  "Syntax table for Clojure mode.")

(defun clojure-space-for-delimiter-p (endp delim)
  "Prevent paredit from inserting useless spaces.
See `paredit-space-for-delimiter-predicates' for the meaning of
ENDP and DELIM."
  (and (not endp)
       ;; don't insert after opening quotes, auto-gensym syntax, or reader tags
       (not (looking-back
             (if (member delim clojure-omit-space-between-tag-and-delimiters)
                 "\\_<\\(?:'+\\|#.*\\)"
               "\\_<\\(?:'+\\|#\\)")
             (line-beginning-position)))))

(declare-function paredit-open-curly "ext:paredit" t t)
(declare-function paredit-close-curly "ext:paredit" t t)
(declare-function paredit-convolute-sexp "ext:paredit")

(defvar clojure--let-regexp
  "\(\\(when-let\\|if-let\\|let\\)\\(\\s-*\\|\\[\\)"
  "Regexp matching let like expressions, i.e. \"let\", \"when-let\", \"if-let\".

The first match-group is the let expression.

The second match-group is the whitespace or the opening square
bracket if no whitespace between the let expression and the
bracket.")

(defun clojure--replace-let-bindings-and-indent (&rest _)
  "Replace let bindings and indent."
  (save-excursion
    (backward-sexp)
    (when (looking-back clojure--let-regexp nil)
      (clojure--replace-sexps-with-bindings-and-indent))))

(defun clojure-paredit-setup (&optional keymap)
  "Make \"paredit-mode\" play nice with `clojure-mode'.

If an optional KEYMAP is passed the changes are applied to it,
instead of to `clojure-mode-map'."
  (when (>= paredit-version 21)
    (let ((keymap (or keymap clojure-mode-map)))
      (define-key keymap "{" #'paredit-open-curly)
      (define-key keymap "}" #'paredit-close-curly))
    (make-local-variable 'paredit-space-for-delimiter-predicates)
    (add-to-list 'paredit-space-for-delimiter-predicates
                 #'clojure-space-for-delimiter-p)))

(defun clojure-mode-variables ()
  "Set up initial buffer-local variables for Clojure mode."
  (add-to-list 'imenu-generic-expression '(nil clojure-match-next-def 0))
  (setq-local indent-tabs-mode nil)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local outline-regexp ";;;;* ")
  (setq-local outline-level 'lisp-outline-level)
  (setq-local comment-start ";")
  (setq-local comment-start-skip ";+ *")
  (setq-local comment-add 1) ; default to `;;' in comment-region
  (setq-local comment-column 40)
  (setq-local comment-use-syntax t)
  (setq-local multibyte-syntax-as-symbol t)
  (setq-local electric-pair-skip-whitespace 'chomp)
  (setq-local electric-pair-open-newline-between-pairs nil)
  (setq-local fill-paragraph-function #'clojure-fill-paragraph)
  (setq-local adaptive-fill-function #'clojure-adaptive-fill-function)
  (setq-local normal-auto-fill-function #'clojure-auto-fill-function)
  (setq-local comment-start-skip
              "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  (setq-local indent-line-function #'clojure-indent-line)
  (setq-local lisp-indent-function #'clojure-indent-function)
  (setq-local lisp-doc-string-elt-property 'clojure-doc-string-elt)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local open-paren-in-column-0-is-defun-start nil))

(defsubst clojure-in-docstring-p ()
  "Check whether point is in a docstring."
  (let ((ppss (syntax-ppss)))
    ;; are we in a string?
    (when (nth 3 ppss)
      ;; check font lock at the start of the string
      (eq (get-text-property (nth 8 ppss) 'face)
          'font-lock-doc-face))))

;;;###autoload
(define-derived-mode clojure-mode prog-mode "Clojure"
  "Major mode for editing Clojure code.

\\{clojure-mode-map}"
  (clojure-mode-variables)
  (clojure-font-lock-setup)
  (add-hook 'paredit-mode-hook #'clojure-paredit-setup)
  ;; `electric-layout-post-self-insert-function' prevents indentation in strings
  ;; and comments, force indentation of non-inlined docstrings:
  (add-hook 'electric-indent-functions
            (lambda (_char) (if (and (clojure-in-docstring-p)
                                     ;; make sure we're not dealing with an inline docstring
                                     ;; e.g. (def foo "inline docstring" bar)
                                     (save-excursion
                                       (beginning-of-line-text)
                                       (eq (get-text-property (point) 'face)
                                           'font-lock-doc-face)))
                                'do-indent))))

(defcustom clojure-verify-major-mode t
  "If non-nil, warn when activating the wrong `major-mode'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(clojure-mode "5.3.0"))

(defun clojure--check-wrong-major-mode ()
  "Check if the current `major-mode' matches the file extension.

If it doesn't, issue a warning if `clojure-verify-major-mode' is
non-nil."
  (when (and clojure-verify-major-mode
             (stringp (buffer-file-name)))
    (let* ((case-fold-search t)
           (problem (cond ((and (string-match "\\.clj\\'" (buffer-file-name))
                                (not (eq major-mode 'clojure-mode)))
                           'clojure-mode)
                          ((and (string-match "\\.cljs\\'" (buffer-file-name))
                                (not (eq major-mode 'clojurescript-mode)))
                           'clojurescript-mode)
                          ((and (string-match "\\.cljc\\'" (buffer-file-name))
                                (not (eq major-mode 'clojurec-mode)))
                           'clojurec-mode))))
      (when problem
        (message "[WARNING] %s activated `%s' instead of `%s' in this buffer.
This could cause problems.
\(See `clojure-verify-major-mode' to disable this message.)"
                 (if (eq major-mode real-this-command)
                     "You have"
                   "Something in your configuration")
                 major-mode
                 problem)))))

(add-hook 'clojure-mode-hook #'clojure--check-wrong-major-mode)

(defsubst clojure-docstring-fill-prefix ()
  "The prefix string used by `clojure-fill-paragraph'.
It is simply `clojure-docstring-fill-prefix-width' number of spaces."
  (make-string clojure-docstring-fill-prefix-width ? ))

(defun clojure-adaptive-fill-function ()
  "Clojure adaptive fill function.
This only takes care of filling docstring correctly."
  (when (clojure-in-docstring-p)
    (clojure-docstring-fill-prefix)))

(defun clojure-fill-paragraph (&optional justify)
  "Like `fill-paragraph', but can handle Clojure docstrings.
If JUSTIFY is non-nil, justify as well as fill the paragraph."
  (if (clojure-in-docstring-p)
      (let ((paragraph-start
             (concat paragraph-start
                     "\\|\\s-*\\([(:\"[]\\|~@\\|`(\\|#'(\\)"))
            (paragraph-separate
             (concat paragraph-separate "\\|\\s-*\".*[,\\.]$"))
            (fill-column (or clojure-docstring-fill-column fill-column))
            (fill-prefix (clojure-docstring-fill-prefix)))
        ;; we are in a string and string start pos (8th element) is non-nil
        (let* ((beg-doc (nth 8 (syntax-ppss)))
               (end-doc (save-excursion
                          (goto-char beg-doc)
                          (or (ignore-errors (forward-sexp) (point))
                              (point-max)))))
          (save-restriction
            (narrow-to-region beg-doc end-doc)
            (fill-paragraph justify))))
    (let ((paragraph-start (concat paragraph-start
                                   "\\|\\s-*\\([(:\"[]\\|`(\\|#'(\\)"))
          (paragraph-separate
           (concat paragraph-separate "\\|\\s-*\".*[,\\.[]$")))
      (or (fill-comment-paragraph justify)
          (fill-paragraph justify))
      ;; Always return `t'
      t)))

(defun clojure-auto-fill-function ()
  "Clojure auto-fill function."
  ;; Check if auto-filling is meaningful.
  (let ((fc (current-fill-column)))
    (when (and fc (> (current-column) fc))
      (let ((fill-column (if (clojure-in-docstring-p)
                             clojure-docstring-fill-column
                           fill-column))
            (fill-prefix (clojure-adaptive-fill-function)))
        (do-auto-fill)))))


;;; #_ comments font-locking
;; Code heavily borrowed from Slime.
;; https://github.com/slime/slime/blob/master/contrib/slime-fontifying-fu.el#L186
(defvar clojure-comment-regexp
  (rx (seq (+ (seq "#_" (* " ")))) (group-n 1 (not (any " "))))
  "Regexp matching the start of a comment sexp.
The beginning of match-group 1 should be before the sexp to be
marked as a comment.  The end of sexp is found with
`clojure-forward-logical-sexp'.")

;; Navigation
(defun clojure-skip-meta ()
  "If the expression at point is a metadata annotation, skip it.  Repeat."
  (while (looking-at "[[:space:]]*\\^[[:space:]]*")
    (goto-char (match-end 0))
    (forward-sexp)))

(defun clojure-skip-comment ()
  "If the expression at point is a #_ comment form, skip it,
including the entire commented form"
  (when (looking-at "#_")
    (goto-char (match-end 0)) ;; after comment literal
    (clojure-forward-exp t)))

(defun clojure-forward-tagged-literal ()
  (when (looking-at (concat "#[[:space:]]*"
			    (concat "[^_]" ;; otherwise its reader comment
				    clojure--sym-regexp)))
    (goto-char (match-end 0))
    (clojure-forward-exp)
    t))

(defun clojure-forward-namespaced-map ()
  (when (looking-at (concat "\\(""#:"
			    clojure--keyword-sym-regexp
			    "[[:space:]]*" "\\)"
			    "{"))
    (goto-char (match-end 1))
    (forward-sexp)
    t))

(defun clojure-skip-whitespace ()
  (when (looking-at "[[:space:]]+")
    (goto-char (match-end 0))))

(defun clojure-forward-exp (&optional skip-comment)
  (clojure-skip-meta) ;; can skip whitespace by itself
  (clojure-skip-whitespace)
  (when skip-comment
    (clojure-skip-comment))
  (or
   (clojure-forward-tagged-literal)
   (clojure-forward-namespaced-map)
   (forward-sexp)))

(defun clojure-backward-exp-1 ()
  (let* ((beginning-of-current (scan-sexps (point) -1))
	 (end-of-current (scan-sexps beginning-of-current 1))
	 found)
    (while (progn
	     (ignore-error scan-error
	       (let ((start (scan-sexps (point) -1)))
		 (when start
		   (goto-char start)
		   (clojure-forward-exp nil)
		   (when (= (point) end-of-current)
		     (goto-char start)
		     (setq found start)))))))
    (goto-char found)))

(defun clojure--search-comment-macro-internal (limit)
  "Search for a comment forward stopping at LIMIT."
  (when (search-forward-regexp clojure-comment-regexp limit t)
    (let* ((md (match-data))
           (start (match-beginning 1))
           (state (syntax-ppss start)))
      ;; inside string or comment?
      (if (or (nth 3 state)
              (nth 4 state))
          (clojure--search-comment-macro-internal limit)
        (goto-char start)
        ;; Count how many #_ we got and step by that many sexps
        ;; For (comment ...), step at least 1 sexp
        (clojure-forward-logical-sexp
         (max (count-matches (rx "#_") (elt md 0) (elt md 1))
              1))
        ;; Data for (match-end 1).
        (setf (elt md 3) (point))
        (set-match-data md)
        t))))

(defun clojure--search-comment-macro (limit)
  "Find comment macros and set the match data.
Search from point up to LIMIT.  The region that should be
considered a comment is between `(match-beginning 1)'
and `(match-end 1)'."
  (let ((result 'retry))
    (while (and (eq result 'retry) (<= (point) limit))
      (condition-case nil
          (setq result (clojure--search-comment-macro-internal limit))
        (end-of-file (setq result nil))
        (scan-error  (setq result 'retry))))
    result))


;;; General font-locking
(defun clojure-match-next-def ()
  "Scans the buffer backwards for the next \"top-level\" definition.
Called by `imenu--generic-function'."
  ;; we have to take into account namespace-definition forms
  ;; e.g. s/defn
  (when (re-search-backward "^[ \t]*(\\([a-z0-9.-]+/\\)?\\(def\\sw*\\)" nil t)
    (save-excursion
      (let (found?
            (deftype (match-string 2))
            (start (point)))
        ;; ignore user-error from down-list when called from inside a string or comment
        ;; TODO: a better workaround would be to wrap it in
        ;; unless (ppss-comment-or-string-start (syntax-ppss)) instead of ignore-errors,
        ;; but ppss-comment-or-string-start is only available since Emacs 27
        (ignore-errors
          (down-list))
        (forward-sexp)
        (while (not found?)
          (ignore-errors
            (forward-sexp))
          (or (when (char-equal ?\[ (char-after (point)))
                (backward-sexp))
              (when (char-equal ?\) (char-after (point)))
                (backward-sexp)))
          (cl-destructuring-bind (def-beg . def-end) (bounds-of-thing-at-point 'sexp)
            (when (char-equal ?^ (char-after def-beg))
              ;; move to the beginning of next sexp
              (progn (forward-sexp) (backward-sexp)))
            (when (or (not (char-equal ?^ (char-after def-beg)))
                      (and (char-equal ?^ (char-after (point))) (= def-beg (point))))
              (setq found? t)
              (when (string= deftype "defmethod")
                (setq def-end (progn (goto-char def-end)
                                     (forward-sexp)
                                     (point))))
              (set-match-data (list def-beg def-end)))))
        (goto-char start)))))

(eval-and-compile
  (defconst clojure--sym-forbidden-rest-chars "][\";@\\^`~\(\)\{\}\\,\s\t\n\r"
    "A list of chars that a Clojure symbol cannot contain.
See definition of `macros': URL `https://git.io/vRGLD'.")
  (defconst clojure--sym-forbidden-1st-chars (concat clojure--sym-forbidden-rest-chars "0-9:'")
    "A list of chars that a Clojure symbol cannot start with.
See the for-loop: URL `https://git.io/vRGTj' lines: URL
`https://git.io/vRGIh', URL `https://git.io/vRGLE' and value
definition of `macros': URL `https://git.io/vRGLD'.")
  (defconst clojure--sym-regexp
    (concat "[^" clojure--sym-forbidden-1st-chars "][^" clojure--sym-forbidden-rest-chars "]*")
    "A regexp matching a Clojure symbol or namespace alias.
Matches the rule `clojure--sym-forbidden-1st-chars' followed by
any number of matches of `clojure--sym-forbidden-rest-chars'.")
  (defconst clojure--keyword-sym-forbidden-1st-chars
    (concat clojure--sym-forbidden-rest-chars ":'")
    "A list of chars that a Clojure keyword symbol cannot start with.")
  (defconst clojure--keyword-sym-regexp
    (concat "[^" clojure--keyword-sym-forbidden-1st-chars "]"
            "[^" clojure--sym-forbidden-rest-chars "]*")
    "A regexp matching a Clojure keyword name or keyword namespace.
Matches the rule `clojure--keyword-sym-forbidden-1st-chars' followed by
any number of matches of `clojure--sym-forbidden-rest-chars'."))

(defconst clojure-font-lock-keywords
  (eval-when-compile
    `(;; Any def form
      (,(concat "(\\(?:" clojure--sym-regexp "/\\)?"
                "\\("
                (regexp-opt '("def"
                              "defonce"
                              "defn"
                              "defn-"
                              "defmacro"
                              "definline"
                              "defmulti"
                              "defmethod"
                              "defprotocol"
                              "definterface"
                              "defrecord"
                              "deftype"
                              "defstruct"
                              ;; clojure.test
                              "deftest"
                              "deftest-"))
                "\\)\\>")
       (1 font-lock-keyword-face))
      ;; Top-level variable definition
      (,(concat "(\\(?:clojure.core/\\)?\\("
                (regexp-opt '("def" "defonce"))
                ;; variable declarations
                "\\)\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                ;; Possibly type or metadata
                "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                "\\(\\sw+\\)?")
       (2 font-lock-variable-name-face nil t))
      ;; Type definition
      (,(concat "(\\(?:clojure.core/\\)?\\("
                (regexp-opt '("defstruct" "deftype" "defprotocol"
                              "defrecord")) ;; LG: why no definterface?
                ;; type declarations
                "\\)\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                ;; Possibly type or metadata
                "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                "\\(\\sw+\\)?")
       (2 font-lock-type-face nil t))
      ;; Function definition
      (,(concat "(\\(?:clojure.core/\\)?\\("
                (regexp-opt '("defn"
                              "defn-"
                              "defmulti"
                              "defmethod"
                              "deftest"
                              "deftest-"
                              "defmacro"
                              "definline"))
                "\\)"
                ;; Function declarations
                "\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                ;; Possibly type or metadata
                "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                (concat "\\(" clojure--sym-regexp "\\)?"))
       (2 font-lock-function-name-face nil t))
      ;; (fn name? args ...)
      (,(concat "(\\(?:clojure.core/\\)?\\(fn\\)[ \t]+"
                ;; Possibly type
                "\\(?:#?^\\sw+[ \t]*\\)?"
                ;; Possibly name
                "\\(\\sw+\\)?" )
       (2 font-lock-function-name-face nil t))
      ;; Special forms
      (,(concat
         "("
         (regexp-opt
          '("do" "if" "let*" "var" "fn" "fn*" "loop*"
            "recur" "throw" "try" "catch" "finally"
            "set!" "new" "."
            "monitor-enter" "monitor-exit" "quote") t)
         "\\>")
       1 font-lock-keyword-face)
      ;; Built-in binding and flow of control forms
      (,(concat
         "(\\(?:clojure.core/\\)?"
         (regexp-opt
          '(
            "->"
            "->>"
            ".."
            "as->"
            "assert"
            "binding"
            "bound-fn"
            "case"
            "comment"
            "cond"
            "cond->"
            "cond->>"
            "condp"
            "declare"
            "delay"
            "doseq"
            "dosync"
            "dotimes"
            "doto"
            "extend-protocol"
            "extend-type"
            "for"
            "gen-class"
            "gen-interface"
            "if-let"
            "if-not"
            "if-some"
            "import"
            "in-ns"
            "lazy-seq"
            "let"
            "letfn"
            "locking"
            "loop"
            "ns"
            "proxy"
            "proxy-super"
            "reify"
            "some->"
            "some->>"
            "sync"
            "when"
            "when-first"
            "when-let"
            "when-not"
            "when-some"
            "while"
            "with-bindings"
            "with-in-str"
            "with-loading-context"
            "with-local-vars"
            "with-open"
            "with-out-str"
            "with-redefs"
            "with-redefs-fn"
            )
          t)
         "\\>")
       1 font-lock-keyword-face)
      ;; LG: Not sure if I ever liked this
      ;; ;; Macros similar to let, when, and while
      ;; (,(rx symbol-start
      ;;       (or "let" "when" "while") "-"
      ;;       (1+ (or (syntax word) (syntax symbol)))
      ;;       symbol-end)
      ;;  0 font-lock-keyword-face)
      ;; Dynamic variables - *something* or @*something*
      (,(concat "\\(?:\\<\\|/\\)@?\\(\\*" clojure--sym-regexp "\\*\\)\\>")
       1 font-lock-variable-name-face)
      ;; Global constants - nil, true, false
      (,(concat
         "\\<"
         (regexp-opt
          '("true" "false" "nil") t)
         "\\>")
       0 font-lock-constant-face)
      ;; namespace definitions: (ns foo.bar)
      (,(concat "(\\<ns\\>[ \r\n\t]*"
                ;; Possibly metadata, shorthand and/or longhand
                "\\(?:\\^?\\(?:{[^}]+}\\|:[^ \r\n\t]+[ \r\n\t]\\)[ \r\n\t]*\\)*"
                ;; namespace
                "\\(" clojure--sym-regexp "\\)")
       (1 font-lock-type-face))

      ;; TODO dedupe the code for matching of keywords, type-hints and unmatched symbols

      ;; keywords: {:oneword/ve/yCom|pLex.stu-ff 0}
      (,(concat "\\(:\\{1,2\\}\\)\\(" clojure--keyword-sym-regexp "?\\)\\(/\\)"
                "\\(" clojure--keyword-sym-regexp "\\)")
       (1 'font-lock-builtin-face)
       (2 'font-lock-builtin-face)
       (3 'default)
       (4 'font-lock-builtin-face))
      (,(concat "\\(:\\{1,2\\}\\)\\(" clojure--keyword-sym-regexp "\\)")
       (1 'font-lock-builtin-face)
       (2 'font-lock-builtin-face))

      ;; #_ and (comment ...) macros.
      (clojure--search-comment-macro 1 font-lock-comment-face t)
      ;; Highlight `code` marks, just like `elisp'.
      (,(rx "`" (group-n 1 (optional "#'")
                         (+ (or (syntax symbol) (syntax word)))) "`")
       (1 'font-lock-constant-face prepend))
      ;; Highlight escaped characters in strings.
      (clojure-font-lock-escaped-chars 0 'bold prepend)))
  "Default expressions to highlight in Clojure mode.")

(defun clojure-font-lock-syntactic-face-function (state)
  "Find and highlight text with a Clojure-friendly syntax table.

This function is passed to `font-lock-syntactic-face-function',
which is called with a single parameter, STATE (which is, in
turn, returned by `parse-partial-sexp' at the beginning of the
highlighted region)."
  (if (nth 3 state)
      ;; This is a (doc)string
      (let* ((startpos (nth 8 state))
             (listbeg (nth 1 state))
             (firstsym (and listbeg
                            (save-excursion
                              (goto-char listbeg)
                              (and (looking-at "([ \t\n]*\\(\\(\\sw\\|\\s_\\)+\\)")
                                   (match-string 1)))))
             (docelt (and firstsym
                          (function-get (intern-soft firstsym)
                                        lisp-doc-string-elt-property))))
        (if (and docelt
                 ;; It's a string in a form that can have a docstring.
                 ;; Check whether it's in docstring position.
                 (save-excursion
                   (when (functionp docelt)
                     (goto-char (match-end 1))
                     (setq docelt (funcall docelt)))
                   (goto-char listbeg)
                   (forward-char 1)
                   (ignore-errors
                     (while (and (> docelt 0) (< (point) startpos)
                                 (progn (forward-sexp 1) t))
                       ;; ignore metadata and type hints
                       (unless (looking-at "[ \n\t]*\\(\\^[A-Z:].+\\|\\^?{.+\\)")
                         (setq docelt (1- docelt)))))
                   (and (zerop docelt) (<= (point) startpos)
                        (progn (forward-comment (point-max)) t)
                        (= (point) (nth 8 state))))
                 ;; In a def, at last position is not a docstring
                 (not (and (string= "def" firstsym)
                           (save-excursion
                             (goto-char startpos)
                             (goto-char (end-of-thing 'sexp))
                             (looking-at "[ \r\n\t]*\)")))))
            font-lock-doc-face
          font-lock-string-face))
    font-lock-comment-face))

(defun clojure-font-lock-setup ()
  "Configures font-lock for editing Clojure code."
  (setq-local font-lock-multiline t)
  (setq font-lock-defaults
        '(clojure-font-lock-keywords    ; keywords
          nil nil
          (("+-*/.<>=!?$%_&:" . "w")) ; syntax alist
          nil
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function
           . clojure-font-lock-syntactic-face-function))))

(defun clojure--font-locked-as-string-p (&optional regexp)
  "Non-nil if the char before point is font-locked as a string.
If REGEXP is non-nil, also check whether current string is
preceeded by a #."
  (let ((face (get-text-property (1- (point)) 'face)))
    (and (or (and (listp face)
                  (memq 'font-lock-string-face face))
             (eq 'font-lock-string-face face))
         (or (clojure-string-start t)
             (unless regexp
               (clojure-string-start nil))))))

(defun clojure-font-lock-escaped-chars (bound)
  "Highlight \escaped chars in strings.
BOUND denotes a buffer position to limit the search."
  (let ((found nil))
    (while (and (not found)
                (re-search-forward "\\\\." bound t))

      (setq found (clojure--font-locked-as-string-p)))
    found))


;; Docstring positions
(put 'ns 'clojure-doc-string-elt 2)
(put 'def 'clojure-doc-string-elt 2)
(put 'defn 'clojure-doc-string-elt 2)
(put 'defn- 'clojure-doc-string-elt 2)
(put 'defmulti 'clojure-doc-string-elt 2)
(put 'defmacro 'clojure-doc-string-elt 2)
(put 'definline 'clojure-doc-string-elt 2)
(put 'defprotocol 'clojure-doc-string-elt 2)

;;; Indentation
(defun clojure-indent-line ()
  "Indent current line as Clojure code."
  (if (clojure-in-docstring-p)
      (save-excursion
        (beginning-of-line)
        (when (and (looking-at "^\\s-*")
                   (<= (string-width (match-string-no-properties 0))
                       (string-width (clojure-docstring-fill-prefix))))
          (replace-match (clojure-docstring-fill-prefix))))
    (lisp-indent-line)))

(defconst clojure-method-body-indent-2
  '(defrecord deftype reify extend-type extend-protocol)
  "Forms that have a method body on nesting level 2")

(defconst clojure-method-body-indent-3
  '(letfn)
  "Forms that have a method body on nesting level 3")

(defun clojure-method-body-p (state depth qualifiers)
  "Are we in a defrecord etc. body"
  (if (<= depth (car state))
      (let ((declbeg (car (last (elt state 9) depth))))
	(goto-char declbeg) ;; on bracket
	(forward-char)
	(clojure-skip-whitespace)
	(memq (symbol-at-point) qualifiers))))

(defconst clojure-defform
  '(ns
    fn
    def
    defn
    bound-fn
    if
    if-not
    case
    cond
    condp
    cond->
    cond->>
    when
    while
    when-not
    when-first
    do
    delay
    future
    comment
    doto
    locking
    as->

    extend
    extend-type
    extend-protocol
    ;; specify and specify! are from ClojureScript
    specify
    specify!
    try
    catch
    finally

    ;; binding forms
    let
    letfn
    binding
    loop
    for
    doseq
    dotimes
    when-let
    if-let
    when-some
    if-some
    this-as ; ClojureScript

    defmethod

    ;; clojure.test
    testing
    deftest
    are
    use-fixtures

    ;; core.async for backcompat with clojure-mode
    go
    go-loop
    thread)
  "Names of of forms that are supposed to get defform indent")

(defun clojure-defform-p (state)
  (let ((declbeg (elt state 1)))
    (goto-char declbeg)
    (forward-char)
    (clojure-skip-whitespace)
    (let ((s (symbol-at-point)))
      (or (string-prefix-p "def" (symbol-name (symbol-at-point)))
	  (memq s clojure-defform)))))

(defun clojure-datastructure-body-p (state)
  (let ((declbeg (elt state 1)))
    (goto-char declbeg)
    (looking-at-p "[{\[]")))

(defun clojure-indent-function (indent-point state)
  (cond
   ((clojure-datastructure-body-p state)
    (1+ (current-column)))
   ((or (clojure-defform-p state)
	(clojure-method-body-p state 2 clojure-method-body-indent-2)
	(clojure-method-body-p state 3 clojure-method-body-indent-3))
    (lisp-indent-defform state indent-point))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Better docstring filling for clojure-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clojure-string-start (&optional regex)
  "Return the position of the \" that begins the string at point.
If REGEX is non-nil, return the position of the # that begins the
regex at point.  If point is not inside a string or regex, return
nil."
  (when (nth 3 (syntax-ppss)) ;; Are we really in a string?
    (let* ((beg (nth 8 (syntax-ppss)))
           (hash (eq ?# (char-before beg))))
      (if regex
          (and hash (1- beg))
        (and (not hash) beg)))))



(defun clojure-project-dir (&optional dir-name)
  "Leaving in for CIDER for now"
  (clojure-project-root-path dir-name))

(defun clojure-project-root-path (&optional dir-name)
  "Return the absolute path to the project's root directory.

Use `default-directory' if DIR-NAME is nil.
Return nil if not inside a project."
  (locate-dominating-file (or dir-name default-directory) "deps.edn"))

(defun clojure-project-relative-path (path)
  "Denormalize PATH by making it relative to the project root."
  (file-relative-name path (clojure-project-dir)))


;;; ns manipulation
(defun clojure-expected-ns (&optional path)
  "Return the namespace matching PATH.

PATH is expected to be an absolute file path.

If PATH is nil, use the path to the file backing the current buffer."
  (let* ((path (or path (file-truename (buffer-file-name))))
         (relative (clojure-project-relative-path path))
         (sans-file-type (substring relative 0 (- (length (file-name-extension path t)))))
         (sans-file-sep (mapconcat 'identity (cdr (split-string sans-file-type "/")) "."))
         (sans-underscores (replace-regexp-in-string "_" "-" sans-file-sep)))
    ;; Drop prefix from ns for projects with structure src/{clj,cljs,cljc}
    (cl-reduce (lambda (a x) (replace-regexp-in-string x "" a))
               clojure-directory-prefixes
               :initial-value sans-underscores)))

(defun clojure-insert-ns-form-at-point ()
  "Insert a namespace form at point."
  (interactive)
  (insert (format "(ns %s)" (clojure-expected-ns))))

(defun clojure-insert-ns-form ()
  "Insert a namespace form at the beginning of the buffer."
  (interactive)
  (widen)
  (goto-char (point-min))
  (clojure-insert-ns-form-at-point))

;; For temporary CIDER support
(defconst clojure--prettify-symbols-alist nil)

(defun clojure-find-ns ()
  (clojure-expected-ns))

(defun clojure-forward-logical-sexp (&optional n)
  (unless n (setq n 1))
  (if (< n 0)
      (clojure-backward-logical-sexp (- n))
    (clojure-forward-exp t)))

(defun clojure-backward-logical-sexp (&optional n)
  (unless n (setq n 1))
  (if (< n 0)
      (clojure-forward-logical-sexp (- n))
    (clojure-backward-exp-1)))

;;; ClojureScript
(defconst clojurescript-font-lock-keywords
  (eval-when-compile
    `(;; ClojureScript built-ins
      (,(concat "(\\(?:\.*/\\)?"
                (regexp-opt '("js-obj" "js-delete" "clj->js" "js->clj"))
                "\\>")
       0 font-lock-builtin-face)))
  "Additional font-locking for `clojurescript-mode'.")

;;;###autoload
(define-derived-mode clojurescript-mode clojure-mode "ClojureScript"
  "Major mode for editing ClojureScript code.

\\{clojurescript-mode-map}"
  (font-lock-add-keywords nil clojurescript-font-lock-keywords))

;;;###autoload
(define-derived-mode clojurec-mode clojure-mode "ClojureC"
  "Major mode for editing ClojureC code.

\\{clojurec-mode-map}")

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.cljc\\'" . clojurec-mode))
  (add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojurescript-mode)))

(provide 'clojure-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; clojure-mode.el ends here
