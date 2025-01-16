;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "zuzhi"
      user-mail-address "zuzhi.hu@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; ------------------------------------------------------------------------------
;;
;; Frame
;; Maximize Emacs frame on start-up
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;
;; Org
;; Add a timestamp when a 'TODO' item closed.
(setq org-log-done 'time)

;; File watchers
;; Increase the file watch warning threshold, the default is 1000
(setq lsp-file-watch-threshold 4000)

;; JDL Mode
;; Load the package
(load "jdl-mode")

;; PDF Tools
;; Prerequisite: https://github.com/politza/pdf-tools#compiling-on-os-x
;; Activate pdf-tools
;;(pdf-loader-install) ;; FIXME void-function error
;; Uses more memory; see https://github.com/politza/pdf-tools/issues/51
;;(setq pdf-view-use-scaling t
;;      pdf-view-use-imagemagick nil)

;; Org todo
(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'tree))

;; Camel case for yasnippets
(defun camelcase (string)
  (concat (downcase (substring string 0 1)) (substring string 1)))

;; Horizontal recenter
;; https://stackoverflow.com/questions/1249497/command-to-center-screen-horizontally-around-cursor-on-emacs
(defun my-horizontal-recenter ()
  "make the point horizontally centered in the window"
  (interactive)
  (let ((mid (/ (window-width) 2))
        (cur (current-column)))
    (if (< mid cur)
        (set-window-hscroll (selected-window)
                            (- cur mid)))))
(global-set-key (kbd "C-S-l") 'my-horizontal-recenter)

;; Set eshell aliases
(set-eshell-alias! "up" "eshell-up $1")

;; Fonts
(setq doom-font (font-spec :family "IBM Plex Mono" :size 14 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "IBM Plex Mono" :size 14))

;; https://github.com/Alexander-Miller/treemacs/issues/626#issuecomment-707471200
;; try to fix treemacs, doesn't work though
(setq lsp-enable-links nil
      treemacs-follow-mode nil)

;; accept completion from copilot and fallback to company
;;(use-package! copilot
;;  :hook (prog-mode . copilot-mode)
;;  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
;;         ("C-<tab>" . 'copilot-accept-completion-by-word)
;;         :map copilot-completion-map
;;         ("<tab>" . 'copilot-accept-completion)
;;         ("TAB" . 'copilot-accept-completion)))


;; use javascript-mode for .mjs files, typescript-mode for .mts files
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.mts\\'" . typescript-mode))

;;
(defun insert-current-date-time () (interactive)
       (insert (shell-command-to-string "echo -n $(date +%Y%m%d%H%M%S)")))

(defun insert-current-date () (interactive)
       (insert (shell-command-to-string "echo -n $(date +%Y%m%d)")))

(after! chatgpt-shell
  (setq chatgpt-shell-openai-key (getenv "OPENAI_API_KEY")))

;;
(defun copy-current-line-position-to-clipboard ()
  "Copy current line in file to clipboard as '</path/to/file>:<line-number>'"
  (interactive)
  (let ((path-with-line-number
         (concat (buffer-file-name) ":" (number-to-string (line-number-at-pos)))))
    (x-select-text path-with-line-number)
    (message (concat path-with-line-number " copied to clipboard"))))

(define-key global-map (kbd "M-l") 'copy-current-line-position-to-clipboard)

(setq lsp-java-configuration-runtimes '[(:name "JavaSE-17"
                                         :path "/Library/Java/JavaVirtualMachines/temurin-17.jdk/Contents/Home"
                                         :default t)])

;; Disable the LSP formatter universally
;;(setq +format-with-lsp nil)

;;(add-hook 'java-mode-hook #'format-all-mode)

;; Ensure clay is loaded
(after! clojure-mode
  (require 'clay))

;; Keybindings for Clay functions
(map! :map clojure-mode-map
      :localleader
      (:prefix ("l" "clay")
        :desc "Start Clay" "s" #'clay-start
        :desc "Make HTML from namespace" "h" #'clay-make-ns-html
        :desc "Make Quarto HTML from namespace" "q" #'clay-make-ns-quarto-html
        :desc "Make Reveal.js slideshow" "r" #'clay-make-ns-quarto-revealjs
        :desc "Make last sexp" "l" #'clay-make-last-sexp
        :desc "Make defun at point" "f" #'clay-make-defun-at-point))

;; [Developing Metabase with Emacs](https://www.metabase.com/docs/latest/developers-guide/emacs)
(setq custom-file (concat user-emacs-directory ".custom.el")) ; tell Customize to save customizations to ~/.emacs.d/.custom.el
(ignore-errors                                                ; load customizations from ~/.emacs.d/.custom.el
  (load-file custom-file))

;; eww
;; ignore popup for eww
(set-popup-rule! "^\\*eww" :ignore t) ; Prevent EWW from being treated as a popup

;; sqlformat on save
(use-package sqlformat
  :commands (sqlformat sqlformat-buffer sqlformat-region)
  :hook (sql-mode sqlformat-on-save-mode)
  :init
  (setq sqlformat-command 'pgformatter
        sqlformat-args '("-s2" "-g" "-u1")))

;; duckdb

;;;###autoload
(defun sql-duckdb (&optional buffer)
  "Run duckdb as an inferior process.


If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-duckdb-program'.  Login uses
the variables `sql-user', `sql-password', `sql-database', and
`sql-server' as defaults, if set.  Additional command line parameters
can be stored in the list `sql-duckdb-options'.

The buffer is put in SQL interactive mode, giving commands for sending
input.  See `sql-interactive-mode'.

To set the buffer name directly, use \\[universal-argument]
before \\[sql-duckdb].  Once session has started,
\\[sql-rename-buffer] can be called separately to rename the
buffer.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-duckdb].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)"
  (interactive "P")
  (sql-product-interactive 'duckdb buffer))
(defun sql-duckdb-completion-object (sqlbuf _schema)
  (sql-redirect-value sqlbuf "show tables" "^\\(\\sw\\(?:\\sw\\|\\s_\\)*\\)," 1))

(defcustom sql-duckdb-program (or (executable-find "duckdb")
                                  "duckdb")
  "Command to start duckdub.

Starts `sql-interactive-mode' after doing some setup."
  :type 'file
  :group 'SQL)

(defcustom sql-duckdb-options nil
  "List of additional options for `sql-duckdb-program'."
  :type '(repeat string)
  :version "20.8" ;; FIXME: What is this?
  :group 'SQL)

(defcustom sql-duckdb-login-params '((database :file nil
                                               :must-match confirm))
  "List of login parameters needed to connect to duckdb."
  :type 'sql-login-params
  :version "26.1"
  :group 'SQL)

(with-eval-after-load 'sql
  (add-to-list 'sql-product-alist
               '(duckdb
                 :name "duckdb"
                 :free-software nil
                 :font-lock sql-mode-sqlite-font-lock-keywords  ;; Use sqlite for now
                 :sqli-program sql-duckdb-program
                 :sqli-options sql-duckdb-options
                 :sqli-login sql-duckdb-login-params
                 :sqli-comint-func sql-comint-sqlite
                 :list-all "show tables"
                 :list-table "describe %s"
                 :completion-object sql-duckdb-completion-object
                 :prompt-regexp "^D "
                 :prompt-length 2
                 :prompt-cont-regexp "^> ")))

(require 'ob-sql)

(defvar org-babel-default-header-args:duckdb
  '((:results . "output") (:exports . "both"))
  "Default arguments for evaluating a DuckDB code block.")

(defun org-babel-execute:duckdb (body params)
  "Execute a block of DuckDB SQL code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((processed-params (org-babel-process-params params))
         (db (or (cdr (assq :db processed-params))
                 (error "No database specified")))
         (sqlcmd (or (executable-find sql-duckdb-program)
                     (error "DuckDB executable not found")))
         (query body))
    (with-temp-buffer
      (call-process sqlcmd nil t nil db "-batch" "-csv" query)
      ;; Capture and return the buffer's contents.
      (buffer-string))))

(add-to-list 'org-babel-tangle-lang-exts '("duckdb" . "sql"))

(with-eval-after-load 'sql
  (add-to-list 'org-babel-load-languages '(duckdb . t)))

;; Add duckdb to the list of org-babel languages with SQL syntax highlighting
(add-to-list 'org-src-lang-modes '("duckdb" . sql))

(defun my-dabbrev-syntax-fix ()
  "Modify syntax table for dabbrev to treat / as a separator."
  (modify-syntax-entry ?/ " "))
(add-hook 'org-mode-hook 'my-dabbrev-syntax-fix) ; Apply to text-mode

;;
(after! typescript-mode
  (setq typescript-indent-level 2))

(after! web-mode
  (setq web-mode-markup-indent-offset 2)  ;; For JSX/TSX HTML-like syntax
  (setq web-mode-css-indent-offset 2)     ;; For CSS inside JSX/TSX
  (setq web-mode-code-indent-offset 2))  ;; For JS/TS code inside JSX/TSX
