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
(setq lsp-file-watch-threshold 2000)

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
      (:prefix ("l" . "clay")
        :desc "Start Clay" "s" #'clay-start
        :desc "Make HTML from namespace" "h" #'clay-make-ns-html
        :desc "Make Quarto HTML from namespace" "q" #'clay-make-ns-quarto-html
        :desc "Make Reveal.js slideshow" "r" #'clay-make-ns-quarto-revealjs
        :desc "Make last sexp" "l" #'clay-make-last-sexp
        :desc "Make defun at point" "f" #'clay-make-defun-at-point))

;; Function to check if the current namespace is within "notebooks"
(defun my/clojure-namespace-in-notebooks-p ()
  "Check if the current Clojure file is in a `notebooks` namespace."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^\\s-*(ns +\\(noj-book\\)" nil t)))

;; Auto render Clay HTML on save only for namespaces under "notebooks"
(defun my/clay-auto-render-on-save ()
  "Automatically render with Clay when in a `notebooks` namespace."
  (when (my/clojure-namespace-in-notebooks-p)
    (clay-make-ns-html)))

;; Add the after-save hook only in clojure-mode
(add-hook 'clojure-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'my/clay-auto-render-on-save nil t)))
