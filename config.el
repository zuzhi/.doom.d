;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "zuzhi"
      user-mail-address "zuzhi.hu@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; ------------------------------------------------------------------------------
;;
;; Org
;; Add a timestamp when a 'TODO' item closed.
(setq org-log-done 'time)

;; File watchers
;; Increase the file watch warning threshold, the default is 1000
(setq lsp-file-watch-threshold 2000)

;; Frame
;; Maximize Emacs frame on start-up
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; JDL Mode
;; Load the package
(load "jdl-mode")

;; PDF Tools
;; Prerequisite: https://github.com/politza/pdf-tools#compiling-on-os-x
;; Activate pdf-tools
(pdf-loader-install)
;; Uses more memory; see https://github.com/politza/pdf-tools/issues/51
(setq pdf-view-use-scaling t
      pdf-view-use-imagemagick nil)

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

;; SQL
;; https://dev.to/viglioni/emacs-as-sql-client-with-lsp-143l
;;;###autoload
;;(defmacro any-nil? (&rest args)
;;  `(not (and ,@args)))
;;
;;;;;###autoload
;;(defmacro throw-if (condition &optional error-description)
;;  "if condition is true, thrown an error"
;;  `(if ,condition (error (or ,error-description ""))))
;;
;;;; Variables related to sql configs
;;(setq lsp-sqls-connections nil)
;;(setq sql-connection-alist nil)
;;
;;;;;###autoload
;;(defun format-postgres-sqls (host port user password db)
;;  (format "host=%s port=%s user=%s password=%s dbname=%s"
;;          host port user password db))
;;
;;;;;###autoload
;;(defun format-mysql-sqls (host port user password db)
;;  (format "%s:%s@tcp(%s:%s)/%s" user password host port db))
;;
;;;;;###autoload
;;(defun format-postgres-uri (host port user password db)
;;  (format "postgresql://%s:%s@%s:%s/%s" user password host port db))
;;
;;
;;;;;###autoload
;;(defun add-to-sqls-connections (db-type data-src-name)
;;  (add-to-list 'lsp-sqls-connections
;;               (list (cons 'driver db-type)
;;                     (cons 'dataSourceName data-src-name))))
;;
;;;;;###autoload
;;(defmacro add-to-sql-conection-alist (db-type name host port user password db)
;;  `(add-to-list 'sql-connection-alist
;;                (list (quote ,name)
;;                     (list 'sql-product (quote ,db-type))
;;                     (list 'sql-user ,user)
;;                     (list 'sql-server ,host)
;;                     (list 'sql-port ,port)
;;                     (list 'sql-password ,password)
;;                     (list 'sql-database ,db))))
;;
;;;;;###autoload
;;(defmacro sql-add-postgres-db (name &rest db-info)
;;  "Adds a mysql database to emacs and lsp
;;   This macro expects a name to the database and a p-list of parameters
;;   :port, :user, :password, :database, :host
;;   The only optional is :port, its default value is 5432
;;   e.g.:
;;   (sql-add-postgres-db
;;        my-db-name ;; notice that there are no quotes here
;;        :port 1234
;;        :user \"username\"
;;        :host \"my-host\"
;;        :database \"my-db\"
;;        :password \"mypassword\")"
;;  `(let ((port (or ,(plist-get db-info :port) 5432))
;;         (user ,(plist-get db-info :user))
;;         (password ,(plist-get db-info :password))
;;         (host ,(plist-get db-info :host))
;;         (db ,(plist-get db-info :database)))
;;     (throw-if (any-nil? user password host db (quote ,name)) "there are info missing!")
;;     (let ((full-uri (format-postgres-uri host port user password db))
;;           (data-src-name (format-postgres-sqls host port user password db)))
;;       (add-to-sqls-connections "postgresql" data-src-name)
;;       (add-to-sql-conection-alist 'postgres ,name host port user password full-uri))))
;;
;;;;;###autoload
;;(defmacro sql-add-mysql-db (name &rest db-info)
;;  "Adds a mysql database to emacs and lsp
;;   This macro expects a name to the database and a p-list of parameters
;;   :port, :user, :password, :database, :host
;;   The only optional is :port, its default value is 3306
;;   e.g.:
;;   (sql-add-mysql-db
;;        my-db-name ;; notice that there are no quotes here
;;        :port 1234
;;        :user \"username\"
;;        :host \"my-host\"
;;        :database \"my-db\"
;;        :password \"mypassword\")"
;;  `(let ((port (or ,(plist-get db-info :port) 3306))
;;         (user ,(plist-get db-info :user))
;;         (password ,(plist-get db-info :password))
;;         (host ,(plist-get db-info :host))
;;         (db ,(plist-get db-info :database)))
;;     (throw-if (any-nil? user password host db (quote ,name)) "there are info missing!")
;;     (add-to-sqls-connections "mysql" (format-mysql-sqls host port user password db))
;;     (add-to-sql-conection-alist 'mysql ,name host port user password db)))

;; Add hook to sql file
(add-hook 'sql-mode-local-vars-hook #'lsp!)

;; Enable pyim-basedict
;; FIXME not working, still requires to run this command manually
;; and keep get `pyim 没有安装，pyim-basedict 启用失败。` when running `doom doctor`
(pyim-basedict-enable)
