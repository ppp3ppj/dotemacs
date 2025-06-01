(setq custom-file "~/.emacs.custom.el")
(package-initialize)
;;(add-to-list 'load-path "~/.emacs.local/")

(load "~/.emacs.rc/rc.el")

(load "~/.emacs.rc/misc-rc.el")

;;; Appearance
(defun rc/get-default-font ()
  (cond
   ((eq system-type 'windows-nt) "Consolas-13")
   ((eq system-type 'gnu/linux) "Fira Code-22")))

;;; display-line-numbers-mode
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

;;; backup
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))
(setq auto-save-default nil)  ;stop creating those #auto-save# files
(setq vc-make-backup-files t) ;Make backups of files, even when they're in version control
(global-auto-revert-mode)



;;; relative line number
;;(setq display-line-numbers 'relative)
;;(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(add-to-list 'default-frame-alist `(font . ,(rc/get-default-font)))

;;(tool-bar-mode 0)
;;(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)

;; theme
(use-package gruber-darker-theme
  :ensure t      ;; install it if not present
  :config
  (load-theme 'gruber-darker t))

;;(eval-after-load 'gruber-darker
;;  (set-face-attribute 'line-number nil :inherit 'default))
;;(rc/require-theme 'gruber-darker)

;; Tree sitter for better syntax highlight
(use-package treesit
  :if (treesit-available-p)
  :ensure nil
  :init
  (setq treesit-language-source-alist
    '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
      (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
      ;(heex "https://github.com/phoenixframework/tree-sitter-heex")
      ;(elixir     . ("https://github.com/elixir-lang/tree-sitter-elixir" "main" "src" nil nil))
      )
  ))

;; Elixir
;(use-package elixir-mode :ensure t)





;; Disable tree-sitter mode for Elixir buffers
;;(add-to-list 'treesit-language-source-alist
             ;;'(elixir . nil))  ;; this disables treesit for Elixir

;;(add-to-list 'major-mode-remap-alist '(elixir-mode . elixir-ts-mode))
;;(setq elixir-ts-indent-offset 4)  ;; for example, change indent to 4 spaces
;;(setq-default tab-width 4)
;;(setq-default indent-tabs-mode nil)

;;(delete-selection-mode 1)    ;; You can select text and delete it by typing.
;;(electric-indent-mode -1)    ;; Turn off the weird indenting that Emacs does by default.
;;(electric-pair-mode 1)       ;; Turns on automatic parens pairing

;(setq make-backup-files nil) ;; Stop creating ~ backup files
;(setq auto-save-default nil) ;; Stop creating # auto save files


  ;;(setq elixir-ts-mode-indent-offset 4) ; Set the indentation offset for Elixir

;;(use-package elixir-ts-mode
;;  :ensure t
;;  :config
;;  (setq elixir-ts-indent-offset 4))  ; Set the indentation offset to 4 spaces


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;(use-package elixir-ts-mode
;  :straight (:type built-in)
; :init
; (setq elixir-ts-indent-offset nil) ; disable indentation
; :mode (("\\.ex\\'" . elixir-ts-mode)
;        ("\\.exs\\'" . elixir-ts-mode)
;        ("\\mix.lock\\'" . elixir-ts-mode))
; :hook
; ((elixir-ts-mode . (lambda ()
;                      (electric-indent-local-mode -1) ; turn off auto-indent
;                      (setq-local indent-line-function #'ignore)
;                      )
;                  )
;  )

; )


;;; multiple cursors
(rc/require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

;;; Paredit
(rc/require 'paredit)

(defun rc/turn-on-paredit ()
  (interactive)
  (paredit-mode 1))

(add-hook 'emacs-lisp-mode-hook  'rc/turn-on-paredit)
(add-hook 'clojure-mode-hook     'rc/turn-on-paredit)
(add-hook 'lisp-mode-hook        'rc/turn-on-paredit)
(add-hook 'common-lisp-mode-hook 'rc/turn-on-paredit)
(add-hook 'scheme-mode-hook      'rc/turn-on-paredit)
(add-hook 'racket-mode-hook      'rc/turn-on-paredit)

;;; yasnippet
(rc/require 'yasnippet)

(require 'yasnippet)

(setq yas/triggers-in-field nil)
(setq yas-snippet-dirs '("~/.emacs.snippets/"))

(yas-global-mode 1)

;;; tramp
;;; http://stackoverflow.com/questions/13794433/how-to-disable-autosave-for-tramp-buffers-in-emacs
(setq tramp-auto-save-directory "/tmp")

;;; Company
(rc/require 'company)
(require 'company)

(global-company-mode)

(add-hook 'tuareg-mode-hook
          (lambda ()
            (interactive)
            (company-mode 0)))

;;; Move Text
(rc/require 'move-text)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

;;; dired
;; Load dired-x for extra dired features
(require 'dired-x)

;; Hide dotfiles when omit mode is enabled
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))

;; Smart target guessing for copying/moving between dired buffers
(setq-default dired-dwim-target t)

;; Use long listing with all files and human readable sizes
(setq dired-listing-switches "-alh")

;; Enable mouse drag and drop for files
(setq dired-mouse-drag-files t)

;;; ido + smex setup with plain require
;;; ido
(rc/require 'smex 'ido-completing-read+)

(require 'ido-completing-read+)

(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;;; word-wrap
(defun rc/enable-word-wrap ()
  (interactive)
  (toggle-word-wrap 1))
;;(add-hook 'markdown-mode-hook 'rc/enable-word-wrap)

;; Do not blink cursor
(blink-cursor-mode -1)

;; load custom file from ~/emacs.custom.el
(load-file custom-file)
