(setq custom-file "~/.emacs.custom.el")
(package-initialize)
(add-to-list 'load-path "~/.emacs.local/")

(load "~/.emacs.rc/rc.el")

(load "~/.emacs.rc/misc-rc.el")
(load "~/.emacs.rc/org-mode-rc.el")

;;; Appearance
(defun rc/get-default-font ()
  (cond
   ((eq system-type 'windows-nt) "Iosevka-20")
   ((eq system-type 'gnu/linux) "Iosevka-20")))

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

(tool-bar-mode 0)
(menu-bar-mode 0)
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
;(use-package treesit
; :ensure nil
; :init
; (setq treesit-language-source-alist
;   '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
;     (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
;     (heex "https://github.com/phoenixframework/tree-sitter-heex")
;     (elixir     . ("https://github.com/elixir-lang/tree-sitter-elixir" "main" "src" nil nil))
;     )
; ))


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

;;; Elixir section
;(use-package elixir-ts-mode
;  :straight (:type built-in)
;  :mode (("\\.ex\\'" . elixir-ts-mode)
;         ("\\.exs\\'" . elixir-ts-mode)
;         ("\\mix.lock\\'" . elixir-ts-mode)))

;(use-package elixir-ts-mode
;  :straight (:type built-in)
; :init
; (setq elixir-ts-indent-offset nil) ; disable indentation
; mode (("\\.ex\\'" . elixir-ts-mode)
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

;; GO
(use-package go-ts-mode
  :mode "\\.go\\'"
  :bind
  (:map go-ts-mode-map
    ("C-c i a" . treesit-beginning-of-defun)
    ("C-c i e" . treesit-end-of-defun)
    ("C-c i t" . go-ts-mode-test-function-at-point)
    ("C-c i f" . go-ts-mode-test-this-file)
    ("C-c i p" . go-ts-mode-test-this-package)
    ("RET"     . reindent-then-newline-and-indent)
    ("M-RET"   . newline)
   )
  :custom
  (go-ts-mode-indent-offset 4)
)


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

;; undo-tree
;; enable undo-tree first
(rc/require 'undo-tree)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in config for developers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'treesit)

(setq treesit-font-lock-level 6)

(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (elisp-mode . elisp-ts-mode)
        (elixir-mode . elixir-ts-mode)
        (hcl-mode . hcl-ts-mode)
        (heex-mode . heex-ts-mode)
        (json-mode . json-ts-mode)
        (makefile-mode . make-ts-mode)
        ;;(python-mode . python-ts-mode)
        (toml-mode . toml-ts-mode)
        ;;(rust-mode . rust-ts-mode)
	(lua-mode . lua-ts-mode)
	;; EMACS
	(emacs-lisp-mode . emacs-lisp-ts-mode)
	;; SHELL
	;(sh-mode . bash-ts-mode)
	;(bash-mode . bash-ts-mode)
	;; WEB
	(js2-mode . js-ts-mode)
        (javascript-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (tsx-mode . tsx-ts-mode)
        (html-mode . html-ts-mode)
        (css-mode . css-ts-mode)
	;; BIN
	(c-mode . c-ts-mode)
	(zig-mode . zig-ts-mode)
	(go-mode . go-ts-mode)
	;; CONFIG
	(dockerfile-mode . dockerfile-ts-mode)
	;; DATA
        (yaml-mode . yaml-ts-mode)
	;; Functional
	(ocaml-mode . ocaml-ts-mode)
	(erlang-mode . erlang-ts-mode)))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (elixir "https://github.com/elixir-lang/tree-sitter-elixir.git")
        (hcl "https://github.com/MichaHoffmann/tree-sitter-hcl")
        (heex "https://github.com/phoenixframework/tree-sitter-heex.git")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (liquid "https://github.com/Shopify/tree-sitter-liquid")
        (make "https://github.com/alemuller/tree-sitter-make")
        ;;(python "https://github.com/tree-sitter/tree-sitter-python")
        ;;(rust "https://github.com/tree-sitter/tree-sitter-rust")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
	;; EMACS
	(elisp "https://github.com/Wilfred/tree-sitter-elisp" "main" "src")
	(lua  "https://github.com/tree-sitter-grammars/tree-sitter-lua" "main" "src")
	;; WEB
	(javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
        (html . ("https://github.com/tree-sitter/tree-sitter-html" "master" "src"))
        (css . ("https://github.com/tree-sitter/tree-sitter-css" "master" "src"))
	;; DOC
	(luadoc "https://github.com/tree-sitter-grammars/tree-sitter-luadoc" "master" "src")
	;; PATTERN
	(luap "https://github.com/tree-sitter-grammars/tree-sitter-luap" "master" "src")
	;; BIN
	(c . ("https://github.com/tree-sitter/tree-sitter-c" "master" "src"))
	(zig . ("https://github.com/maxxnino/tree-sitter-zig" "main" "src"))
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(gomod "https://github.com/camdencheek/tree-sitter-go-mod")
	;; DATA
	(json . ("https://github.com/tree-sitter/tree-sitter-json" "master" "src"))
	;; CONFIG
	(dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile" "main" "src"))
	;; Functional
	(ocaml "https://github.com/tree-sitter/tree-sitter-ocaml" "master" "grammars/ocaml/src/")
        (ocaml-interface "https://github.com/tree-sitter/tree-sitter-ocaml" "master" "grammars/interface/src/")
	(erlang "https://github.com/WhatsApp/tree-sitter-erlang")))

;; for install all treesitter
(defun tree-sitter-setup ()
  (interactive)
  (dolist (source treesit-language-source-alist)
    (treesit-install-language-grammar (car source))))

;;; Elixir lang
(use-package elixir-ts-mode
  :straight (:host github :repo "wkirschbaum/elixir-ts-mode")
  :mode (("\\.ex\\'" . elixir-ts-mode)
         ("\\.exs\\'" . elixir-ts-mode)
         ("\\mix.lock\\'" . elixir-ts-mode))
  :config
  (setq elixir-ts-indent-offset 4)
  )

(use-package heex-ts-mode
  :straight (:host github :repo "wkirschbaum/heex-ts-mode")
  :mode "\\.heex\\'")

;;; Erlang
(use-package erlang-ts
 :ensure t
 :defer t
 :mode ("\\.erl\\'" . erlang-ts-mode))

;;; Ocaml;
(use-package ocaml-ts-mode
   :ensure t
 :defer t
  :mode (("\\.ml\\'" . ocaml-ts-mode)))

;;; Lua
(use-package lua-ts-mode
   :ensure t
 :defer t
 :mode (("\\.lua\\'" . lua-ts-mode)))

;;; Rust
;;(use-package rust-ts-mode
;;  :ensure t
;; :defer t
;; :mode (("\\.rs\\'" . rust-ts-mode)))

;;; Magit
;; magit requres this lib, but it is not installed automatically on
;; Windows.
(rc/require 'cl-lib)
(rc/require 'magit)

(setq magit-auto-revert-mode nil)

(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "C-c m l") 'magit-log)

(use-package hl-todo
  :straight t
  :custom
  (hl-todo-keyword-faces
   '(("HOLD" . "#d0bf8f") ("TODO" . "#7209b7") ("NEXT" . "#dca3a3")
     ("THEM" . "#dc8cc3") ("PROG" . "#7cb8bb") ("OKAY" . "#7cb8bb")
     ("DONT" . "#5f7f5f") ("FAIL" . "#8c5353") ("DONE" . "#afd8af")
     ("NOTE" . "#d0bf8f") ("MAYBE" . "#d0bf8f") ("KLUDGE" . "#d0bf8f")
     ("HACK" . "#d0bf8f") ("TEMP" . "#d0bf8f") ("FIXME" . "#cc9393")
     ("XXXX*" . "#cc9393")

     ("CRITICAL" . "#ff0000") ("IN-PROGRESS" . "#4361ee") ("BLOCKED" . "#4f000b")
     ("WONT-DO" . "#dee2e6")))
  :config
  (global-hl-todo-mode))

;; Native editing supports
;; suggest to use /smartparens/ or /paredit/ (enabled in =init-basics.el=)
;; + ~forward-sexp~, =C-M-f=:
;;     move forward over a balanced expression that can be a pair or a symbol
;; + ~backward-sexp~, =C-M-b=:
;;   move backward
;; + ~kill-sexp~, =C-M-k=:
;;   kill balaced expression forward that can be a pair or a symbol
;; + ~mark-sexp~, =C-M-<SPC>= or =C-M-@=:
;;   put mark after following expression that can be a pair or a symbol
;; + ~beginning-of-defun~, =C-M-a=:
;;   move point to beginning of a function
;; + ~end-of-defun~, =C-M-e=:
;;   move point to end of a function
;; + ~mark-defun~, =C-M-h=:
;;   put a region around whole current or following function

;; Tips
;; M-% → replace inside region (with M-x query-replace)

;; Jump to definition, used as a fallback of lsp-find-definition
(use-package dumb-jump
  :ensure t
  :bind (("M-g j" . dumb-jump-go)
         ("M-g J" . dumb-jump-go-other-window))
  :custom
  (dumb-jump-quiet t)
  (dumb-jump-aggressive t)
  (dumb-jump-selector 'completing-read))


;; Browse devdocs.io
;; How to install use M-x devdocs-install
;; Ex. M-x devdocs-install elixir
(use-package devdocs
  :ensure t
  :bind ("C-c b" . devdocs-lookup)
  :config
  (add-to-list 'completion-category-overrides '(devdocs (styles . (flex)))))

;;; helm
;;(rc/require 'helm 'helm-git-grep 'helm-ls-git)
;; helm-git-grep error
(rc/require 'helm 'helm-ls-git)

(setq helm-ff-transformer-show-only-basename nil)

(global-set-key (kbd "C-c h t") 'helm-cmd-t)
(global-set-key (kbd "C-c h g g") 'helm-git-grep)
(global-set-key (kbd "C-c h g l") 'helm-ls-git-ls)
(global-set-key (kbd "C-c h f") 'helm-find)
(global-set-key (kbd "C-c h a") 'helm-org-agenda-files-headings)
(global-set-key (kbd "C-c h r") 'helm-recentf)

;;; Simpc mode
(require 'simpc-mode)
(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))
;;(add-to-list 'auto-mode-alist '("\\.[b]\\'" . simpc-mode))

;;; Whitespace mode
(defun rc/set-up-whitespace-handling ()
  (interactive)
  (whitespace-mode 1)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

;;; Set whitespace
(add-hook 'tuareg-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'c++-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'c-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'simpc-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'emacs-lisp-mode 'rc/set-up-whitespace-handling)
(add-hook 'java-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'lua-mode-hook 'rc/set-up-whitespace-handling)
;;(add-hook 'rust-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'scala-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'markdown-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'haskell-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'python-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'erlang-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'asm-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'fasm-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'go-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'nim-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'yaml-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'porth-mode-hook 'rc/set-up-whitespace-handling)
;; Elixir hook
(add-hook 'elixir-ts-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'heex-ts-mode-hook 'rc/set-up-whitespace-handling)
;; Rust hook
;;(add-hook 'rust-ts-mode-hook 'rc/set-up-whitespace-handling)
;;Rust mode hook
(add-hook 'rust-mode-hook 'rc/set-up-whitespace-handling)

;; Eglot
;(use-package eglot
;  ;; no :ensure t here because it's built-in
;
;  :custom
;  (eglot-send-changes-idle-time 0.1)
;  (eglot-extend-to-xref t)
;
;
;  :config
;  (fset #'jsonrpc--log-event #'ignore)
;
;  ;; Add the path to where nextls is located
;  (add-to-list 'exec-path "~/dev/language_servers/elixir/next-ls/bin/")
;
;  ;; NextLS
;  ;; # Installation
;  ;; gh release download v0.19.2 \
;  ;; --pattern next_ls_linux_amd64 \
;  ;; --output ~/.local/bin/nextls \
;  ;; --clobber \
;  ;; --repo elixir-tools/next-ls
;  ;; chmod +x ~/.local/bin/nextls
;
;  ;;https://www.elixir-tools.dev/docs/next-ls/editors/#emacs-with-eglot
;
;  ;; (add-to-list 'eglot-server-programs
;  ;;              `((elixir-ts-mode heex-ts-mode elixir-mode) .
;  ;;                ("nextls" "--stdio=true" :initializationOptions (:experimental (:completions (:enable t)))))
;  ;;              )
;
;
;  ;; Tell eglot to use nextls for Elixir
;  (add-to-list 'eglot-server-programs
;               '((elixir-ts-mode heex-ts-mode elixir-mode)
;                 . ("nextls" "--stdio=true")))
;
;
;
;  ;; Auto-enable eglot for Elixir
;  (add-hook 'elixir-mode-hook #'eglot-ensure)
;  (add-hook 'elixir-ts-mode-hook #'eglot-ensure)
;  (add-hook 'heex-ts-mode-hook #'eglot-ensure)
;  )


(defalias 'gf3/eglot
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'eglot-code-actions)
    (define-key map (kbd "f") #'eglot-format)
    (define-key map (kbd "r") #'eglot-rename)
    map)
  "Eglot commands.")

;; how to use
; C-c c a run code actions
; C-c c f format
; C-c c r rename
(global-set-key (kbd "C-c c") 'gf3/eglot)

;; Buffer Mngt
(defun switch-to-last-buffer ()
  "Switch to the previously open buffer."
  (interactive)
  (switch-to-buffer nil))

(defun new-buffer ()
  "Create a new empty buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "Untitled")))
	  (set-buffer-major-mode buffer)
	  (switch-to-buffer buffer)))

(defun gf3/buffer ()
  "Find buffers, maybe scoped to a project."
  (interactive)
  (if (project-current)
      (consult-project-buffer)
	  (consult-buffer)))

(defun gf3/kill-buffer-filename (&optional prefix)
  "Kill the current buffer's filename to the kill ring. If PREFIX is set the filename is killed to the system clipboard."
  (interactive "p")
  (let ((filename
         (if (project-current)
             (file-relative-name buffer-file-name (project-root (project-current)))
           buffer-file-name)))
    (if (eq 4 prefix)
        (gui-set-selection 'CLIPBOARD filename)
      (kill-new filename))
    (message (format "%s %s" (if (eq 4 prefix) "Copied" "Killed") filename))))


(defalias 'gf3/buffers
  (let ((map (make-sparse-keymap)))
	  (define-key map (kbd "a") '("Last buffer" . switch-to-last-buffer))
	  (define-key map (kbd "b") '("Buffers". gf3/buffer))
	  (define-key map (kbd "i") '("Symbols" . consult-imenu))
	  (define-key map (kbd "f") '("Files" . gf3/find))
	  (define-key map (kbd "F") '("Browse files" . find-file))
	  (define-key map (kbd "g") '("Grep" . deadgrep))
	  (define-key map (kbd "k") '("Kill filename" . gf3/kill-buffer-filename))
	  (define-key map (kbd "q") '("Close buffer" . kill-current-buffer))
	  (define-key map (kbd "l") '("Lines" . consult-line))
	  (define-key map (kbd "n") '("New buffer" . new-buffer))
	  (define-key map (kbd "r") '("Recent file" . consult-recent-file))
	  map)
  "Buffer management.")

(global-set-key (kbd "C-c b") '("Buffer management". gf3/buffers))

;; Global key binding
;; ==== Dvorak niceity ====
;;(define-key key-translation-map "\C-t" "\C-x")

;; ==== transpose buffers ====
(defun transpose-buffers (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(global-set-key (kbd "C-x 4 t") 'transpose-buffers)


;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Start a regular shell if you prefer that.
(global-set-key (kbd "C-x C-m") 'shell)


;; ==== M-n, M-p ====
(defun scroll-down-keep-cursor ()
  ;; Scroll the text one line down while keeping the cursor
  (interactive)
  (scroll-down 1))

(defun scroll-up-keep-cursor ()
  ;; Scroll the text one line up while keeping the cursor
  (interactive)
  (scroll-up 1))

(global-set-key (kbd "M-N") 'scroll-down-keep-cursor)
(global-set-key (kbd "M-P") 'scroll-up-keep-cursor)


;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)


;; treesitter
;;; Packages that don't require configuration
(rc/require
  'rust-mode
  'toml-mode
  )

;; load custom file from ~/emacs.custom.el
(load-file custom-file)
