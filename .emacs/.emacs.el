;; -*- coding: utf-8; lexical-binding: t -*-

;(setq debug-on-error t)


;; MAKE SURE YOU RUN THESE:
;; pip install proselint
;; pip install pyright
;; pip install debugpy
;; pip install "python-lsp-server[all]"
;; 



; ------------------------------------- EMACS STUFF ---------------------------------------------

; Tramp speedup?
(setq tramp-completion-reread-directory-timeout t)

(setq remote-file-name-inhibit-cache nil)
(setq tramp-verbose 1)


; Assume bash cause I use zsh
(setq tramp-login-shell "bash")
(setq tramp-remote-shell "/bin/bash")


; Makes several backups of the init file
(setq
backup-by-copying t ; don't clobber symlinks
backup-directory-alist
'(("." . "~/.saves")) ; don't litter my fs tree
delete-old-versions t
kept-new-versions 6
kept-old-versions 2
version-control t)

;; set font Hack 15 pt
(set-face-attribute 'default nil
                    :family "Iosevka"
                    :height 140)

; Magit is broken on earlier emacs versions, we need to include this line
(defun seq-keep (function sequence)
  "Apply FUNCTION to SEQUENCE and return the list of all the non-nil results."
  (delq nil (seq-map function sequence)))

; Don't check package signatures... for some reason? I forgot why I did this
(setq package-check-signature nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(conda-anaconda-home "~/miniconda3")
 '(custom-enabled-themes '(deeper-blue))
 '(custom-safe-themes
   '("e6fb17048752ae4f07e8a689f59fb909a7e3008c5db75af3d870b701ce6506ef" "b9628f9ad175d774bfc02f140e7aa058fc0a2f3aa10134125d643eae88b3f36c" "4c877a679cb7c0c17de7267c6440f60403b104a02659634656e9c2722a405f6a" "7f2c832000d6f007e8dc49bd30b5fbb7a4cf2e44b35ce8f6f65be57c59628421" "6e13ff2c27cf87f095db987bf30beca8697814b90cd837ef4edca18bdd381901" "dccf4a8f1aaf5f24d2ab63af1aa75fd9d535c83377f8e26380162e888be0c6a9" "2902694c7ef5d2a757146f0a7ce67976c8d896ea0a61bd21d3259378add434c4" "039112154ee5166278a7b65790c665fe17fd21c84356b7ad4b90c29ffe0ad606" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" default))
 '(inhibit-startup-screen t)
 '(package-selected-packages '(lsp-treemacs helm-lsp hydra avy which-key helm-xref))
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Add MELPA archive to packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(package-initialize)

; Use-package for easy package management
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(defalias 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "C-M-<tab>") 'indent-region)
(global-set-key (kbd "C-z") 'undo)

;--------------------------------- Random packages ----------------------------------

(use-package benchmark-init
    :ensure
    :config
    ;; To disable collection of benchmark data after init is done.
    (add-hook 'after-init-hook 'benchmark-init/deactivate))

(add-hook 'after-init-hook
          (lambda () (message "loaded in %s" (emacs-init-time))))

(use-package auctex
  :ensure)

(use-package goto-last-change
  :bind (("C-;" . goto-last-change)))

; lsp mode looks for python in bad places, use the shell paths instead
(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))

(use-package emojify :ensure)

; Better parens highlighting
(use-package rainbow-delimiters
  :ensure
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package neotree
  :ensure)

(use-package projectile
  :ensure)
;; project navigation tools
(projectile-mode +1)
;; Recommended keymap prefix on macOS
;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;; Recommended keymap prefix on Windows/Linux
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq-default neo-show-hidden-files t)

(setq treemacs--width-is-locked nil)
(setq treemacs-width-is-initially-locked nil)
(setq treemacs-width 14) ; Make the side bar a bit smaller

(use-package flycheck :ensure)


; Checks that your english looks good. Run pip install proselint
(flycheck-define-checker proselint
  "A linter for prose."
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
            (id (one-or-more (not (any " "))))
            (message (one-or-more not-newline)
                     (zero-or-more "\n" (any " ") (one-or-more not-newline)))
            line-end))
  :modes (text-mode markdown-mode gfm-mode org-mode))

(use-package writegood-mode
    :bind ("C-c g" . writegood-mode)
    :config
    (add-to-list 'writegood-weasel-words "actionable"))

(use-package notmuch :ensure) ; Mail client

; For some reason default emacs doesn't support markdown
(use-package markdown-mode
  :ensure
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package magit :ensure)

(use-package git-gutter
  :ensure
  :config
  (global-git-gutter-mode 't))

;; This kinda sucks
;; (use-package smartparens
;;   :config
;;   (add-hook 'prog-mode-hook 'smartparens-mode))

;; Alternatively:
(electric-pair-mode 1)
;; make electric-pair-mode work on more brackets
(setq electric-pair-pairs '(
                            (?\" . ?\")
			    (?\' . ?\')
			    (?\( . ?\))
                            (?\{ . ?\})
			    (?\[ . ?\])))

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(yas-global-mode 1)

(use-package json-mode
  :ensure t)

;--------------------------------- LANGUAGE SERVERS ----------------------------------

; Let GC use 200MB
(setq gc-cons-threshold 200000000
      read-process-output-max 20000000 ; Read 20mb from the process
      treemacs-space-between-root-nodes nil ; Don't put spaces in the f8 menu (I think)
      company-idle-delay 0.2
      company-minimum-prefix-length 1
      lsp-idle-delay 0.5)  ;; let lsp sit idle for half a second

; Allegedly this is faster than using hash tables
(setenv "LSP_USE_PLISTS" "true")

(setq lsp-enable-snippet t)

; For tramp
(require 'tramp)
(setq lsp-clients-clangd-executable "/bin/clangd")  ;; Adjust if clangd is in a different location
(setq tramp-verbose 0)
(setq tramp-default-method "ssh")
(setq tramp-login-shell "bash")
(setq tramp-remote-shell "/bin/bash")

(use-package lsp-mode
  :init
  (setq lsp-log-io nil)
  :hook ((c++-mode python-mode c-mode) . lsp-deferred)
  :commands lsp
  ;; extra rust commands
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  ;; enable / disable the hints as you prefer:
  (lsp-inlay-hint-enable t)
  ;; These are optional configurations. See https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/#lsp-rust-analyzer-display-chaining-hints for a full list
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints t) ; This can get messy sometims
  (lsp-rust-analyzer-display-reborrow-hints t)
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection 
                    (lambda ()
                      (let ((home (getenv "HOME")))
                        (cond
                         ;; Try local installation first
                         ((file-exists-p (expand-file-name "~/local/bin/clangd"))
                          (expand-file-name "~/local/bin/clangd"))
                         ((file-exists-p "/usr/bin/clangd") "/usr/bin/clangd")
                         ((file-exists-p "/usr/local/bin/clangd") "/usr/local/bin/clangd")
                         (t "clangd")))))
    :major-modes '(c-mode c++-mode)
    :remote? t
    :priority -1
:server-id 'clangd-remote)))

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook 'lsp-ui-mode #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))


(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  ;; Could move these into config but stole them and don't wanna mess with em
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'default))
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-delay 0.5))

;; use company mode in every single buffer (So i don't have to keep writing it in the shell)
(add-hook 'after-init-hook 'global-company-mode)

(use-package company
  :ensure
  :custom
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  (:map company-mode-map
	("<tab>". tab-indent-or-complete) ; Hit tab to autocomplete, escape to cancel
	("TAB". tab-indent-or-complete)))

;; Tell lsp to stay out of company mode
(setq lsp-completion-provider :none)
(use-package yasnippet-snippets :ensure)

;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

;; (defun company-mode/backend-with-yas (backend)
;;   (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
;;       backend
;;     (append (if (consp backend) backend (list backend))
;;             '(:with company-yasnippet))))

;; (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

;; (defun company-yasnippet-or-completion ()
;;   (interactive)
;;   (or (do-yas-expand)
;;       (company-complete-common)))

;; (defun check-expansion ()
;;   (save-excursion
;;     (if (looking-at "\\_>") t
;;       (backward-char 1)
;;       (if (looking-at "\\.") t
;;         (backward-char 1)
;;         (if (looking-at "::") t nil)))))

;; (defun do-yas-expand ()
;;   (let ((yas/fallback-behavior 'return-nil))
;;     (yas/expand)))

;; (defun tab-indent-or-complete ()
;;   (interactive)
;;   (if (minibufferp)
;;       (minibuffer-complete)
;;     (if (or (not yas/minor-mode)
;;             (null (do-yas-expand)))
;;         (if (check-expansion)
;;             (company-complete-common)
;;           (indent-for-tab-command)))))

;------------------------------------- DEBUGGERS -------------------------------

;; Disable TRAMP temporarily if it's causing interference
(setq tramp-mode nil)

;; Enable DAP and set local environment settings
(setq dap-auto-configure-mode t)
(setq dap-auto-configure-features '(sessions locals breakpoints expressions controls tooltip))
(setq dap-python-executable (executable-find "python"))

;; Enable logging for debugging
(setq dap-print-io t)
(setq dap-log-protocol-enable t)
(setq dap-debug-buffer-mode t)

;; DAP Mode Configuration
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  :bind
  (:map global-map
        ("C-c d" . dap-debug)))


;; --------------------------------------- PYTHON SETUP ------------------------------

(use-package conda)

(conda-env-initialize-interactive-shells)
(conda-env-initialize-eshell)



(setq conda-anaconda-home "/home/yelnat/miniconda3")
;; Set default environment
(setq conda-env-home-directory (expand-file-name "~/miniconda3/envs"))


(defun my/set-pyright-env ()
  "Set pyright settings based on current conda environment"
  (when (getenv "CONDA_DEFAULT_ENV")
    (let ((env-name (getenv "CONDA_DEFAULT_ENV")))
      (setq-local lsp-pyright-venv-path "/home/yelnat/miniconda3/envs")
      (setq-local lsp-pyright-venv-directory env-name)
      (setq-local lsp-pyright-python-executable-cmd 
                  (concat "/home/yelnat/miniconda3/envs/" env-name "/bin/python")))))

(add-hook 'python-mode-hook #'my/set-pyright-env)


(use-package lsp-pyright
  :hook (python-mode . (lambda () (require 'lsp-pyright) (require 'conda)))
  :init (when (executable-find "python")
          (setq lsp-pyright-python-executable-cmd "python")
	  (setq lsp-pyright-multi-root nil)
	  (setq lsp-pyright-auto-search-paths nil)))



;; if you installed debugpy, you need to set this
;; https://github.com/emacs-lsp/dap-mode/issues/306
;; Gotta run pip install debugpy and maybe pip install pip install "python-lsp-server[all]"

(dap-register-debug-template "Python template"
  (list :type "python"
        :args ""
        :cwd "/home/yelnat/Documents/programmin/python-test/"
	:program "~/Documents/programmin/python-test/test.py"
       ; :target-module (expand-file-name "~/Documents/programmin/python-test/test.py")
        :request "launch"
        :name "My App"))


;; Broken on debian stable
;; (use-package py-autopep8
;;   :ensure t  ;; This tells use-package to install if not present
;;   :hook (python-mode . py-autopep8-mode))


(defun autopep8-buffer ()
  "Automatically formats Python code to conform to the PEP 8 style guide."
  (interactive)
  (when (eq major-mode 'python-mode)
    (shell-command-to-string 
     (format "autopep8 --in-place --aggressive --aggressive %s" 
             (shell-quote-argument (buffer-file-name))))
    (revert-buffer t t t)))

(add-hook 'after-save-hook #'autopep8-buffer)

;; ;; --------------------------------------- DOCKER SETUP ------------------------------

(use-package docker
  :ensure t
  :bind ("C-c C-c d" . docker))
(use-package dockerfile-mode)
(require 'dockerfile-mode)


;; ;; -------------------------- RUST SETUP -----------------------

(use-package toml-mode :ensure)
(dap-register-debug-template "Rust::GDB Run Configuration"
                             (list :type "gdb"
                                   :request "launch"
                                   :name "GDB::Run"
				   :gdbpath "~/.cargo/bin/rust-gdb"
                                   :target nil
                                   :program "${workspaceFolder}/target/debug/hello / replace with binary"
                                   :cwd "${workspaceFolder}"))



(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)
  )

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;; ;; ------------------------- C/C++ SETUP ----------------------

(use-package cmake-mode)

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;----------------------------- HELM ----------------------

(setq package-selected-packages '(lsp-treemacs helm-lsp
     hydra avy which-key helm-xref))

;; sample `helm' configuration use https://github.com/emacs-helm/helm/ for details
(helm-mode)
(require 'helm-xref)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)

(which-key-mode) ; auto complete some keybinds
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)



; --------------------------------- STUFF TO PUT IN FILES IN PLACES! ------------------------------------------------



;; pyrightconfig.json: 
;; {
;;   "venv": "gentenv",
;;   "venvPath": "/home/yelnat/miniconda3/envs",
;;   "pythonVersion": "3.10",
;;   "pythonPath": "/home/yelnat/miniconda3/envs/gentenv/bin/python",
;;   "typeCheckingMode": "basic"
;; }


;; Launch.json for python files
;; {
;;     "version": "0.2.0",
;;     "configurations": [
;;         {
;;             "name": "Python: Current File",
;;             "type": "python",
;;             "request": "launch",
;;             "program": "${file}",
;;             "console": "integratedTerminal",
;;             "justMyCode": true,
;;             "env": {
;;                 "PYTHONPATH": "${workspaceFolder}"
;;             }
;;         },
;;         {
;;             "name": "Python: Specific File",
;;             "type": "python",
;;             "request": "launch",
;;             "program": "${workspaceFolder}/path/to/your/file.py",
;;             "console": "integratedTerminal",
;;             "justMyCode": true,
;;             "env": {
;;                 "PYTHONPATH": "${workspaceFolder}"
;;             }
;;         },
;;         {
;;             "name": "Python: Remote Attach",
;;             "type": "python",
;;             "request": "attach",
;;             "connect": {
;;                 "host": "localhost",
;;                 "port": 5678
;;             },
;;             "pathMappings": [
;;                 {
;;                     "localRoot": "${workspaceFolder}",
;;                     "remoteRoot": "."
;;                 }
;;             ],
;;             "justMyCode": true
;;         }
;;     ]
;; }
