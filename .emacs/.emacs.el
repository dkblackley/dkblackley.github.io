(setq debug-on-error t)

; Makes several backups of the init file
(setq
backup-by-copying t ; don't clobber symlinks
backup-directory-alist
'(("." . "~/.saves")) ; don't litter my fs tree
delete-old-versions t
kept-new-versions 6
kept-old-versions 2
version-control t)

(set-face-attribute 'default nil :height 130)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(defun seq-keep (function sequence)
  "Apply FUNCTION to SEQUENCE and return the list of all the non-nil results."
  (delq nil (seq-map function sequence)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(base16-google-dark))
 '(custom-safe-themes
   '("2902694c7ef5d2a757146f0a7ce67976c8d896ea0a61bd21d3259378add434c4" "039112154ee5166278a7b65790c665fe17fd21c84356b7ad4b90c29ffe0ad606" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" default))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(spacemacs-theme yaml-mode dockerfile-mode magit base16-theme alect-themes exec-path-from-shell docker flycheck rustic dap-mode toml-mode projectile neotree company lsp-pyright lsp-ui lsp-mode solarized-theme)))
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

(when (not package-archive-contents)
    (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))

(use-package neotree
  :ensure)
(use-package projectile
  :ensure)

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq-default neo-show-hidden-files t)

(setq treemacs--width-is-locked nil)
(setq treemacs-width-is-initially-locked nil)
(setq treemacs-width 22)

(setq lsp-enable-snippet nil)

(use-package lsp-mode
  :init
  (setq lsp-log-io t)
  :hook ((c++-mode python-mode c-mode) . lsp-deferred)
  :commands lsp
  ;; extra rust commands
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.1)
  ;; enable / disable the hints as you prefer:
  (lsp-inlay-hint-enable t)
  ;; These are optional configurations. See https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/#lsp-rust-analyzer-display-chaining-hints for a full list
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "/usr/local/bin/clangd")
   :major-modes '(c++-mode)
   :remote? t
   :server-id 'clangd-remote)))

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
  (setq lsp-ui-sideline-delay 0.05))

;; use company mode in every single buffer (So i don't have to keep writing it in the shell)
(add-hook 'after-init-hook 'global-company-mode)

;; project navigation tools
(projectile-mode +1)
;; Recommended keymap prefix on macOS
;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;; Recommended keymap prefix on Windows/Linux
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package company
  :ensure
  :custom
  (company-idle-delay 0.2) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  ;; I like defaults, M-n, M-p :>
  ;; :bind
  ;; (:map company-active-map
  ;;          ("C-n". company-select-next)
  ;;	      ("C-p". company-select-previous)
  ;;	      ("M-<". company-select-first)
;;	      ("M->". company-select-last)))
  (:map company-mode-map
	("<tab>". tab-indent-or-complete)
	("TAB". tab-indent-or-complete)))

(use-package flycheck :ensure)

(use-package dap-mode
  :ensure
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  :bind
  (:map global-map
	("C-c d" . dap-debug)))

(use-package magit)

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

;; Tell lsp to stay out of company mode
(setq lsp-completion-provider :none)

(use-package yasnippet-snippets)

;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

;; --------------------------------------- DOCKER SETUP ------------------------------

(use-package docker
  :ensure t
  :bind ("C-c C-c d" . docker))
(use-package dockerfile-mode)
(require 'dockerfile-mode)

(require 'docker-tramp)

;; --------------------------------------- PYTHON SETUP ------------------------------

(require 'dap-cpptools)

(use-package lsp-pyright
  :hook (python-mode . (lambda () (require 'lsp-pyright)))
  :init (when (executable-find "python")
          (setq lsp-pyright-python-executable-cmd "python")
	  (setq lsp-pyright-multi-root nil)
	  (setq lsp-pyright-auto-search-paths nil)))

;; (setf (lsp-session-folders-blacklist (lsp-session)) nil)


;; if you installed debugpy, you need to set this
;; https://github.com/emacs-lsp/dap-mode/issues/306
;; Gotta run pip install debugpy and maybe pip install pip install "python-lsp-server[all]"

;; (setq python-shell-interpreter "/usr/local/bin/python3.10")
;; (setq python-shell-exec-path "/usr/local/bine/python3.10")

(dap-register-debug-template "Python template"
  (list :type "python"
        :args ""
        :cwd "/home/yelnat/Documents/programmin/python-test/"
       ; :target-module (expand-file-name "~/Documents/programmin/python-test/test.py")
        :request "launch"
        :name "My App"))

;; (dap-debug
;;  (list :type "python"
;;        :args ""
;;        :cwd nil
;;        :module nil
;;        :program "~/Documents/programmin/python-test/test.py"
;;        :request "launch"
;;        :environment-variables '(("FOO" . "BAR"))
;;        :name "Python :: Run Configuration please"
;;        :hostName "localhost"
;;        :host "localhost"))


;; (require 'dap-gdb-lldb)


;; -------------------------- RUST SETUP -----------------------

;; (setq dap-gdb-lldb-path "~/.emacs-extras/webfreak.debug-0.26.1.vsix")

;; (require 'dap-gdb-lldb)

;; ;; Still need to do dap-cpptools-setup, but I think those come from microsoft. could zip my emacs extension folder
;; Needs node, and you have to specify absolute path to target
(require 'dap-gdb-lldb)
(use-package toml-mode :ensure)
    (dap-register-debug-template "Rust::GDB Run Configuration"
                             (list :type "gdb"
                                   :request "launch"
                                   :name "GDB::Run"
                           :gdbpath "/home/temp/.cargo/bin/rust-gdb"
                                   :target nil
                                   :cwd nil))
;;   (require 'dap-lldb)
;;   (require 'dap-gdb-lldb)
;;   ;; Installs .extension/vscode
;;   (dap-gdb-lldb-setup)
;;   (dap-register-debug-template
;;    "Rust::GDB Run Configuration"
;;    (list :type "gdb"
;;          :request "launch"
;;          :name "LLDB::Run"
;; 	 :gdbpath "rust-lldb"
;;          :target nil
;;          :cwd nil))


;  (setq dap-cpptools-extension-version "1.17.1")

  (with-eval-after-load 'lsp-rust
    (require 'dap-cpptools))

;; Also have to install mono and nevermind?
  ;; (with-eval-after-load 'dap-cpptools
  ;;   ;; Add a template specific for debugging Rust programs.
  ;;   ;; It is used for new projects, where I can M-x dap-edit-debug-template
  ;;   (dap-register-debug-template "Rust::CppTools Run Configuration"
  ;;                                (list :type "cppdbg"
  ;;                                      :request "launch"
  ;;                                      :name "Rust::Run"
  ;;                                      :MIMode "gdb"
  ;;                                      :miDebuggerPath "rust-gdb"
  ;;                                      :environment []
  ;;                                      :program "${workspaceFolder}/target/debug/hello / replace with binary"
  ;;                                      :cwd "${workspaceFolder}"
  ;;                                      :console "external"
  ;;                                      :dap-compilation "cargo build"
  ;;                                      :dap-compilation-dir "${workspaceFolder}")))

  ;; (with-eval-after-load 'dap-mode
  ;;   (setq dap-default-terminal-kind "integrated") ;; Make sure that terminal programs open a term for I/O in an Emacs buffer
  ;;   (dap-auto-configure-mode +1))

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
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;; ------------------------- C/C++ SETUP ----------------------

(use-package cmake-mode)

(setq package-selected-packages '(lsp-mode yasnippet lsp-treemacs helm-lsp
    projectile hydra flycheck company avy which-key helm-xref dap-mode))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;; sample `helm' configuration use https://github.com/emacs-helm/helm/ for details
(helm-mode)
(require 'helm-xref)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)

(which-key-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))
