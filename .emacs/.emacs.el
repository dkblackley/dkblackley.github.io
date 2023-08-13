; Makes several backups of the init file
(setq
backup-by-copying t ; don't clobber symlinks
backup-directory-alist
'(("." . "~/.saves")) ; don't litter my fs tree
delete-old-versions t
kept-new-versions 6
kept-old-versions 2
version-control t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(modus-vivendi))
 '(custom-safe-themes
   '("7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" default))
 '(package-selected-packages
   '(exec-path-from-shell docker flycheck rustic dap-mode toml-mode projectile neotree company lsp-pyright lsp-ui lsp-mode solarized-theme)))
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

(setq lsp-enable-snippet nil)
(use-package lsp-mode
  :hook ((c++-mode python-mode) . lsp-deferred)
  :commands lsp
  ;; extra rust commands
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
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
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))



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
  (company-idle-delay 0.3) ;; how long to wait until popup
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
  :bind
  (:map global-map
  ("C-c d" . dap-debug)))

;; --------------------------------------- DOCKER SETUP ------------------------------

(use-package docker
  :ensure t
  :bind ("C-c C-c d" . docker))

;; --------------------------------------- PYTHON SETUP ------------------------------

(require 'dap-cpptools)

(use-package lsp-pyright
  :hook (python-mode . (lambda () (require 'lsp-pyright)))
  :init (when (executable-find "python3")
          (setq lsp-pyright-python-executable-cmd "python3")
	  (setq lsp-pyright-multi-root nil)
	  (setq lsp-pyright-auto-search-paths nil)))

(setf (lsp-session-folders-blacklist (lsp-session)) nil)


(require 'dap-python)
;; if you installed debugpy, you need to set this
;; https://github.com/emacs-lsp/dap-mode/issues/306
(setq dap-python-debugger 'debugpy)


;; (require 'dap-gdb-lldb)


;; -------------------------- RUST SETUP -----------------------

;; (setq dap-gdb-lldb-path "~/.emacs-extras/webfreak.debug-0.26.1.vsix")

;; (require 'dap-gdb-lldb)

;; ;; Still need to do dap-cpptools-setup, but I think those come from microsoft. could zip my emacs extension folder
;; (use-package toml-mode :ensure)
;;     (dap-register-debug-template "Rust::GDB Run Configuration"
;;                              (list :type "gdb"
;;                                    :request "launch"
;;                                    :name "GDB::Run"
;;                            :gdbpath "/home/yelnat/.cargo/bin/rust-gdb"
;;                                    :target nil
;;                                    :cwd nil))
;;   (require 'dap-lldb)
;;   (require 'dap-gdb-lldb)
;;   ;; installs .extension/vscode
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
  (with-eval-after-load 'dap-cpptools
    ;; Add a template specific for debugging Rust programs.
    ;; It is used for new projects, where I can M-x dap-edit-debug-template
    (dap-register-debug-template "Rust::CppTools Run Configuration"
                                 (list :type "cppdbg"
                                       :request "launch"
                                       :name "Rust::Run"
                                       :MIMode "gdb"
                                       :miDebuggerPath "rust-gdb"
                                       :environment []
                                       :program "${workspaceFolder}/target/debug/hello / replace with binary"
                                       :cwd "${workspaceFolder}"
                                       :console "external"
                                       :dap-compilation "cargo build"
                                       :dap-compilation-dir "${workspaceFolder}")))

  (with-eval-after-load 'dap-mode
    (setq dap-default-terminal-kind "integrated") ;; Make sure that terminal programs open a term for I/O in an Emacs buffer
    (dap-auto-configure-mode +1))

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

