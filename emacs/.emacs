;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Sections:
;;    -> Packages
;;    -> General
;;    -> Keybindings and commands
;;    -> EMACS user interface
;;    -> EVIL configuration
;;    -> Colors and Fonts
;;    -> Files and backups
;;    -> Text, tab and indent related
;;    -> Visual mode related
;;    -> Moving around, splits, windows and buffers
;;    -> Editing mappings
;;    -> vimgrep searching and cope displaying
;;    -> Spell checking
;;    -> Misc
;;    -> Helper functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initialize use-package (installed with nix)
(package-initialize)
;; use use-package
(eval-when-compile (require 'use-package))
;; use melpa archive
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; auto download packages
(setq use-package-always-ensure t)

;; automatically update packages
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; easy mode line config
(use-package delight)

;; use Superior Lisp Interaction Mode
(use-package slime
  ;;:bind ("C-b" . 'slime)
  :config
  (general-define-key
   :states 'normal
   :keymaps 'override
   "C-s" 'slime)
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy)))

;; better keybindings
(use-package general)

;; autocomplete
(use-package company
  :delight company-mode
  :config
  (add-to-list 'company-frontends 'company-tng-frontend)
  (add-to-list 'completion-styles 'initials t)
  (setq company-minimum-prefix-length 2)
  (global-company-mode 1))

;; linting
(use-package flycheck
  :config
  (global-flycheck-mode))

;; vim emulation
(use-package evil
  :config
  (evil-mode 1)
  ;; no broken undo tree
  (global-undo-tree-mode -1))

;; like vim powerline
(use-package powerline)

;; themes for powerline
(use-package airline-themes)

;; themes from doom emacs
(use-package doom-themes
  :config
  ;; flash mode line when emacs bell rings
  (doom-themes-visual-bell-config))

;; buffer local themes
(use-package load-theme-buffer-local)

;;  ;; lighter shade for buffers which represent open files. Praise the sun.
;;  (use-package solaire-mode
;;      ;; turn on solaire mode
;;      :hook ((change-major-mode after-revert) . turn-on-solaire-mode)
;;      :config
;;      (solaire-mode-swap-bg))


;; projectile project managment
(use-package projectile
  :delight projectile-mode
  :config
  (general-define-key
   :states 'normal
   :keymaps 'override
   "C-p" 'projectile-find-file)
  (projectile-mode +1))
  

;; helm framework
(use-package helm
  :delight helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-b" . helm-mini))
  :config
  (helm-mode +1))

;; use helm for projectile
(use-package helm-projectile
  :config
  (helm-projectile-on))

;; ORG MODE
(use-package org
  :hook (org-mode ((lambda nil (load-theme-buffer-local 'tsdh-light (current-buffer)))))
  :config
  (setq header-line-format " ")
  ;;(add-hook 'org-mode-hook '(load-theme-buffer-local 'tsdh-light (current-buffer)))
  ;; (lambda () (progn
  ;;              (setq left-margin-width 2)
  ;;              (setq right-margin-width 2)
  ;;              (set-window-buffer nil (current-buffer))))
  ;;(setq line-spacing 0.1)
  (setq org-startup-indented t
        ;;org-bullets-bullet-list '(" ") ;; no bullets, needs org-bullets package
        ;;org-ellipsis "  " ;; folding symbol
        org-pretty-entities t
        org-hide-emphasis-markers t
        ;; show actually italicized text instead of /italicized text/
        ;;;org-agenda-block-separator ""
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t))

;; distraction free writing
(use-package darkroom)

;; easily restart emacs daemon
(use-package restart-emacs)

;; integration with nix shell
(use-package nix-sandbox
  :config
  ;; ;; allows flycheck to find executables from nix shell
  ;; (setq flycheck-command-wrapper-function
  ;;       (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command))
  ;;       flycheck-executable-find
  ;;       (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd)))

  ;; ;; allows haskell-mode to find ghc in current nix shell
  ;; (setq haskell-process-wrapper-function
  ;;       (lambda (args) (apply 'nix-shell-command (nix-current-sandbox) args)))
  )


;; nix language autocomplete
(use-package company-nixos-options
  :config
  (add-to-list 'company-backends 'company-nixos-options))

;; nix language support
(use-package nix-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
  (add-hook 'nix-mode-hook
            (lambda ()
              (setq-local indent-line-function #'indent-relative))))

;; haskell language support
(use-package haskell-mode
  :config
  ;; allows capf and dabbrev backends while using haskell
  (add-hook 'haskell-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   (append '((company-capf company-dabbrev-code))
                           company-backends)))))

;; elm language support
(use-package flycheck-elm
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-elm-setup))
  
(use-package elm-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run as server
(server-mode 1)

;; save curson position
(save-place-mode 1)

;; always follow symlinks, even in vc
(setq vc-follow-symlinks t)

;; enter mode for bash on .profile, .bash_aliases, .inputrc
(add-to-list 'auto-mode-alist '(".profile\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '(".bash_aliases\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '(".inputrc\\'" . shell-script-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => Keybindings and commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(general-evil-setup)
(evil-ex-define-cmd "Src" 'reload-init-file)
(evil-ex-define-cmd "Restart" 'restart-emacs)

(general-nmap
 "C-h" 'evil-window-left
 "C-j" 'evil-window-down
 "C-k" 'evil-window-up
 "C-l" 'evil-window-right)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => EMACS user interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; disable garbage UI elements
(tool-bar-mode -1)
(menu-bar-mode -1)
(add-to-list 'default-frame-alist
             '(vertical-scroll-bars . nil))

;; display line numbers
(global-display-line-numbers-mode 1)

;; disable startup screen and scratch message
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; type y or n, not yes or no.
(fset 'yes-or-no-p 'y-or-n-p)

;; start client maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; vim-like clipboard
(setq select-enable-clipboard nil)

;; match parens
(electric-pair-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => Colors and fonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; main theme
(load-theme 'doom-one t)

;; powerline theme
(load-theme 'airline-doom-one t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => Text, tab and indent related
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; dont print tabs
(setq-default tab-width 4
              indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => Moving around, splits, windows and buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; winner mode lets you switch between window configurations with C-c left and right.
(winner-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => Files and backups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; dont make backups
(setq make-backup-files nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; function to reload config
(defun reload-init-file ()
  "Load all elisp from 'user-init-file'."
  (interactive)
  (load-file user-init-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "b54826e5d9978d59f9e0a169bbd4739dd927eead3ef65f56786621b53c031a7c" "af717ca36fe8b44909c984669ee0de8dd8c43df656be67a50a1cf89ee41bde9a" "01e067188b0b53325fc0a1c6e06643d7e52bc16b6653de2926a480861ad5aa78" "b59d7adea7873d58160d368d42828e7ac670340f11f36f67fa8071dbf957236a" "a94f1a015878c5f00afab321e4fef124b2fc3b823c8ddd89d360d710fc2bddfc" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" default)))
 '(helm-mode t)
 '(package-selected-packages
   (quote
    (darkroom elm-mode flycheck-elm haskell-mode nix-mode company-nixos-options nix-sandbox helm-projectile flycheck restart-emacs projectile delight evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
