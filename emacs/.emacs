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

(use-package delight)

;; better keybindings
(use-package general)

;; autocomplete
(use-package company
  :delight company-mode
  :config
  (add-to-list 'company-frontends 'company-tng-frontend)
  (add-to-list 'completion-styles 'initials t)
  (setq company-minimum-prefix-length 2)
  (company-mode 1))

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

;; coloschemes from doom emacs
(use-package doom-themes
  :config
  ;; flash mode line when emacs bell rings
  (doom-themes-visual-bell-config))

;;  ;; lighter shade for buffers which represent open files. Praise the sun.
;;  (use-package solaire-mode
;;      ;; turn on solaire mode
;;      :hook ((change-major-mode after-revert) . turn-on-solaire-mode)
;;      :config
;;      (solaire-mode-swap-bg))


;; projectile project managment
(use-package projectile
  :config
  (general-define-key
   :states 'normal
   :keymaps 'override
   "C-p" 'projectile-find-file))
  ;;(projectile-mode +1))
  

;; helm framework
(use-package helm
  :delight helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-b" . helm-mini))
  :config
  (helm-mode +1))

;; restart emacs daemon
(use-package restart-emacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run as server
(server-mode 1)

;; save curson position
(save-place-mode 1)

;; always follow symlinks, even in vc
(setq vc-follow-symlinks t)

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
(toggle-scroll-bar -1)

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
    ("af717ca36fe8b44909c984669ee0de8dd8c43df656be67a50a1cf89ee41bde9a" "01e067188b0b53325fc0a1c6e06643d7e52bc16b6653de2926a480861ad5aa78" "b59d7adea7873d58160d368d42828e7ac670340f11f36f67fa8071dbf957236a" "a94f1a015878c5f00afab321e4fef124b2fc3b823c8ddd89d360d710fc2bddfc" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" default)))
 '(helm-mode t)
 '(package-selected-packages
   (quote
    (flycheck restart-emacs projectile delight evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
