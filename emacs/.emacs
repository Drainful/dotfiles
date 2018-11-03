;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Sections:
;;    -> Packages
;;    -> General / Hydra
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
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
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

;; better keybindings
(use-package general)

;; common lisp autocompletion with slime and company
;; (use-package slime-company)

;; clojure mode
(use-package clojure-mode)

;; CIDER like slime for clojure
(use-package cider
  :config
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion))

;; ;; linting clojure
;; (use-package flycheck-clojure
;;   :config
;;   (eval-after-load 'flycheck '(flycheck-clojure-setup))
;;   (add-hook 'after-init-hook #'global-flycheck-mode)
;;   ;; navigate clojure errors with flycheck functions
;;   (add-hook 'cider-mode-hook
;;   (lambda () (setq next-error-function #'flycheck-next-error-function))))

;; ;; avoid clash between cider eldoc and flycheck-clojure
;; (use-package flycheck-pos-tip
;;   :config
;;   (eval-after-load 'flycheck
;;     '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

;; use Sylvester the Cat's Common Lisp IDE for Emacs
(use-package sly-repl-ansi-color)
(use-package sly
  ;;:bind ("C-b" . 'slime)
  :config
  (push 'sly-repl-ansi-color sly-contribs)
  ;;(load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "quicklisp run"))


;; autocomplete
(use-package company
  :delight company-mode
  :config
  (add-to-list 'company-frontends 'company-tng-frontend)
  (add-to-list 'completion-styles 'initials t)
  ;;(add-to-list 'completion-styles 'substring t)
  (define-key company-active-map (kbd "M-.") 'company-show-location)
  (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
  (setq company-minimum-prefix-length 2)
  ;;(setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0)
  (global-company-mode nil))

;; linting
(use-package flycheck
  :config
  (global-flycheck-mode))

;; vim emulation
(use-package evil
  :init
  ;; settings for evil-collection integration
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :config
  (evil-mode 1)
  ;; no broken undo tree
  (global-undo-tree-mode -1))

(use-package evil-collection
  :init
  (setq evil-want-keybinding nil)
  :after evil
  :config
  (evil-collection-init))

;; vim like folding
(add-hook 'prog-mode-hook #'hs-minor-mode)
;; (use-package evil-vimish-fold
;;   :config
;;   (evil-vimish-fold-mode 1))

;; better lisp editing
(use-package lispy)

;; lispy-evil integration
(use-package lispyville
  :hook ((emacs-lisp-mode lisp-mode lispy-mode clojure-mode) . lispyville-mode)
  :config
   (lispyville-set-key-theme
    '(operators
      atom-motions
      prettify
      wrap
      slurp-cp
      barf-cp
      c-w
      (escape insert)
      (additional-movement normal visual motion))))

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
   "C-p" 'helm-projectile-find-file)
  (projectile-mode +1))
  
;; show matching paren fast
;; (use-package mic-paren
;;   :config
;;   (paren-activate))

;; helm framework
(use-package helm
  :delight helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-b" . helm-mini))
  :config (helm-mode t))

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

;; rainbow delimiters
;; (use-package rainbowdelimiters
;;   :hook (lispyville-mode-hook . rainbow-delimiters-mode))
;;OK so use package can only install it once, then bugs, so just install and use this add hook.
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; nix language autocomplete
(use-package company-nixos-options
  :config
  ;;(add-to-list 'company-backends 'company-nixos-options)
  )

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
;; => General / Hydra
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run as server
(server-mode 1)

;; save cursor position
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

;; leader key
(defconst leader-key ",")

(general-create-definer leader-key-def
  :prefix leader-key)

(general-create-definer eval-key-def
  :prefix (concat leader-key " e"))

(general-create-definer start-key-def
  :prefix (concat leader-key " s"))

(general-create-definer space-def)

;; general leader definitions
(leader-key-def 'normal
  "q" 'kill-this-buffer ; ",q" to kill buffer not window.
  "b" 'helm-mini
  "o" 'occur
  "i" 'imenu) ; ",b" to switch buffers.

;; sly
(leader-key-def 'normal sly-mode-map
  "z" 'sly-switch-to-output-buffer
  "c" 'sly-compile-file
  "l" 'sly-load-file)

(start-key-def 'normal sly-mode-map
  "s" 'sly
  "c" 'sly-connect)

(eval-key-def 'normal sly-mode-map
  "b" 'sly-eval-buffer
  "f" 'sly-eval-defun)

;; cider
(leader-key-def 'normal clojure-mode-map
  "s" 'cider-jack-in
  "z" 'cider-switch-to-repl-buffer
  "c" 'cider-close-ancillary-buffers)

(eval-key-def 'normal clojure-mode-map
  "b" 'cider-eval-buffer
  "f" 'cider-eval-defun-at-point)


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
(display-line-numbers-mode 1)

;; disable startup screen and scratch message
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; set scratch buffer to sly common lisp
(setq initial-major-mode 'sly-mode)

;; type y or n, not yes or no.
(fset 'yes-or-no-p 'y-or-n-p)

;; start client maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; vim-like clipboard
(setq select-enable-clipboard nil)

;; match parens
(electric-pair-mode nil)

;;show expression
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis)
;; highlight parens when inside them (thanks to https://stackoverflow.com/questions/34846531/show-parentheses-when-inside-them-emacs)
(define-advice show-paren-function (:around (fn) fix)
  "Highlight enclosing parens."
  (cond ((looking-at-p "\\s(") (funcall fn))
        (t (save-excursion
             (ignore-errors (backward-up-list))
             (funcall fn)))))


;; show paren match
(show-paren-mode nil)

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
    ("3a3de615f80a0e8706208f0a71bbcc7cc3816988f971b6d237223b6731f91605" "0cd56f8cd78d12fc6ead32915e1c4963ba2039890700458c13e12038ec40f6f5" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "64ca5a1381fa96cb86fd6c6b4d75b66dc9c4e0fc1288ee7d914ab8d2638e23a9" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "b54826e5d9978d59f9e0a169bbd4739dd927eead3ef65f56786621b53c031a7c" "af717ca36fe8b44909c984669ee0de8dd8c43df656be67a50a1cf89ee41bde9a" "01e067188b0b53325fc0a1c6e06643d7e52bc16b6653de2926a480861ad5aa78" "b59d7adea7873d58160d368d42828e7ac670340f11f36f67fa8071dbf957236a" "a94f1a015878c5f00afab321e4fef124b2fc3b823c8ddd89d360d710fc2bddfc" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" default)))
 '(package-selected-packages
   (quote
    (sly-repl-ansi-color sly sly-quicklisp helm-ag flycheck-pos-tip rainbowdelimiters rainbow-delimiters mic-paren evil-vimish-fold rainbow-delimeters lispyville lispy evil-cleverparens darkroom elm-mode flycheck-elm haskell-mode nix-mode nix-sandbox helm-projectile flycheck restart-emacs projectile delight evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
