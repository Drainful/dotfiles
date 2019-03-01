;; -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Sections:
;;    -> Packages
;;    -> Evil
;;    -> Keybindings and commands
;;    -> User interface
;;    -> Shell
;;    -> Tramp
;;    -> Emacs client/server settings
;;    -> Colors, Themes, Fonts, and other aesthetic settings
;;    -> Text, tab and indent related
;;    -> Moving around, buffers, windows and splits
;;    -> Programming tools and settings
;;    -> Language specific tools and settings
;;       - Lisps
;;       - Emacs lisp
;;       - Common Lisp
;;       - Scheme
;;       - Racket
;;       - Shen
;;       - Clojure
;;       - C/C++
;;       - Rust
;;       - Julia
;;       - Haskell
;;       - Elm
;;       - Org
;;       - Nix
;;       - Bash
;;    -> Excessive BS
;;    -> Files and backups
;;    -> Helper functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; for startup speed (from doom emacs faq)
;; turn off GC before config 
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defvar temp-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;;; for startup speed (from doom emacs faq)
;; re-enable GC 
(add-hook 'emacs-startup-hook
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook
  (setq file-name-handler-alist temp-file-name-handler-alist))


;; not ready for this shit
;; (setq package-enable-at-startup nil ; don't auto-initialize!
;;       ;; don't add that `custom-set-variables' block to my initl!
;;       package--init-file-ensured t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initialize use-package (installed with nix)
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

(package-initialize)


;;(package-refresh-contents) ; don't need every time emacs starts

;; use use-package
;; (eval-when-compile (require 'use-package))

;; auto download packages
(setq use-package-always-ensure t)

(use-package quelpa)

;; don't auto upgrade quelpa stuff
(setq quelpa-upgrade-p nil)

(use-package quelpa-use-package)

(quelpa-use-package-activate-advice)

;; risky
;; ;; automatically update packages
;; (use-package auto-package-update
;;   :config
;;   (setq auto-package-update-delete-old-versions t)
;;   (setq auto-package-update-hide-results t)
;;   (auto-package-update-maybe))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => Evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; vim emulation
(use-package evil
  :init
  ;; highlight all search results
  (setq evil-search-module 'evil-search)
  ;; settings for evil-collection integration
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :config
  (evil-mode 1)
  ;; The undo tree sometimes deletes undo data, I prefer to just disable it.
  (global-undo-tree-mode -1))

;; vim keybindings for a variety of modes
(use-package evil-collection
  :init
  (setq evil-want-keybinding nil)
  :after evil
  :config
  (evil-collection-minibuffer-setup)
  (evil-collection-init))

;; 2 character find. TODO integration with easymotion.
(use-package evil-snipe
  ;; :after evil-easymotion
  ;; :config
  ;; (evilem-define (kbd "SPC s") 'evil-snipe-s)
  )

;; easymotion
(use-package evil-easymotion
  :config
  (evilem-default-keybindings "SPC"))

;; equivalent of surround.vim
(use-package "evil-surround"
  :config
  (global-evil-surround-mode 1))

;;; creation of text objects

;; this macro was copied from here: https://stackoverflow.com/a/22418983/4921402
(defmacro define-and-bind-quoted-text-object (name key start-regex end-regex)
  (let ((inner-name (make-symbol (concat "evil-inner-" name)))
        (outer-name (make-symbol (concat "evil-a-" name))))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key #',inner-name)
       (define-key evil-outer-text-objects-map ,key #',outer-name))))

(defalias 'textobj 'define-and-bind-quoted-text-object)

(textobj "pipe" "|" "|" "|")
(textobj "slash" "/" "/" "/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => Keybindings and commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Easily create keybindings
(use-package general)

(general-evil-setup)
(evil-ex-define-cmd "Src" 'reload-init-file)
(evil-ex-define-cmd "Restart" 'restart-emacs)

(general-nmap
  "C-h" 'evil-window-left
  "C-j" 'evil-window-down
  "C-k" 'evil-window-up
  "C-l" 'evil-window-right
  "s-h" 'evil-window-left
  "s-j" 'evil-window-down
  "s-k" 'evil-window-up
  "s-l" 'evil-window-right
  "s-H" 'evil-window-move-far-left
  "s-J" 'evil-window-move-very-bottom
  "s-K" 'evil-window-move-very-top
  "s-L" 'evil-window-move-far-right
  "C--" 'helm-projectile-grep
  ;; Move a line of text using ALT+[jk]
  "M-j" (kbd ":move + RET")
  "M-k" (kbd ":move .-2 RET")
  "M-j" 'move-line-down
  "M-k" 'move-line-up)

(general-vmap
  ;; Move a visual block of text using ALT+[jk]
  "M-k" (kbd ":move '< -2 RET `> my `< mz gv`yo`z"))
  "M-j" (kbd ":move '> + RET `< my `> mz gv`yo`z")

;; leader key
(defconst leader-key ",")
(defconst alt-leader "SPC")

(general-create-definer leader-key-def
  :prefix leader-key)

(general-create-definer eval-key-def
  :prefix (concat leader-key " e"))

(general-create-definer xpand-key-def
  :prefix (concat leader-key " x"))

(general-create-definer start-key-def
  :prefix (concat leader-key " s"))

;; general leader definitions
(leader-key-def 'normal
  "q" 'kill-this-buffer              ; ",q" to kill buffer not window.
  "b" 'helm-mini                     ; ",b" to switch buffers.
  "f" 'helm-find-files               ; ",f" to find file (replace :e)
  "p" '(lambda () (interactive)
         ;; ",p" to fuzzy find directory from home directory
         (fzf/start "~/" "find ${1:-.} -path '*/\\.*' -prune \ -o -type d -print 2> /dev/null")) 
  "o" 'occur 
  "i" 'imenu
  "RET" (kbd ":noh")) ; ,RET to clear highlighted search results.

(leader-key-def 'visual
  "c" 'comment-or-uncomment-region)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => User interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mode line time + battery
;; (display-time-mode 1)
;; (display-battery-mode 1)

;; Hide info on mode line
(use-package delight :quelpa (:stable t)
  :config
  (delight 'eldoc-mode nil "eldoc"))

;; use helm narrowing framework framework for many narrowing tasks
(use-package helm
  :delight
  :bind (("M-x" . helm-M-x))
  :config (helm-mode t))

;; disable startup screen and scratch message
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; inital major mode
(setq initial-major-mode 'emacs-lisp-mode)

;; prefer scratch buffer
;; nice splash screen which allows access to recent projects and files
;; (use-package dashboard
;;   :ensure t
;;   :config
;;   (dashboard-setup-startup-hook)
;;   (setq dashboard-startup-banner 'logo)
;;   (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

;; folding, not quite as good as vim
(add-hook 'prog-mode-hook #'hs-minor-mode)
(delight 'hs-minor-mode nil "hideshow")

;; (use-package evil-vimish-fold
;;   :config
;;   (evil-vimish-fold-mode 1))

;; makes the modeline like vim powerline
;; (use-package powerline)

;; disable garbage UI elements
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(add-to-list 'default-frame-alist
             '(vertical-scroll-bars . nil))

;; display line numbers
(global-display-line-numbers-mode 1)
(display-line-numbers-mode 1)

;; display column numbers
(setq column-number-mode t)

;; type y or n, not yes or no.
(fset 'yes-or-no-p 'y-or-n-p)

;; start client maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; vim-like clipboard
(setq select-enable-clipboard nil)

;; automatically create matching delimiters as you write
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


;; Mouse scroll settings
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; "Discoverable" (or rediscoverable) keybindings. Like spacemacs.
(use-package which-key
  :config
  (which-key-add-prefix-title
    ", e" "eval"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => Shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; I prefer shell because comint mode works well with evil.
;;; Eshell
(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

;; from https://www.emacswiki.org/emacs/EshellMultipleEshellBuffers
(defun eshell-new()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N))

(global-set-key [f2] 'eshell)
;; Let me use C-j/k
(add-hook 'eshell-mode-hook ; needs to be in a hook because eshell is dumb
          (lambda ()
            (general-define-key :states 'normal :keymaps 'eshell-mode-map
                                "C-j" 'evil-window-down
                                "C-k" 'evil-window-up)))
;; set proper beginning of line so that evil commends don't get interrupted.
(defun restrict-bol (string)
  (propertize string 'inhibit-line-move-field-capture t
                     'rear-nonsticky t
                     'field 'output
                     'front-sticky '(field inhibit-line-move-field-capture)))

;; (setq eshell-prompt-regexp (regexp-quote "^\b$")
;;       eshell-prompt-function
;;       (lambda nil ""))

;; (setq old-eshell-prompt-function (lambda nil (eshell-prompt-function)))
;; (setq old-eshell-prompt-function (symbol-value 'eshell-prompt-function))
;; (setq eshell-prompt-function
;;       (lambda nil
;;         (restrict-bol (old-eshell-prompt-function))))
(setq eshell-prompt-function
      (lambda nil
        (restrict-bol (concat
                   (eshell/pwd)
                   " $ "))))

;;; Shell
(setq comint-prompt-read-only t ; Don't let me delete the comint prompt duh
      comint-move-point-for-output nil ; reduce frequent redisplays
      comint-scroll-show-maximum-output nil)
(global-set-key [f1] 'shell)
(general-define-key :states 'normal :keymaps 'shell-mode-map
   "C-j" 'evil-window-down
   "C-k" 'evil-window-up)

;; multiple terminals if necessary
(use-package multi-term
  :config
  ;; (global-set-key [f1] 'multi-term)
  ;; access shift arrow keys
  (define-key global-map "\eO2D" (kbd "S-<left>"))
  (define-key global-map "\eO2C" (kbd "S-<right>"))
  ;; term movement
  (general-define-key
   :states 'normal
   :keymaps 'term-mode-map
   "S-<right>" 'multi-term-next
   "S-<left>" 'multi-term-prev
   ;; was overridden
   "C-j" 'evil-window-down
   "C-k" 'evil-window-up))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => Tramp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun connect-to-serenity ()
  (interactive)
  (dired "/ssh:axf1557@serenity.ist.rit.edu:/home/MAIN/axf1557/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => Emacs client/server settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run as server
(server-mode 1)

;; easily restart emacs daemon
(use-package restart-emacs)

;; focus any new frames
(add-to-list 'after-make-frame-functions 'select-frame-set-input-focus)

(cl-defun make-daemon-frame (socket-name &rest args)
  "Make a new emacs frame for the daemon with the given socket name."
  (apply 'start-process
         (concat socket-name "-frame")
         nil
         "emacsclient" "--create-frame" (concat "--socket-name=" socket-name)
         args))

(cl-defun make-daemon (socket-name &key (create-buffer t) before after (theme 'doom-nord-light))
  "Make a new emacs daemon with the given socket name."
  (message "Loading inferior emacs")
  (let ((daemon-name (concat socket-name "-daemon")))
    (start-process-shell-command
     daemon-name (when create-buffer daemon-name)
     (concat before
             "emacs --daemon=" socket-name
             ;; "--execute \"(load-theme '"
             ;; (symbol-name theme)
             ;; " t)\""
             ";"
             after))))

(defun nix-daemon ()
  "Start a daemon and frame in the current nix project."
  (interactive)
  (if (nix-current-sandbox)
      (let ((default-directory (nix-current-sandbox))
            (daemon-name (elt (nreverse (split-string (nix-current-sandbox) "/")) 1)))
        (if (get-buffer-process (get-buffer (concat daemon-name "-daemon")))
            (nix-daemon-frame)
          (make-daemon daemon-name
                     ;; :create-buffer nil
                     :theme 'doom-opera
                     :before "nix-shell --command \""
                     :after (concat "emacsclient --create-frame "
                                    (concat "--socket-name=" daemon-name)
                                    "\""))))
    (error "No nix environment was found")))

(defun nix-daemon-frame ()
  "Start a daemon and frame in the current nix project."
  (interactive)
  (if (nix-current-sandbox)
      (let ((default-directory (nix-current-sandbox))
            (daemon-name (elt (nreverse (split-string (nix-current-sandbox) "/")) 1)))
        (unless (get-buffer-process (get-buffer (concat daemon-name "-daemon")))
          (error "The daemon is not active"))
        (start-process-shell-command
         (concat daemon-name "-frame") nil
         (concat "nix-shell --command "
                 (concat "\"emacsclient --create-frame --socket-name=" daemon-name "\""))))
    (error "No nix environment was found")))

;; (defun nix-daemon ()
;;   "Start a daemon and frame in the current nix project."
;;   (interactive)
;;   (when (nix-current-sandbox)
;;     (let ((default-directory (nix-current-sandbox))
;;           (daemon-name (elt (nreverse (split-string (nix-current-sandbox) "/")) 1)))
;;       (let ((buffer-name (concat daemon-name "-daemon")))
;;         (start-process-shell-command
;;          buffer-name buffer-name
;;          (concat "nix-shell --command \""
;;                  "emacs --daemon=" daemon-name
;;                  "--execute \\\"(load-theme 'doom-tomorrow-night t)\\\""
;;                  ";"
;;                  (concat "emacsclient --create-frame "
;;                                   (concat "--socket-name=" daemon-name)
;;                                   "\"")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => Colors, Themes, Fonts, and other aesthetic settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  ;; lighter shade for buffers which represent open files.
;;  (use-package solaire-mode
;;      ;; turn on solaire mode
;;      :hook ((change-major-mode after-revert) . turn-on-solaire-mode)
;;      :config
;;      (solaire-mode-swap-bg))

;; Hilight the parenthesis corresponding to the one under the cursor
(show-paren-mode nil)

;;;; Themes

;; poet theme for org etc.
;; (use-package poet-theme
;;   :config
;;   (add-hook 'text-mode-hook
;;             (lambda ()
;;               (variable-pitch-mode 1)))
;;   (set-face-attribute 'default nil :family "Iosevka" :height 100)
;;   (set-face-attribute 'fixed-pitch nil :family "Iosevka")
;;   (set-face-attribute 'variable-pitch nil :family "Baskerville"))

;; themes from doom emacs
(use-package doom-themes
  :config
  ;; flash mode line when emacs bell rings
  (doom-themes-visual-bell-config))

;; themes for powerline
;; (use-package airline-themes)

;; allows the loading of themes on a per-buffer basis.
(use-package load-theme-buffer-local)

; main theme
;; automatic time-based theme change 
(when (display-graphic-p)
  (use-package theme-changer
  :after doom-themes
  :config
  (setq calendar-location-name "Rochester, NY")
  (setq calendar-latitude 43.16103)
  (setq calendar-longitude -77.6109219)
  (change-theme 'doom-one-light 'doom-one)))

;; powerline theme
;; (load-theme 'airline-doom-molokai t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => Text, tab and indent related
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; dont indent with tabs, indent witn 4 spaces.
(setq-default tab-width 4
              indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => Moving around, buffers, windows, and splits
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; winner mode lets you switch between window configurations with C-c left and right.
;; TODO rebind to C-left and C-right.
(winner-mode 1)

;; project navigation
(use-package projectile
  :delight "P"
  :after general
  :config
  (general-define-key
   :states 'normal
   :keymaps 'override
 "C-p" 'helm-projectile-find-file)
  (projectile-mode +1))

;; use helm for projectile
(use-package helm-projectile
  :config
  (helm-projectile-on))

;; file finding
(use-package fzf)

;; always follow symlinks, even in vc
(setq vc-follow-symlinks t)

;; swap to most recent buffer, from https://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
(defun er-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(general-define-key :states 'normal :keymaps 'override
                    "<tab>" 'er-switch-to-previous-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => Programming tools and settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; autocomplete
(use-package company
  :delight
  :config
  (add-to-list 'company-frontends 'company-tng-frontend) ; test this vs evil collection
  (add-to-list 'completion-styles 'initials t)
  ;;(add-to-list 'completion-styles 'substring t)
  (define-key company-active-map (kbd "M-.") 'company-show-location)
  (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
  ;;(setq company-dabbrev-downcase 0)
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0)
  (add-hook 'sly-mode-hook (lambda () (progn (setq company-idle-delay 0.5)
                                             (setq company-minimum-prefix-length 3))))
  (global-company-mode nil))

;; linting
(use-package flycheck
  :config
  (setq flycheck-global-modes '(not c-mode c++-mode)))

;; git integration
(use-package magit
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))
;; evil-friendly commands for magit
(use-package evil-magit)

;; magit keybindings
(leader-key-def 'normal
  "m" 'magit)

;; language server protocol
(use-package lsp-mode
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)

(add-hook 'lsp-mode-hook 'lsp-ui-mode)

;; compile by recursively looking for makefile
;; inspired by https://emacs.stackexchange.com/questions/7475/recursively-go-up-to-find-makefile-and-compile
(cl-defun compile-rec (&key (filename "Makefile") (command "make -k"))
  "Traveling up the path, find a Makefile and `compile'."
  (interactive)
  (let ((makefile-dir (locate-dominating-file default-directory filename)))
    (when makefile-dir
      (with-temp-buffer
        (cd makefile-dir)
        (compile command)))))

;; multiple major modes in a single buffer
;; (use-package polymode)
;; (use-package poly-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => Language specific tools and settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Lisp

;; ivy is required for lispy
(use-package ivy
  :quelpa (:stable t))

;; better lisp editing
(use-package lispy)

;; lispy-evil integration. I use lispyville mode as the de-facto minor mode
;; for editing lisp related languages.
(use-package lispyville
  :delight
  :hook ((emacs-lisp-mode lisp-mode lispy-mode clojure-mode shen-mode) . lispyville-mode)
  :config
   (lispyville-set-key-theme
    '(operators
      ;; atom-motions
      prettify
      wrap
      slurp-cp
      barf-cp
      c-w
      (escape insert)
      (additional-movement normal visual motion))))

;; Each depth layer of delimiter is given a different color (useful for lisps)
(use-package rainbow-delimiters
  :delight
  :hook (lispyville-mode-hook . rainbow-delimiters-mode))

;;;; Emacs Lisp

;; emacs lisp keybindings
(eval-key-def 'normal emacs-lisp-mode-map
  "b" 'eval-buffer
  "f" 'eval-defun)
(eval-key-def 'visual emacs-lisp-mode-map
  "r" 'eval-region)

;;;; Common Lisp

;; I use Sly, a common lisp IDE and fork of SLIME.
(use-package sly
  :quelpa (:stable t)
  :after evil
  :config

  ;; make functions for using specific lisp implementations.
  (defmacro define-sly-lisp (name command)
    `(defun ,name ()  (interactive)  (sly ,command)))

  (defmacro define-sly-lisp-defun (name fn)
    `(defun ,name ()  (interactive)  (sly (funcall ,fn))))

  ;;(define-sly-lisp-defun sbcl (lambda () (nix-executable-find (nix-current-sandbox) "sbcl")))
  (define-sly-lisp sbcl "sbcl")
  (define-sly-lisp ecl "ecl --load /home/adrian/quicklisp/setup.lisp")
  (define-sly-lisp ccl "ccl")
  (define-sly-lisp clisp "clisp")

  (setq inferior-lisp-program "sbcl")
  ;; Open sly debug buffers in emacs state, rather than evil state.
  (add-to-list 'evil-emacs-state-modes 'sly-db-mode))

;; ;; common lisp autocompletion with slime and company
;; (use-package slime-company)

;; sly keybindings
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
(eval-key-def 'visual 'sly-mode-map
  "r" 'sly-eval-region)

;;;; Scheme
(use-package geiser
  :config
  (setq geiser-active-implementations '(racket))
  ;; geiser keybindings
  (leader-key-def 'normal geiser-mode-map
    "z" 'geiser-mode-switch-to-repl
    "c" 'geiser-compile-file
    "l" 'geiser-load-file)
  
  (start-key-def 'normal geiser-mode-map
    "s" 'geiser
    "c" 'geiser-connect)
  
  (eval-key-def 'normal geiser-mode-map
    "b" 'geiser-eval-buffer
    "f" 'geiser-eval-definition)
  (eval-key-def 'visual 'geiser-mode-map
    "r" 'geiser-eval-region)
  (general-define-key
   :states 'normal
   :keymaps 'geiser-repl-mode-map
   ;; was overridden
   "C-j" 'evil-window-down
   "C-k" 'evil-window-up))

;;;; Racket
;; (use-package racket-mode)

;;;; Shen
(use-package shen-mode)

(leader-key-def 'normal shen-mode-map
  "z" 'switch-to-shen
  "c" 'shen-compile-file
  "l" 'shen-load-file)

(start-key-def 'normal shen-mode-map
  "s" 'run-shen)

(eval-key-def 'normal shen-mode-map
  "b" 'shen-eval-buffer
  "f" 'shen-eval-defun)

(eval-key-def 'visual 'shen-mode-map
  "r" 'shen-eval-region)

;;;; Clojure
(use-package clojure-mode)

;; CIDER like slime for clojure
(use-package cider
  :config
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion))

;; spinner for cider
(use-package spinner :quelpa (:stable t))

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

;; cider keybindings
(leader-key-def 'normal clojure-mode-map
  "s" 'cider-jack-in
  "z" 'cider-switch-to-repl-buffer
  "a" 'cider-close-ancillary-buffers)

(eval-key-def 'normal clojure-mode-map
  "b" 'cider-eval-buffer
  "f" 'cider-eval-defun-at-point)

;;;; C/C++/CPP/Cpp/Sepples
;; C/C++ language server which leverages lsp-mode
(use-package cquery
  :after projectile
  :init
  (add-hook 'c-mode-hook #'cquery//enable)
  (add-hook 'c++-mode-hook #'cquery//enable)
  :config
  (defun cquery//enable ()
    (condition-case nil
        (lsp)
      (user-error nil)))
  (setq cquery-executable "cquery")
  (setq cquery-extra-init-params '(:cacheFormat "msgpack"))
  (setq projectile-project-root-files-top-down-recurring
        (append '("compile_commands.json"
                  ".cquery")
                projectile-project-root-files-top-down-recurring)))

(use-package modern-cpp-font-lock
  :config
  (modern-c++-font-lock-global-mode t))

;; (add-hook 'c++-mode-hook #'lsp)
(add-hook 'c++-mode-hook 'flycheck-mode)

(leader-key-def 'normal c-mode-base-map
  "s" 'ff-find-other-file
  "c" 'compile-rec
  "r" '(lambda () (interactive) (compile-rec :command "make run")))

(setq-default c-basic-offset 4
              c-default-style "linux")



;; (use-package rtags
;;   :config

;;   ;; rtags must be installed
;;   (unless (rtags-executable-find "rc") (error "Binary rc is not installed!"))
;;   (unless (rtags-executable-find "rdm") (error "Binary rdm is not installed!"))

;;   ;; start daemon if not started when editing c
;;   (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
;;   (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
;;   (add-hook 'objc-mode-hook 'rtags-start-process-unless-running)

;;   (setq rtags-use-helm t))

;; (use-package company-rtags
;;   :after company rtags
;;   :config 
;;   (setq rtags-autostart-diagnostics t)
;;   (rtags-diagnostics)
;;   (setq rtags-completions-enabled t)
;;   (push 'company-rtags company-backends))

;; (use-package flycheck-rtags
;;   :after flycheck rtags
;;   :config
;;   (defun setup-flycheck-rtags ()
;;       (flycheck-select-checker 'rtags)
;;       (setq-local flycheck-highlighting-mode nil) ; RTags creates more accurate overlays.
;;       (setq-local flycheck-check-syntax-automatically nil)
;;       (rtags-set-periodic-reparse-timeout 2.0)  ; Run flycheck 2 seconds after being idle.
;;       )
;;   (add-hook 'c-mode-hook #'setup-flycheck-rtags)
;;   (add-hook 'c++-mode-hook #'setup-flycheck-rtags))

;; (use-package helm-rtags
;;   :after helm rtags
;;   :config
;;   (setq rtags-display-result-backend 'helm))

;;;; Rust

;;TODO

;;;; Julia
;;(use-package julia-mode)
(use-package julia-repl
  :config
  (add-hook 'julia-mode-hook 'julia-repl-mode))

;;;; Haskell
(use-package haskell-mode
  :config
  ;; allows capf and dabbrev backends while using haskell
  (add-hook 'haskell-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   (append '((company-capf company-dabbrev-code))
                           company-backends)))))

;;;; Elm
(use-package flycheck-elm
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-elm-setup))
  
(use-package elm-mode)

;;;; Org
(use-package org
  ;; :hook (org-mode ((lambda nil (load-theme-buffer-local 'tsdh-light (current-buffer)))))
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

;; Org mode table of contents without exporting
(use-package toc-org
  :config
  (add-hook 'org-mode-hook 'toc-org-mode))

;;;; Nix

;; integration with nix shell, could use some work. for now I just open a new emacs daemon in nix-shell.
(use-package nix-sandbox
  :config
  ;; (defun nix-executable-find-advice (orig-fun &rest args)
  ;;   (apply #'nix-executable-find (cons (nix-current-sandbox) args)))
  ;; (defun nix-executable-find-in-current-sandbox-advice (orig-fun &rest args)
  ;;   "Search for an executable in the current sandbox."
  ;;   (let ((exec-path (nix-exec-path (nix-current-sandbox))))
  ;;     (and exec-path (apply orig-fun args))))
  ;; (advice-add 'executable-find :around #'nix-executable-find-in-current-sandbox-advice)
  ;; (advice-add 'executable-find :after #'nix-executable-find)
  ;; (advice-remove 'executable-find #'nix-executable-find)
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
  :hook (nix-mode-hook . (lambda () (add-to-list 'company-backends 'company-nixos-options))))

;; nix language support
(use-package nix-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
  (add-hook 'nix-mode-hook
            (lambda ()
              (setq tab-always-indent nil)
              (setq indent-tabs-mode t))))

;;;; Bash
;; enter mode for bash on .profile, .bash_aliases, .inputrc
(add-to-list 'auto-mode-alist '(".profile\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '(".bash_aliases\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '(".inputrc\\'" . shell-script-mode))

;;;; Web
(use-package emmet-mode
  :delight
  :config
  (xpand-key-def 'normal emmet-mode-map
    "e" 'emmet-expand-line)
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode)) ;; enable Emmet's css abbreviation.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => Excessive BS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; control spotify running on any device from emacs
(use-package spot4e :load-path "~/code/elisp/spot4e"
  :after general
  :requires helm url json
  :config
  (setq spot4e-refresh-token "AQCOzkgs6cLWmIWZ-ucPLPwMoEOC6HCRfeqhs7DIRMVmmCeG6g5hi7EGR7Dvms5kZf925jH0UzhhQ8xYdiCPLt3Nw-lW4A8_eDlN1rKrr9FEAHv4MhaasQn6-ai9wiC12Ex4XA")
  (run-with-timer 0 (* 60 59) 'spot4e-refresh)
  (general-create-definer spotify-key-def
    :prefix (concat alt-leader " s"))
  (spotify-key-def 'normal
                   "b" 'spot4e-helm-search-user-tracks
                   "r" 'spot4e-helm-search-recommendations-track
                   "s" 'spot4e-player-pause
                   "p" 'spot4e-player-play
                   "n" 'spot4e-player-next
                   "N" 'spot4e-player-previous))

;; emacs window manager
(use-package exwm
  :config
  (fringe-mode 1)
  (require 'exwm-config)
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                          (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-class-name))))
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (when (or (not exwm-instance-name)
                        (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-title))))
  (setq exwm-input-global-keys
        `(
          ;; Bind "s-r" to exit char-mode and fullscreen mode.
          ([?\s-r] . exwm-reset)
          ;; Bind "s-c" to enter char mode
          ([?\s-c] . exwm-input-release-keyboard)
          ;; Bind "s-w" to switch workspace interactively.
          ([?\s-w] . exwm-workspace-switch)
          ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))
          ;; Bind "s- " to launch applications
          ([?\s- ] . (lambda (command)
		               (interactive (list (read-shell-command "$ ")))
		               (start-process-shell-command command nil command)))
          ;; Bind "s-<f2>" to "slock", a simple X display locker.
          ;; ([s-f2] . (lambda ()
		  ;;             (interactive)
		  ;;             (start-process "" nil "/usr/bin/slock")))
          ))
  ;; logout function
  (defun exwm-logout ()
    (interactive)
    (recentf-save-list)
    (save-some-buffers)
    (start-process-shell-command "logout" nil "lxsession-logout"))
  ;; start in char mode by default
  ;; (setq exwm-manage-configurations '((t char-mode t)))
  ;; sys tray with network
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)
  (shell-command "nm-applet")
  ;; set prefix keys
  (setq my-exwm-prefix-keys (list ?\s-f ?\s-h ?\s-j ?\s-k ?\s-l ?\s-H ?\s-J ?\s-K ?\s-L ?\: ?\,))
  (setq exwm-input-prefix-keys (nconc exwm-input-prefix-keys my-exwm-prefix-keys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => Files and backups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; dont make backups
(setq make-backup-files nil)

;; save cursor position in a file between sessions
(save-place-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; => Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun reload-init-file ()
  "Load all elisp from 'user-init-file'."
  (interactive)
  (load-file user-init-file))

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

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
    ("93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "9129c2759b8ba8e8396fe92535449de3e7ba61fd34569a488dd64e80f5041c9f" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "99b2fdc7de612b74fcb76eb3a1962092cf729909223434f256c7007d490d787a" "3e2fd26606cba08448283cc16860c1deab138ede73c38c91fdaf4e5c60ece485" "07ed389142fef99649ebcfe1f835cf564fc40bb342d8d2f4e13f05302378a47a" "3eb93cd9a0da0f3e86b5d932ac0e3b5f0f50de7a0b805d4eb1f67782e9eb67a4" "5e5345ea15d0c2234356bc5958a224776b83198f0c3df7155d1f7575405ce990" "251348dcb797a6ea63bbfe3be4951728e085ac08eee83def071e4d2e3211acc3" "3fa07dd06f4aff80df2d820084db9ecbc007541ce7f15474f1d956c846a3238f" "b563a87aa29096e0b2e38889f7a5e3babde9982262181b65de9ce8b78e9324d5" "158013ec40a6e2844dbda340dbabda6e179a53e0aea04a4d383d69c329fba6e6" "3a3de615f80a0e8706208f0a71bbcc7cc3816988f971b6d237223b6731f91605" "0cd56f8cd78d12fc6ead32915e1c4963ba2039890700458c13e12038ec40f6f5" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "64ca5a1381fa96cb86fd6c6b4d75b66dc9c4e0fc1288ee7d914ab8d2638e23a9" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "b54826e5d9978d59f9e0a169bbd4739dd927eead3ef65f56786621b53c031a7c" "af717ca36fe8b44909c984669ee0de8dd8c43df656be67a50a1cf89ee41bde9a" "01e067188b0b53325fc0a1c6e06643d7e52bc16b6653de2926a480861ad5aa78" "b59d7adea7873d58160d368d42828e7ac670340f11f36f67fa8071dbf957236a" "a94f1a015878c5f00afab321e4fef124b2fc3b823c8ddd89d360d710fc2bddfc" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" default)))
 '(electric-pair-mode t)
 '(inferior-shen-program "~/Projects/common-lisp/my-forks/shen-cl/bin/sbcl/shen" t)
 '(line-spacing 0.2)
 '(org-blank-before-new-entry (quote ((heading . auto) (plain-list-item))))
 '(package-selected-packages
   (quote
    (toc-org nix-buffer evil-goggles modern-cpp-font-lock which-key emacs-which-key geiser comint exwm-systemtray poet-theme exwm sly emmet-mode multi-term helm-fzf theme-changer circe shen-emacs shen-mode fzf sql-indent evil-surround lispyville lispy ivy company-lsp lsp-ui cquery markdown-mode evil-magit magit sublimity-scroll evil-snipe snipe evil-easymotion evil-search-highlight-persisist color-theme-approximate emacs-dashboard cider macrostep evil-collection quelpa ac-slime julia-repl company-nixos-options load-theme-buffer-local doom-themes company clojure-mode general sly-quicklisp flycheck-pos-tip rainbowdelimiters rainbow-delimiters mic-paren evil-vimish-fold rainbow-delimeters evil-cleverparens darkroom elm-mode flycheck-elm haskell-mode nix-mode helm-projectile flycheck restart-emacs projectile delight evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
