;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1))

(defvar temp-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
  (setq file-name-handler-alist temp-file-name-handler-alist))

(defun auto-mode-add (mode &rest filenames)
  "Add a number of file name patterns to the given mode in
auto-mode-alist"
  (mapcar (lambda (name)
            (add-to-list 'auto-mode-alist
                         `(,name . ,mode)))
          filenames))

(defun remove-keyword-args (list)
  "Remove keyword arguments from the given list"
  (car (general--remove-keyword-args list)))

(defun earmuffs (string)
  "Add *earmuffs* to the given string. This represents the name
  of a buffer which is not associated with a file."
  (concat "*" string "*"))

(defun final-directory-name (path)
  "Return the name of the final directory of the given path"
  (elt (nreverse (split-string path "/")) 1))

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

(defun reboot ()
  (interactive)
  (shell-command "reboot"))

(defun dotfiles (path)
  "Return path relative to the dotfiles directory"
  (concat me/dotfiles path))

(defun actually-kill-this-buffer ()
  "Kills the current buffer, unlike ~kill-this-buffer~ which does
not always do that."
  (interactive)
  (kill-buffer (current-buffer)))

(defun answer-yes ()
  (interactive)
  (kbd "y"))

(defun answer-no ()
  (interactive)
  (kbd "n"))

(defun windows-right ()
  (interactive)
  (if (> (length (window-list)) 1)
      (evil-window-move-far-right)
    (progn
      (split-window-right)
      (evil-window-move-far-right)
      (other-window 1)
      (next-buffer)
      (other-window 1))))

(defun windows-left ()
  (interactive)
  (if (> (length (window-list)) 1)
      (funcall-interactively
       #'evil-window-move-far-left)
    (progn
      (split-window-right)
      (other-window 1)
      (next-buffer)
      (other-window 1))))

(let ((secret.el (expand-file-name ".secrets.el" user-emacs-directory)))
  (when (file-exists-p secret.el)
    (load secret.el)))

;; Directory containing my dotfiles.
(defconst me/dotfiles "~/.dotfiles/")
;; directory containing my nixos configutation
(defconst me/nixos-directory "/sudo::/etc/nixos/")

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

(setq use-package-always-ensure t)

(use-package quelpa)

(setq quelpa-upgrade-p nil)

(use-package quelpa-use-package)

(quelpa-use-package-activate-advice)

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

(use-package evil-collection
  :init
  (setq evil-want-keybinding nil)
  :after evil
  :config
  ;; (setq evil-collection-term-sync-state-and-mode-p nil)
  (evil-collection-minibuffer-setup)
  (evil-collection-init))

(use-package evil-easymotion
  :config
  (evilem-default-keybindings "SPC"))

(use-package "evil-surround"
  :config
  (global-evil-surround-mode 1))

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

(use-package general)

(general-evil-setup)

(use-package hydra)

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

(cl-eval-when (compile load eval)
 (defconst leader-key ",")
 (defconst alt-leader "SPC"))

(general-create-definer leader-key-def
  :prefix leader-key)

(general-create-definer alt-leader-key-def
  :prefix alt-leader-key)

(cl-defun leader-prefix (str &optional (prefix leader-key))
  "Append a leader key to the given string"
  (concat prefix " " str))

(cl-defmacro define--subleader (key name general-definer-name &key (leader leader-key))
  "Both create a general definer, and a which-key replacement for the given subleader."
  `(progn
     (which-key-add-key-based-replacements
       (leader-prefix ,key ,leader) ,name)
     (general-create-definer ,general-definer-name
       :prefix (leader-prefix ,key ,leader))))

;; the format for the input of this function is inspired by general's
;; easy to use functions.
(cl-defmacro define-subleader (&rest args &key (leader leader-key) &allow-other-keys)
  "Both create a general definer, and a which-key replacement for
the given subleader. Accepts arguments in threes with no
delimiter."
  `(progn ,@(mapcar (lambda (elt)
                      `(define--subleader ,@elt :leader ,leader))
                    (seq-partition (remove-keyword-args args) 3))))

(define-subleader
  "e" "eval" eval-key-def
  "s" "start" start-key-def
  "x" "xpand" xpand-key-def
  "p" "project" project-key-def)

(leader-key-def 'normal
  "q" 'actually-kill-this-buffer              ; ",q" to kill buffer not window.
  "w" 'evil-delete-buffer              ; ",w" to kill buffer and window. equivalent of :bd<cr>.
  "b" 'helm-mini                     ; ",b" to switch buffers.
  "f" 'helm-find-files               ; ",f" to find file (replace :e)
  "d" 'fzf-directory-from-home
  "h" 'help
  "o" 'occur 
  "i" 'imenu
  "RET" (kbd ":noh"))

(leader-key-def 'visual
  "c" 'comment-or-uncomment-region)

(defun nixconf () (interactive) (find-file (concat me/nixos-directory "configuration.nix")))

(defun econf () (interactive) (find-file (dotfiles "emacs/emacs.org")))

(setq inhibit-splash-screen t
      initial-scratch-message nil)

(setq initial-major-mode 'emacs-lisp-mode)(setq initial-major-mode 'emacs-lisp-mode)

(use-package delight :quelpa (:stable t)
  :config
  (delight 'eldoc-mode nil "eldoc"))

(use-package helm
  :delight
  :bind (("M-x" . helm-M-x))
  :config
  (helm-mode t)

  ;; Do not create a new frame with helm completion.
  (setq helm-show-completion-display-function
        #'helm-show-completion-default-display-function))

(add-hook 'prog-mode-hook #'hs-minor-mode)
(delight 'hs-minor-mode nil "hideshow")

(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(add-to-list 'default-frame-alist
             '(vertical-scroll-bars . nil))

(global-display-line-numbers-mode 1)
(display-line-numbers-mode 1)

(setq column-number-mode t)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(fset 'yes-or-no-p 'y-or-n-p)

(setq select-enable-clipboard nil)

(electric-pair-mode nil)

(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis)

(define-advice show-paren-function (:around (fn) fix)
  "Highlight enclosing parens."
  (cond ((looking-at-p "\\s(") (funcall fn))
        (t (save-excursion
             (ignore-errors (backward-up-list))
             (funcall fn)))))

(use-package which-key
  :delight
  :config
  (which-key-mode t))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Use atool for compression and extraction
(use-package dired-atool
  :config
  (leader-key-def normal dired-mode-map
    "z" #'dired-atool-do-unpack
    "Z" #'dired-atool-do-pack))

(require 'eshell)
(require 'em-smart)

(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)
;;(add-to-list 'eshell-visual-commands "rlwrap")

;; Eshell modules
(require 'esh-module)
(add-to-list 'eshell-modules-list 'eshell-tramp)

(setq password-cache t) ; enable password caching
(setq password-cache-expiry 3600) ; for one hour (time in secs)

(defun eshell/e (args)
  "Open the given files"
  (if (listp args)
      (dolist (file args) (find-file file t))
    (find-file args)))

(defun eshell/ee (args)
  "Open the given files in new windows"
  (if (listp args)
      (dolist (file args) (find-file file t))
    (find-file-other-window args)))

(defun eshell/fd (&optional from-directory)
  "Run fzf to open a directory in dired"
  (fzf-directory-from (if from-directory
                          from-directory
                        default-directory)))

(defun eshell/fh ()
  (eshell/fd "~"))

(defun eshell/econf () (econf))
(defun eshell/nixconf () (nixconf))

(global-set-key [f1] 'eshell)
(global-set-key [f2] 'eshell-new)
;; Let me use C-j/k
(add-hook 'eshell-mode-hook ; needs to be in a hook because eshell is dumb
          (lambda ()
            (general-define-key :states 'normal :keymaps 'eshell-mode-map
                                "C-j" 'evil-window-down
                                "C-k" 'evil-window-up)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell-cmpl-initialize)
            (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)))

(add-hook 'eshell-mode-hook (lambda () (company-mode -1)))

(defun eshell-new()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N))

(defun protect-eshell-prompt ()
  (let ((inhibit-field-text-motion t)
        (inhibit-read-only t))
    (add-text-properties
     (point-at-bol)
     (point)
     '(rear-nonsticky t
                      inhibit-line-move-fiold-capture t
                      field output
                      read-only t
                      front-sticky (field inhibit-line-move-field-capture)))))

(add-hook 'eshell-after-prompt-hook 'protect-eshell-prompt)

(setq comint-prompt-read-only t ; Don't let me delete the comint prompt duh
      comint-move-point-for-output nil ; reduce frequent redisplays
      comint-scroll-show-maximum-output nil)

(general-define-key :states 'normal :keymaps 'shell-mode-map
   "C-j" 'evil-window-down
   "C-k" 'evil-window-up)

(push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)

(general-define-key :states 'insert
                    :keymaps 'term-raw-map
                    "C-g" 'term-send-esc)

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

(defun connect-to-serenity ()
  (interactive)
  (dired me/serenity-path))

;; easily restart emacs daemon
(use-package restart-emacs)

;; focus any new frames
(add-to-list 'after-make-frame-functions 'select-frame-set-input-focus)

(cl-defun make-daemon-frame (socket-name &rest args)
  "Make a new emacs frame for the daemon with the given socket name."
  (interactive "M" "Socket name: ")
  (apply 'start-process
         (concat socket-name "-frame")
         nil
         "emacsclient" "--create-frame" (concat "--socket-name=" socket-name)
         args))

(cl-defun make-daemon (socket-name &key (create-buffer t) before after (theme 'doom-nord-light))
  "Make a new emacs daemon with the given socket name."
  (interactive "M" "Socket name: ")
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

(cl-defun nix-daemon-running-p (&optional (socket "server"))
  "Check if a daemon which was started from nix-shell is running
on the given socket. Default unnamed socket."
  ;; nix-shell starts daemosn in /run/user/
  (interactive)
  (let ((running? (file-exists-p (concat "/run/user/1000/emacs1000/" socket))))
    (when (interactive-p) (message (if running? "yes" "no")))
    running?))

(cl-defun non-nix-daemon-running-p (&optional (socket "server"))
  "Check if a daemon which was NOT started from nix-shell is running
on the given socket. Default unnamed socket."
  ;; daemons started outside of nix-shell exist in /tmp/
  (interactive)
  (let ((running? (file-exists-p (concat "/tmp/emacs1000/" socket))))
    (when (interactive-p) (message (if running? "yes" "no")))
    running?))

(cl-defun nix-daemon (&optional (theme 'doom-nord-light))
  "Start a daemon and frame in the current nix project."
  (interactive)
  (if (nix-current-sandbox)
      (let* ((default-directory (file-name-directory (nix-current-sandbox)))
             (socket-name (final-directory-name default-directory))
             (daemon-name (concat socket-name "-daemon")))
        (if (nix-daemon-running-p socket-name) 
            (nix-daemon-frame)
          (message "Loading inferior nix emacs")
          (start-process-shell-command
           daemon-name daemon-name
           (concat "nix-shell --command \""
                     "emacs --daemon=" socket-name
                     " --execute \\\"
                       (load-theme '"
                       (symbol-name theme)
                       " t)\\\""
                     "; "
                     "emacsclient --create-frame "
                     (concat "--socket-name=" socket-name)
                   "; "
                   "return"
                   "\""))))
    (error "No nix environment was found")))

(defun nix-daemon-frame ()
  "Start a frame from the relevant nix Emacs daemon in the current nix project."
  (interactive)
  (if (nix-current-sandbox)
      (let ((default-directory (file-name-directory (nix-current-sandbox)))
            (socket-name (elt (nreverse (split-string default-directory "/")) 1)))
        (unless (nix-daemon-running-p socket-name)
          (error "The daemon is not active"))
        (start-process-shell-command
         (concat socket-name "-frame") nil
         (concat "nix-shell --command "
                 (concat "\"emacsclient --create-frame --socket-name=" socket-name "\""))))
    (error "No nix environment was found")))

(use-package doom-themes
  :config
  ;; flash mode line when emacs bell rings
  (doom-themes-visual-bell-config))

(defun disable-all-themes ()
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes))

(if (display-graphic-p)
    (load-theme 'doom-one-light t)
  (disable-all-themes))

(defun disable-most-recent-theme ()
  (interactive)
  (disable-theme (first custom-enabled-themes)))

(defun switch-theme (theme)
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                 (mapcar 'symbol-name
                     (custom-available-themes))))))
  (disable-most-recent-theme)
  (load-theme theme t))

(setq-default tab-width 4
              indent-tabs-mode nil)

(winner-mode 1)

(defhydra hydra-winner (global-map "C-c")
  "Window configuration history"
  ("u" winner-undo)
  ("r" winner-redo))

(use-package projectile
  :delight "P"
  :after general
  :config
  (general-define-key
   :states 'normal
   :keymaps 'override
 "C-p" 'helm-projectile-find-file)
  (project-key-def 'normal 'projectile-mode-map
    "p" 'projectile-switch-project)
  (projectile-mode +1))

;; use helm for projectile
(use-package helm-projectile
  :config
  (helm-projectile-on))

(use-package skeletor
  :config
  (setq skeletor-project-directory "~/code/") ; by default, put the
                                        ; project in the ~/code
                                        ; directory.
  (setq skeletor-user-directory (dotfiles "emacs/skeletor/"))

  (defun skeletor-create-project-here ()
    "Create a skeletor project in the current directory."
    (interactive)
    (let ((skeletor-project-directory default-directory))
      (call-interactively 'skeletor-create-project)))

  ;; global substitutions
  (add-to-list 'skeletor-global-substitutions
               '("__AUTHOR__" . "Adrian Fullmer"))

  (defun setup--lorri (dir)
    (let ((default-directory dir))
      (skeletor-shell-command "direnv allow")
      (projectile-lorri-watch)))

  ;; Custom project types
  (skeletor-define-template "generic"
    :title "Generic Project"
    :substitutions
    '(("__PACKAGES__" . (lambda () (read-string "Packages to use: "))))
    :after-creation setup--lorri)

  (skeletor-define-template "common-lisp"
    :title "Common Lisp Project"
    :default-license (rx bol "mit")
    :substitutions
    '(("__DESCRIPTION__" . (lambda () (read-string "Description: "))))
    :after-creation setup--lorri)

  (skeletor-define-template "python"
    :title "Python Library"
    :default-license (rx bol "mit"j)
    :substitutions
    '(("__DESCRIPTION__" . (lambda () (read-string "Description: "))))
    :after-creation setup--lorri)

  ;;keybindings
  (project-key-def 'normal
    "s" 'skeletor-create-project-here))

(use-package fzf)

(defun fzf-directory-from-home () (interactive)
       (fzf/start "~/" "find ${1:-.} -path '*/\\.*' -prune \ -o -type d -print 2> /dev/null"))

(defun fzf-directory-from (directory) (interactive "D")
       (fzf/start directory "find ${1:-.} -path '*/\\.*' -prune \ -o -type d -print 2> /dev/null"))

(setq vc-follow-symlinks t)

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

(use-package flycheck
  :config
  (setq flycheck-global-modes '(not c-mode c++-mode)))

(use-package magit
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package evil-magit)

(leader-key-def 'normal
  "m" 'magit)

(use-package yasnippet
  :delight yas-minor-mode
  :config
  (yas-global-mode t)
  (setq yas-snippet-dirs
        (list (dotfiles "emacs/snippets") yasnippet-snippets-dir))
  (yas-reload-all))

(use-package yasnippet-snippets
  :quelpa (:stable t))

;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

(use-package lsp-mode
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil))

(use-package lsp-ui :commands lsp-ui-mode) ; adds flycheck support
(use-package company-lsp :commands company-lsp) ; links with company
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(cl-defun compile-rec (&key (filename "Makefile") (command "make -k"))
  "Traveling up the path, find a Makefile and `compile'."
  (interactive)
  (let ((makefile-dir (locate-dominating-file default-directory filename)))
    (when makefile-dir
      (with-temp-buffer
        (cd makefile-dir)
        (compile command)))))

(global-set-key [f3] (lambda () (interactive) (manual-entry (current-word))))

(use-package direnv
  :delight
  :after projectile ; I integrate projectile with direnv here
  :config
  (direnv-mode)

  ;; Keybindings to direnv refresh and lorri watch

  (cl-defun projectile-lorri-watch (&optional (project-directory (projectile-project-root)))
    "Begin an inferior process to watch the current projectile
project with lorri."
    (interactive)
    (let* ((project-name (final-directory-name project-directory))
           (process-name (concat "Lorri [" project-name "]"))
           (default-directory project-directory))
      (if (file-exists-p "shell.nix")
          (if (not (get-process process-name))
              (progn
                ;; (start-process-shell-command
                ;;  (concat "direnv-" process-name) nil
                ;;  "direnv-allow")
                (start-process-shell-command
                 process-name (earmuffs process-name)
                 "lorri watch")
                (message (concat "Lorri watching " project-name)))
            (error (concat "Lorri is already watching " project-name)))
        (error (concat "There is no shell.nix for " project-name)))))

  (project-key-def 'normal
    "d" 'direnv-update-directory-environment
    "l" 'projectile-lorri-watch)

  ;; Lorri watch the given project when switching to a new project.
  (add-hook 'projectile-after-switch-project-hook
            (lambda () (ignore-errors (projectile-lorri-watch))))

  ;; Non-file modes which should also be synched with direnv
  (defmacro add-direnv-non-file-modes (&rest body)
    `(mapcar (lambda (mode) (add-to-list 'direnv-non-file-modes mode)) (list ,@body)))

  (add-direnv-non-file-modes
   'sly-mode
   'eshell-mode
   'comint-mode
   'term-mode
   'prolog-mode
   'inferior-python-mode
   'haskell-mode))

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

(use-package rainbow-delimiters
  :delight)

(eval-key-def 'normal emacs-lisp-mode-map
  "b" 'eval-buffer
  "f" 'eval-defun)
(eval-key-def 'visual emacs-lisp-mode-map
  "r" 'eval-region)

(use-package sly
  ;; :quelpa (:stable t)
  ;; :load-path "~/code/elisp/sly"
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
  (add-to-list 'evil-emacs-state-modes 'sly-db-mode)
  (add-to-list 'helm-completing-read-handlers-alist
               '(sly-read-symbol-name . nil))
  ;; Avoid using helm when bugget at sly-read-symbol-name functions
  (add-to-list 'helm-completing-read-handlers-alist
               '(sly-describe-symbol . nil)
               '(sly-describe-function . nil)))

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

(use-package clojure-mode)

(use-package cider
  :config
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion))

;; some visual flare
(use-package spinner :quelpa (:stable t))

(leader-key-def 'normal clojure-mode-map
  "s" 'cider-jack-in
  "z" 'cider-switch-to-repl-buffer
  "a" 'cider-close-ancillary-buffers)

(eval-key-def 'normal clojure-mode-map
  "b" 'cider-eval-buffer
  "f" 'cider-eval-defun-at-point)

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

(add-hook 'python-mode-hook #'lsp)

(leader-key-def 'normal python-mode-map
  "z" 'python-shell-switch-to-shell)

(start-key-def 'normal python-mode-map
  "s" 'run-python)

(eval-key-def 'normal python-mode-map
  "b" 'python-shell-send-buffer
  "f" 'python-shell-send-defun)

(eval-key-def 'visual 'python-mode-map
  "r" 'python-shell-send-region)

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

(leader-key-def 'normal prolog-mode-map
  "z" 'switch-to-prolog
  "c" 'prolog-compile-buffer)

(start-key-def 'normal prolog-mode-map
  "s" 'run-swi-prolog)

(eval-key-def 'normal prolog-mode-map
  "b" 'prolog-consult-buffer
  "f" 'prolog-consult-predicate)

(eval-key-def 'visual 'prolog-mode-map
  "r" 'prolog-consult-region)

(defun run-swi-prolog ()
  (interactive)
  (let ((prolog-program-name "swipl"))
    (call-interactively 'run-prolog)))

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

(add-hook 'c++-mode-hook 'flycheck-mode)

(leader-key-def 'normal c-mode-base-map
  "s" 'projectile-find-other-file
  "c" 'compile-rec
  "r" '(lambda () (interactive) (compile-rec :command "make run")))

(setq-default c-basic-offset 4
              c-default-style "linux")

(use-package julia-repl
  :config
  (add-hook 'julia-mode-hook 'julia-repl-mode))

(use-package haskell-mode
  :config
  ;; allows capf and dabbrev backends while using haskell
  (add-hook 'haskell-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   (append '((company-capf company-dabbrev-code))
                           company-backends)))))

(use-package flycheck-elm
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-elm-setup))
  
(use-package elm-mode)

(use-package org
  :delight org-indent-mode
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
        org-fontify-quote-and-verse-blocks t
        org-src-ask-before-returning-to-edit-buffer nil)
  (general-define-key :states 'normal :mode 'org-mode-map
                      "C-`" 'org-edit-special)

                      ;; "C-j" 'evil-window-down
                      ;; "C-k" 'evil-window-up
  ;; (define-key org-mode-map (kbd "<C-j>") nil)
  ;; (define-key org-mode-map (kbd "<C-k>") nil)
  (leader-key-def 'normal org-src-mode-map
    "w" 'org-edit-src-exit)
  (add-hook 'org-mode-hook (lambda ()
                             (general-define-key :states 'normal :mode 'org-mode-map
                                                 "C-j" 'evil-window-down
                                                 "C-k" 'evil-window-up))))

(defhydra hydra-org-babel-source-block-jump (org-mode-map "C-c C-v")
    "Jump between org babel source blocks"
    ("n" org-babel-next-src-block)
    ("p" org-babel-previous-src-block))

(use-package toc-org
  :config
  (add-hook 'org-mode-hook 'toc-org-mode))

(use-package company-nixos-options
  :hook (nix-mode-hook . (lambda () (add-to-list 'company-backends 'company-nixos-options))))

(use-package nix-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
  (add-hook 'nix-mode-hook
            (lambda ()
              (setq tab-always-indent nil)
              (setq indent-tabs-mode t))))

(auto-mode-add 'shell-script-mode
               ".profile\\'"
               ".bash_aliases\\'"
               ".inputrc\\'")

(auto-mode-add 'common-lisp-mode
     ".sbclrc\\'"
     ".lisprc\\'"
     ".otherlisprc\\'")

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
  (general-define-key :keymaps 'exwm-mode-map
                      "C-c C-f" 'exwm-layout-toggle-fullscreen
                      "C-c C-l" 'exwm-floating-toggle-floating)

  (setq exwm-input-global-keys
        `(
          ;; Bind "s-r" to exit char-mode and fullscreen mode.
          ([?\s-r] . exwm-reset)
          ;; Bind "s-tab" to toggle line vs char mode.
          ([s-tab] . exwm-input-toggle-keyboard)
          ;; Bind "s-w" to switch workspace interactively.
          ([?\s-`] . exwm-workspace-switch)
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
          ;; bind "s-[direction] to switch windows"
          ([?\s-h] . windmove-left)
          ([?\s-j] . windmove-down)
          ([?\s-k] . windmove-up)
          ([?\s-l] . windmove-right)

          ;; bind "s-arrow" to move, "maximize" or "minimize" a window
          ([s-right] . windows-right)
          ([s-down] . evil-quit)
          ([s-up] . delete-other-windows)
          ([s-left] . windows-left)
          ;; bind "s-[v and s] to split vertical and horizontal"
          ([?\s-v] . evil-window-vsplit)
          ([?\s-s] . evil-window-split)
          ;; bind "s-z" to work like a global "M-x"
          ([?\s-z] . helm-M-x)
          ;; bind "s-;" to work loke a global ":"
          ([?\s-\;] . evil-ex)
          ;; bind "s-b" to buffer switching
          ([?\s-b] . helm-mini)
          ;; bind "s-q" to kill this buffer, closing the current program.
          ([?\s-q] . actually-kill-this-buffer)
          ([?\s-w] . evil-delete-buffer)
          ;; bind "s-g" to keyboard-quit
          ([?\s-g] . keyboard-quit)

          ;; Bind "s-<f2>" to "slock", a simple X display locker.
          ;; ([s-f2] . (lambda ()
          ;;             (interactive)
          ;;             (start-process "" nil "/usr/bin/slock")))

          ;; bind yes or no answers to s y and n
          ([?\s-y] . answer-yes)
          ([?\s-n] . answer-no)))
  ;; start in char mode by default
  (setq exwm-manage-configurations '((t char-mode t)))

  ;; sys tray
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  ;; set prefix keys
  (setq my-exwm-prefix-keys (list
                             ?\: ; evil ex mode
                             ?\, ; leader key
                             ?\M-x ; M-x
                            ?\C-w)) ; window options

  (setq exwm-input-prefix-keys (nconc exwm-input-prefix-keys my-exwm-prefix-keys))

  ;; Allow pulling a window between workspaces by switching to its buffer
  ;; to see x windows on other workspaces, do C-c a
  (setq exwm-workspace-show-all-buffers t)
  (setq exwm-layout-show-all-buffers t)


  ;; fix global-display-line-numbers-mode bug regarding the exwm
  ;; "frame" not having the 'client parameter
  (defun display-line-numbers--turn-on ()
    "Turn on `display-line-numbers-mode'."
    (unless (or (minibufferp)
                ;; taken from linum.el
                nil
                ;; (and (daemonp) (null (frame-parameter nil 'client)))
                )
      (display-line-numbers-mode))))

(use-package symon)

(use-package helm-exwm)

(use-package desktop-environment
  :config
  (desktop-environment-mode))

(use-package exwm-firefox-core)

(use-package exwm-firefox-evil)

;; logout function
(defun logout ()
  (interactive)
  (recentf-save-list)
  (save-some-buffers)
  (start-process-shell-command "logout" nil "lxsession-logout"))

(defun bluetooth ()
  (interactive)
  (split-window-vertically)
  (other-window 1)
  (start-process-shell-command "blueman-manager" nil "blueman-manager"))

(setq make-backup-files nil)

(save-place-mode 1)

(defun auto-mode-add (mode &rest filenames)
  "Add a number of file name patterns to the given mode in
auto-mode-alist"
  (mapcar (lambda (name)
            (add-to-list 'auto-mode-alist
                         `(,name . ,mode)))
          filenames))

(defun remove-keyword-args (list)
  "Remove keyword arguments from the given list"
  (car (general--remove-keyword-args list)))

(defun earmuffs (string)
  "Add *earmuffs* to the given string. This represents the name
  of a buffer which is not associated with a file."
  (concat "*" string "*"))

(defun final-directory-name (path)
  "Return the name of the final directory of the given path"
  (elt (nreverse (split-string path "/")) 1))

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

(defun reboot ()
  (interactive)
  (shell-command "reboot"))

(defun dotfiles (path)
  "Return path relative to the dotfiles directory"
  (concat me/dotfiles path))

(defun actually-kill-this-buffer ()
  "Kills the current buffer, unlike ~kill-this-buffer~ which does
not always do that."
  (interactive)
  (kill-buffer (current-buffer)))

(defun answer-yes ()
  (interactive)
  (kbd "y"))

(defun answer-no ()
  (interactive)
  (kbd "n"))

(defun windows-right ()
  (interactive)
  (if (> (length (window-list)) 1)
      (evil-window-move-far-right)
    (progn
      (split-window-right)
      (evil-window-move-far-right)
      (other-window 1)
      (next-buffer)
      (other-window 1))))

(defun windows-left ()
  (interactive)
  (if (> (length (window-list)) 1)
      (funcall-interactively
       #'evil-window-move-far-left)
    (progn
      (split-window-right)
      (other-window 1)
      (next-buffer)
      (other-window 1))))
