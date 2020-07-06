(define-module (os-modules emacs)
  #:use-module (lib)
  #:use-module (my-packages)
  #:use-module ((gnu packages emacs)
                #:select ((emacs . emacs-package)))
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages emacs-xyz)

  #:use-module (gnu packages gnupg))

;; TODO write macro to prepend emacs- to each symbol
(define-public emacs
  (os-module
   #:packages
   (list emacs-package
         ;; spell check
         aspell aspell-dict-en
         ;; commented out emacs packages are installed via package.el
         emacs-use-package
         emacs-evil
         emacs-evil-commentary
         emacs-evil-collection
         emacs-evil-surround
         emacs-god-mode
         emacs-general
         emacs-hydra
         emacs-delight
         emacs-minibuffer-line
         emacs-helm
         emacs-helm-ag
         emacs-which-key
         emacs-iedit
         ;; emacs-evil-iedit-state
         emacs-dired-rsync rsync
         ;; emacs-diredfl
         ;; emacs-dired-atool
         emacs-dired-du
         emacs-dired-hacks
         emacs-bash-completion
         emacs-fish-completion
         emacs-multi-term
         ;; my-emacs-doom-themes 
         emacs-doom-themes
         emacs-projectile
         emacs-helm-projectile
         emacs-skeletor
         emacs-aggressive-indent
         emacs-company
         ;; emacs-smart-tab
         emacs-flycheck
         emacs-magit
         emacs-evil-magit
         emacs-lsp-mode
         emacs-lsp-ui
         emacs-company-lsp
         emacs-helm-lsp
         ;; emacs-direnv
         emacs-lispyville
         emacs-rainbow-delimiters
         ;; emacs-nameless
         emacs-sly
         emacs-clojure-mode
         emacs-cider
         emacs-spinner
         emacs-geiser
         emacs-lua-mode
         ;; emacs-shen-mode
         emacs-rust-mode
         emacs-julia-mode
         ;; emacs-omnisharp
         emacs-haskell-mode
         ;; emacs-indium
         emacs-js2-mode
         emacs-web-mode
         emacs-emmet-mode
         ;; emacs-toc-org
         ;; emacs-elfeed-org
         emacs-org
         my-emacs-org-roam
         my-emacs-company-org-roam
         emacs-lsp-java
         (let ((file "/home/guix/adrian/Code/guix-packages/emacs-guix/guix.scm"))
           (if (stat file #f)
               (load file)
               (load emacs-guix)))
         emacs-nix-mode
         emacs-openwith
         emacs-erc-hl-nicks 
         ;; emacs-circe ; not used
         emacs-olivetti
         emacs-adaptive-wrap
         emacs-symon
         ;; emacs-helm-exwm ; installed in exwm.scm
         emacs-pulseaudio-control
         emacs-desktop-environment
         my-emacs-nov-el
         emacs-emms
         emacs-helm-emms
         ;; emacs-system-packages
         emacs-helm-system-packages
         emacs-znc
         ;; emacs-erc-image ; bugged build?
         ;; emacs-elfeed
         emacs-wttrin
         ;; emacs-bluetooth
         emacs-transmission

         emacs-pinentry
         emacs-pass
         emacs-helm-pass
         emacs-auth-source-pass)))
