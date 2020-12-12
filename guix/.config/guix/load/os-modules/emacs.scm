(define-module (os-modules emacs)

  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages package-management)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)

  #:use-module (lib)
  #:use-module (gnu packages)
  #:use-module (my-packages))

;; TODO write macro to prepend emacs- to each symbol
(define-public emacs
  (os-module
   #:packages
   (cons*
    my-emacs-god-mode
    my-emacs-org-roam
    my-emacs-company-org-roam
    my-emacs-nov-el
    
    (package
      (inherit emacs-guix)
      (name "emacs-guix-fixed")
      (inputs
       `(("guile" ,guile-3.0-latest)
         ("guix" ,guix)))
      (propagated-inputs
       `(("geiser" ,(package
                      (inherit emacs-geiser)
                      (name "emacs-geiser")
                      (version "0.11.2")
                      (source
                       (origin
                         (method git-fetch)
                         (uri (git-reference
	                       (url "https://gitlab.com/jaor/geiser/")
	                       (commit version)))
                         (file-name (git-file-name name version))
                         (sha256
                          (base32 "1khi1bghsjx6cs5acizmlbw9z19s4qycnji9krdbn42cbpv0rysv"))))
                      (inputs
                       `(("guile" ,guile-3.0-latest)))))
         ("guile-gcrypt" ,guile-gcrypt)
         ("dash" ,emacs-dash)
         ("bui" ,emacs-bui)
         ("edit-indirect" ,emacs-edit-indirect)
         ("magit-popup" ,emacs-magit-popup))))
    ;; (let ((file "/home/guix/adrian/Code/guix-packages/emacs-guix/guix.scm"))
    ;;   (if (stat file #f)
    ;;       (load file)
    ;;       (specification->package "emacs-guix")))
    (pkgs emacs
          ;; emacs-guix
          ;; spell check
          aspell aspell-dict-en
          ;; commented out emacs packages are installed via package.el
          emacs-use-package
          emacs-evil
          emacs-evil-commentary
          emacs-evil-collection
          emacs-evil-surround
          emacs-general
          emacs-hydra
          emacs-delight
          emacs-minibuffer-line
          emacs-helm
          emacs-helm-ag
          emacs-helm-company
          emacs-which-key
          emacs-all-the-icons
          emacs-iedit
          ;; emacs-evil-iedit-state
          emacs-dired-rsync rsync
          ;; emacs-diredfl
          ;; emacs-dired-atool
          emacs-dired-du
          emacs-dired-hacks
          emacs-all-the-icons-dired
          emacs-helm-fish-completion
          emacs-vterm
          emacs-multi-term
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
          emacs-helm-sly
          emacs-clojure-mode
          emacs-cider
          emacs-spinner
          ;; emacs-geiser
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
          emacs-elfeed
          emacs-org
          ;;;emacs-lsp-java
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
          emacs-pdf-tools
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
          emacs-auth-source-pass))))
