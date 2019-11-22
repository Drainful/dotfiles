(use-modules (gnu)
             (gnu packages emacs)
             (gnu packages emacs-xyz)
             (gnu packages shells)
             ;; (gnu packages mpd)
             ;; emms
             (gnu packages audio)
             (gnu packages xiph)
             (gnu packages mp3)
             (gnu packages rsync)
             (lib))

;; (use-service-modules audio)

(os-part '()
         (list
          fish
          ;; mpd-mpc
          ;;; emms
          opus-tools
          mp3info
          cuetools

          rsync

          emacs
          )
         (list ;; (service mpd-service-type
          ;;          (mpd-configuration
          ;;           (user "adrian")))
          ))

;; emacs-evil
;; emacs-evil-collection
;; emacs-evil-surround
;; ;; emacs-evil-snipe
;; emacs-general
;; emacs-hydra
;; ;; emacs-delight
;; emacs-minibuffer-line
;; emacs-helm
;; emacs-helm-ag
;; emacs-which-key
;; emacs-iedit
;; ;; emacs-evil-iedit-state
;; ;; emacs-diredfl
;; ;; emacs-dired-atool
;; emacs-dired-du
;; emacs-dired-hacks
;; emacs-bash-completion
;; emacs-fish-completion
;; emacs-multi-term
;; emacs-doom-themes
;; emacs-projectile
;; emacs-helm-projectile
;; emacs-skeletor
;; emacs-aggressive-indent
;; emacs-company
;; emacs-flycheck
;; emacs-magit
;; emacs-evil-magit
;; emacs-lsp-mode
;; emacs-lsp-ui
;; emacs-company-lsp
;; emacs-helm-lsp
;; emacs-direnv
;; emacs-lispyville
;; emacs-rainbow-delimiters
;; emacs-sly
;; emacs-clojure-mode
;; emacs-cider
;; emacs-geiser
;; emacs-lua-mode
;; ;; emacs-shen-mode
;; emacs-rust-mode
;; ;; emacs-julia-repl
;; ;; emacs-omnisharp
;; emacs-haskell-mode
;; ;; emacs-indium
;; emacs-js2-mode
;; emacs-web-mode
;; emacs-emmet-mode
;; ;; emacs-toc-org
;; emacs-guix
;; ;; emacs-openwith
;; emacs-symon
;; ;; emacs-helm-exwm
;; emacs-desktop-environment
;; ;; emacs-pdf-tools ; fails to build
;; emacs-emms
;; emacs-helm-emms
;; ;; emacs-system-packages
;; emacs-helm-system-packages
;; emacs-znc
;; emacs-wttrin
