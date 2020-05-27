(define-module (my-packages)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages messaging)
  #:use-module (games packages minecraft)

  #:use-module (guix build-system cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages java)
  #:use-module (guix packages)
  #:use-module (guix git-download)

  #:use-module (ice-9 match))

(define-public my-emacs-guix
  (package
    (inherit emacs-guix)
    (version "0.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://emacs-guix.gitlab.io/website/"
                                  "releases/emacs-guix-" version ".tar.gz"))
              (sha256
               (base32
                "0yz64c0z4ygi2k4af18k4r1ncgys18jb8icywkp2g5pgmpn5l7ps"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Add support for Guile 3.0.  Remove for versions > 0.5.2.
                  (substitute* "configure"
                    (("\"2\\.2 2\\.0\"")
                     "\"3.0 2.2 2.0\""))
                  #t))))
    ))

;; (define-public my-emacs-doom-themes
;;   (let ((commit "655685d3bdc5322e68581dd306b09fcecafbc912")
;;         (revision "5")
;;         (version "2.1.6"))
;;     (package
;;       (inherit emacs-doom-themes)
;;       (version (git-version version revision))
;;       (source (origin
;;                 (method git-fetch)
;;                 (uri (git-reference
;;                       (url "https://github.com/hlissner/emacs-doom-themes.git")
;;                       (commit commit)))
;;                 (file-name (git-file-name name version))
;;                 (sha256
;;                  (base32 "1jwdjq4q2gkhi6jwas3ywgmdz5dg14sfb3fzhqd7wih6j3i2l3cr")))))))

(define-public my-multimc
  (package
    (inherit multimc)
    (version "0.6.11")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/MultiMC/MultiMC5.git")
                    (recursive? #t)
                    (commit version)))
              (file-name (git-file-name (package-name multimc) version))
              (sha256
               (base32
                "1jkbmb4sgfk8d93f5l1vd9pkpvhq9sxacc61w0rvf5xmz0wnszmz"))))))

(define-public my-emacspeak
  (package
    (inherit emacspeak)
    (version "49.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/tvraman/emacspeak/releases/download/"
             version "/emacspeak-" version ".tar.bz2"))
       (sha256
        (base32
         "0qsj7rzfyqmyidfsjrhjnxi2d43axx6r3gac1fhv5xkkbiiqzqkb"))))))

(define-public my-emacs-nov-el
  (let* ((commit "cd1b264b3f978a9285fa9297e40ad29d1434adf5")
         (version (git-version "0.2.9" "0" commit)))
    (package
      (inherit emacs-nov-el)
      (name "emacs-nov-el")
      (version version)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/wasamasa/nov.el.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "02apbr5x9vdrx05mfs8qyp4ami1y30n8mmy4kpd4xw04nza9wxki")))))))

(define-public my-bitlbee-discord
  (package
    (inherit bitlbee-discord)
    (name "my-bitlbee-discord")
    (version "20200216")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sm00th/bitlbee-discord.git")
             (commit "3061edd283b4e886384e5e8cad10f92dc45f3ee7")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1d6nkr7wfrhra09ql258hvhr6q8kmnigcr14hjbwk10kqcb277y6"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-autogen
           (lambda _
             (let ((sh (which "sh")))
               (substitute* "autogen.sh" (("/bin/sh") sh))
               (setenv "CONFIG_SHELL" sh))
             #t))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (invoke "./configure"
                     (string-append
                      "--with-bdatadir="
                      (assoc-ref outputs "out")
                      "/share/bitlbee/")
                     (string-append
                      "--with-plugindir="
                      (assoc-ref outputs "out")
                      "/lib/bitlbee/")))))))
    (synopsis "Discord plugin for Bitlbee")))

(define-public my-emacs-exwm
  (match (package-arguments emacs-exwm)
    ((#:emacs e #:phases p)
     (package
       (inherit emacs-exwm)
       (name "my-emacs-exwm")
       (synopsis "Exwm X window manager modified to launch a
daemon and client.")
       (arguments
        `(#:emacs ,e
          #:phases
          (modify-phases ,p
            (replace 'install-xsession
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (xsessions (string-append out "/share/xsessions"))
                       (bin (string-append out "/bin"))
                       (exwm-executable (string-append bin "/exwm")))
                  ;; Add a .desktop file to xsessions
                  (mkdir-p xsessions)
                  (mkdir-p bin)
                  (with-output-to-file
                      (string-append xsessions "/exwm.desktop")
                    (lambda _
                      (format #t "[Desktop Entry]~@
                     Name=~a~@
                     Comment=~a~@
                     Exec=~a~@
                     TryExec=~:*~a~@
                     Type=Application~%" ,name ,synopsis exwm-executable)))
                  ;; Add a shell wrapper to bin
                  (with-output-to-file exwm-executable
                    (lambda _
                      (format #t "#!~a ~@
                     ~a +SI:localuser:$USER ~@
                     [ -f \"$HOME/.xinitrc\" ] && . \"$HOME/.xinitrc\" ~@
                     ~a --daemon ~@
                     exec ~a --exit-with-session ~a --create-frame --eval '~s' \"$@\" ~%"
                              (string-append (assoc-ref inputs "bash") "/bin/sh")
                              (string-append (assoc-ref inputs "xhost") "/bin/xhost")
                              (string-append (assoc-ref inputs "emacs") "/bin/emacs")
                              (string-append (assoc-ref inputs "dbus") "/bin/dbus-launch")
                              (string-append (assoc-ref inputs "emacs") "/bin/emacsclient")
                              '(cond
                                ((file-exists-p "~/.exwm")
                                 (load-file "~/.exwm"))
                                ((not (featurep 'exwm))
                                 (require 'exwm)
                                 (require 'exwm-config)
                                 (exwm-config-default)
                                 (message (concat "exwm configuration not found. "
                                                  "Falling back to default configuration...")))))))
                  (chmod exwm-executable 365)
                  #t))))))))))
