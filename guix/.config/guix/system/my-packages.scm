(define-module (my-packages)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages messaging)
  #:use-module (ice-9 match))

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
