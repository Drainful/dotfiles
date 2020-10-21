(use-modules (srfi srfi-1)
             (gnu)
             (gnu system nss)
             (gnu services base)
             (gnu services xorg)
             (gnu services mcron)
             (gnu services networking)
             (gnu services nix)
             (gnu packages fonts)
             (my-packages)
             (guix channels)
             (guix inferior)
             (lib)
             (os-modules)
             (os-modules graphical)
             (os-modules emacs))

(use-service-modules desktop xorg)
(use-package-modules certs)

(let* ((keyboard-layout (keyboard-layout "us" "altgr-intl"))
       (default-user-name "adrian")
       (os-module ((os-module #:inherit (list core
                                              (exwm)
                                              (no-desktop-environment default-user-name keyboard-layout (list "vt6" "vt7"))
                                              audio
                                              ;; (garbage-collection #:free "10G" #:delete-after "1w")
                                              ;; (delete-generations #:delete-after "1m")
                                              )
                              #:packages (list nss-certs)))))
  (operating-system
    ;; (kernel linux)
    (host-name "guix-tortoise")
    (timezone "America/Chicago")
    (locale "en_US.utf8")
    (keyboard-layout keyboard-layout)
    (file-systems (cons*
                   (file-system
                     (device (uuid "fc8927f6-3f05-4cfe-af99-85240e67ed3a"
                                   'ext4))
                     (mount-point "/")
                     (type "ext4"))
                   %base-file-systems))
    (swap-devices '("/dev/sda1"))
    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (target "/dev/sda")
                 (keyboard-layout keyboard-layout)))
    (users (cons (user-account
                  (name default-user-name)
                  (group "users")
                  (supplementary-groups '("lp" "wheel" "netdev"
                                          "audio" "video"))
                  (home-directory (string-append "/home/" default-user-name)))
                 %base-user-accounts))

    (packages (append (assoc-ref os-module #:packages) %base-packages))
    (services (cons*
               (simple-service 'ld-service-type activation-service-type
                               #~(begin
                                   (system* "mkdir" "-p" "/lib")
                                   (system* "ln" "-sf"
                                            (string-append #$glibc "/lib/ld-linux-x86.so.2")
                                            "/lib64/ld-linux-x86-64.so.2")
                                   (system* "mkdir" "-p" "/lib64")
                                   (system* "ln" "-s"
                                            (string-append #$glibc "/lib/ld-linux-x86-64.so.2")
                                            "/lib64/ld-linux-x86-64.so.2")))
               (simple-service 'set-env-vars
                               session-environment-service-type
                               '(("DPI" . "139")
                                 ("GUIX_RECONF_FILE" . "/home/adrian/.config/guix/system/tortoise.scm")
                                 ("GUIX_RECONF_LOAD_DIRECTORY" . "/home/adrian/.config/guix/load/")))
               (service mcron-service-type
                        (mcron-configuration
                         (jobs (assoc-ref os-module #:jobs))))
               (assoc-ref os-module #:services)))
    (name-service-switch %mdns-host-lookup-nss)))
