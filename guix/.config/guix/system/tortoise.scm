(use-modules (srfi srfi-1)
             (gnu)
             (gnu system nss)
             (gnu services base)
             (gnu services xorg)
             (gnu services mcron)
             (gnu services networking)
             (gnu packages linux)
             (gnu packages fonts)
             (my-packages)
             (nongnu packages linux)
             (nongnu system linux-initrd)
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
                                              (exwm default-user-name keyboard-layout)
                                              audio
                                              ;; (garbage-collection #:free "10G" #:delete-after "1w")
                                              ;; (delete-generations #:delete-after "1m")
                                              )
                              #:packages (list nss-certs)))))
  (operating-system
    (kernel linux)
    (host-name "guix-tortoise")
    (timezone "America/Chicago")
    (locale "en_US.utf8")
    (keyboard-layout keyboard-layout)
    (file-systems (append
                   (list (file-system
                           (device (file-system-label "guix-root"))
                           (mount-point "/")
                           (type "ext4"))
                         (file-system
                           (device "/dev/nvme0n1p1")
                           (mount-point "/boot/efi")
                           (type "vfat")))
                   %base-file-systems))
    (swap-devices '("/dev/nvme0n1p2"))
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (target "/boot/efi")
                 (keyboard-layout keyboard-layout)))
    (users (cons (user-account
                  (name default-user-name)
                  (group "users")
                  (supplementary-groups '("lp" "wheel" "netdev"
                                          "audio" "video"))
                  (home-directory (string-append "/home/guix/" default-user-name)))
                 %base-user-accounts))
    (packages (append (assoc-ref os-module #:packages) %base-packages))
    (services (cons*
               (service mcron-service-type
                        (mcron-configuration
                         (jobs (assoc-ref os-module #:jobs))))))
    (name-service-switch %mdns-host-lookup-nss)))
