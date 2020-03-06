(use-modules (srfi srfi-1)
             (gnu)
             (gnu system nss)
             (gnu services base)
             (gnu services xorg)
             (gnu packages linux)
             (nongnu packages linux)
             (lib))

(use-service-modules desktop xorg)
(use-package-modules certs)

(let* ((keyboard-layout (keyboard-layout "us" "altgr-intl"))
       (default-user-name "adrian")
       (os-part ((os-part (list
                           (load "./parts/core.scm")
                           (load "./parts/exwm.scm")
                           (load "./parts/audio.scm")
                           (load "./parts/bluetooth.scm")
                           (load "./parts/games.scm")
                           (load "./parts/bitlbee.scm"))
                          (list nss-certs)
                          (list))))
       (services (modify-services (assoc-ref os-part #:services)
                   (gdm-service-type
                    config => (gdm-configuration
                               (inherit config)
                               (default-user default-user-name)
                               (xorg-configuration
                                (xorg-configuration
                                 (inherit (gdm-configuration-xorg config))
                                 (keyboard-layout keyboard-layout)))))
                   (slim-service-type
                    config => (slim-configuration
                               (inherit config)
                               (default-user default-user-name)
                               (xorg-configuration
                                (xorg-configuration
                                 (inherit (slim-configuration-xorg config))
                                 (keyboard-layout keyboard-layout)))))
                   (console-font-service-type
                    config => (map (lambda (tty)
                                     (cons (car tty)
                                           (file-append font-terminus "/share/consolefonts/ter-132n")))
                                   config))))
       (packages (append (assoc-ref os-part #:packages) %base-packages)))
  (operating-system
    (kernel linux-5.4)
    (firmware (list linux-firmware))
    (host-name "guix-yoga")
    (timezone "America/Chicago")
    (locale "en_US.utf8")
    (keyboard-layout keyboard-layout)
    (file-systems (append
                   (list (file-system
                           (device (file-system-label "guix-root"))
                           (mount-point "/")
                           (type "ext4"))
                         (file-system
                           (device (file-system-label "home"))
                           (mount-point "/home")
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
                 (keyboard-layout keyboard-layout)

                 (menu-entries
                  (list (menu-entry
                         (label "Ubuntu")
                         (device "alternate-root")
                         (linux "/boot/vmlinuz")
                         (linux-arguments '("root=/dev/nvme0n1p5"))
                         (initrd "/boot/initrd.img"))))))
    (groups (cons (user-group (name "nonet")
                              (password #f))
                  %base-groups))
    (users (cons (user-account
                  (name default-user-name)
                  (group "users")
                  (supplementary-groups '("lp" "wheel" "netdev"
                                          "audio" "video" "nonet"))
                  (home-directory (string-append "/home/guix/" default-user-name)))
                 %base-user-accounts))
    (packages packages)
    (services services)
    (name-service-switch %mdns-host-lookup-nss)))
