(use-modules (srfi srfi-1)
             (gnu)
             (gnu system nss)
             (gnu services base)
             (gnu packages linux)
             (nongnu packages linux)
             (lib))

(use-service-modules desktop xorg)
(use-package-modules certs)

(let* ((keyboard-layout (keyboard-layout "us" "altgr-intl"))
       (os-part ((os-part
                  (list
                   (load "./parts/core.scm")
                   (load "./parts/no-desktop-environment.scm")
                   (load "./parts/exwm.scm")
                   (load "./parts/graphical.scm")
                   (load "./parts/audio.scm")
                   (load "./parts/bluetooth.scm")
                   (load "./parts/games.scm"))
                  (list nss-certs)
                  (list))))
       (services (append (assoc-ref os-part #:services)
                         (cons (service
                                slim-service-type
                                (slim-configuration
                                 (xorg-configuration
                                  (xorg-configuration
                                   (keyboard-layout keyboard-layout)
                                   (extra-config '("
                   Section \"Device\"
                   
                       Identifier \"Intel Graphics\"
                   
                       Driver \"intel\"
                   
                       Option \"TearFree\" \"false\"
                   
                   EndSection
                   "))))))
                               (remove
                                (lambda (service)
                                  (eq? (service-kind service) gdm-service-type))
                                (modify-services %desktop-services
                                  (udev-service-type
                                   config =>
                                   (udev-configuration
                                    (inherit config)
                                    (rules (cons brightnessctl
                                                 (udev-configuration-rules config))))))))))
       (packages (append (assoc-ref os-part #:packages)
                         %base-packages)))
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
                  (name "adrian")
                  (group "users")
                  (supplementary-groups '("lp" "wheel" "netdev"
                                          "audio" "video" "nonet"))
                  (home-directory "/home/guix/adrian"))
                 %base-user-accounts))
    (packages packages)
    (services services)
    (name-service-switch %mdns-host-lookup-nss)))
