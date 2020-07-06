(use-modules (srfi srfi-1)
             (gnu)
             (gnu system nss)
             (gnu services base)
             (gnu services xorg)
             (gnu services mcron)
             (gnu services networking)
             (gnu packages linux)
             (gnu packages fonts)
             (nongnu packages linux)
             (lib)
             (os-modules)
             (os-modules graphical)
             (os-modules emacs))

(use-service-modules desktop xorg)
(use-package-modules certs)

(let ((keyboard-layout (keyboard-layout "us" "altgr-intl"))
      (default-user-name "adrian")
      (os-module ((os-module #:inherit (list core
                                             exwm
                                             audio
                                             bluetooth
                                             graphical-games
                                             bitlbee
                                             ;; virtual-machines
                                             android
                                             (garbage-collection #:free "10G"
                                                                 #:delete-after "1w"))
                             #:packages (list nss-certs)))))
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
    (packages (append (assoc-ref os-module #:packages) %base-packages))
    (services (cons*
               (service mcron-service-type
                        (mcron-configuration
                         (jobs (assoc-ref os-module #:jobs))))
               (service iptables-service-type
                        (let ((rules (plain-file
                                      "iptables-nonet.rules"
                                      "*filter 
:INPUT ACCEPT
:FORWARD ACCEPT
:OUTPUT ACCEPT
-A OUTPUT -m owner --gid-owner nonet -j DROP
COMMIT
"
                                      
                                      )))
                          
                          (iptables-configuration
                           (ipv4-rules rules)
                           (ipv6-rules rules)
                           )))
               (modify-services (assoc-ref os-module #:services)
                 
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
                                 config)))))
    (name-service-switch %mdns-host-lookup-nss)))
