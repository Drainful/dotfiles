(use-modules (gnu)
             (gnu system nss)
             (gnu services base)
             (gnu services sound)
             (gnu packages fonts)
             (gnu packages linux)
             (nongnu packages linux)
             (lib))

(use-service-modules desktop xorg)
(use-package-modules certs)

(let* ((keyboard-layout (keyboard-layout "us" "altgr-intl"))
       (os-part ((os-part
                  (list
                   (load "./parts/core.scm")
                   (load "./parts/exwm.scm")
                   (load "./parts/graphical.scm")
                   ;; (load "./parts/gnome.scm")
                   )

                  (list
                   nss-certs
                   font-inconsolata
                   brightnessctl
                   pulseaudio
                   bluez
                   )
                  
                  (list
                   (bluetooth-service #:auto-enable? #t)
                   ;; (service (nix-service-type))
                   ;; (service alsa-service-type)
                   (set-xorg-configuration
                    (xorg-configuration
                     (keyboard-layout keyboard-layout)))))))
       (services (append (assoc-ref os-part #:services)
                         ;; %desktop-services
                         (modify-services %desktop-services
                           (udev-service-type config =>
                                              (udev-configuration
                                               (inherit config)
                                               (rules (cons brightnessctl
                                                            (udev-configuration-rules config)))))
                           ;; (gdm-service-type config =>
                           ;;                   (gdm-configuration
                           ;;                    (inherit config)
                           ;;                    (xsession " [ -f \"$HOME/.Xresources\" ] && xrdb -merge \"$HOME/.Xresources\" ")))
                           )))
       (packages (append (assoc-ref os-part #:packages)
                         %base-packages)))
  (operating-system
    (kernel linux-5.3)
    (firmware (list linux-firmware))
    (host-name "guix-yoga")
    (timezone "America/Chicago")
    (locale "en_US.utf8")
    (keyboard-layout keyboard-layout)
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (target "/boot/efi")
                 (keyboard-layout keyboard-layout)))
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
    (users (cons (user-account
                  (name "adrian")
                  (group "users")
                  (supplementary-groups '("lp" "wheel" "netdev"
                                          "audio" "video"))
                  (home-directory "/home/guix/adrian"))
                 %base-user-accounts))
    (packages packages)
    (services services)
    (name-service-switch %mdns-host-lookup-nss)))
