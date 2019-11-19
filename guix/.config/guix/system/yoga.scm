(use-modules (gnu)
             (gnu system nss)
             (gnu packages fonts)
             (nongnu packages linux)
             (lib))

(use-service-modules desktop xorg)
(use-package-modules certs)

(define keyboard-layout (keyboard-layout "us" "altgr-intl"))

(let ((os-part
       ((os-part (list
                  (load "./parts/core.scm")
                  (load "./parts/exwm.scm")
                  (load "./parts/graphical.scm")
                  ;; (load "./parts/gnome.scm")
                  )
                 (list
                  nss-certs
                  font-inconsolata)
                 (list)))))
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
                 (supplementary-groups '("wheel" "netdev"
                                         "audio" "video"))
                 (home-directory "/home/guix/adrian"))
                %base-user-accounts))
   (packages (append (assoc-ref os-part #:packages)
                     %base-packages))
   (services (append (assoc-ref os-part #:services)                     
                     (list (set-xorg-configuration
                            (xorg-configuration
                             (keyboard-layout keyboard-layout))))
                     %desktop-services))
   (name-service-switch %mdns-host-lookup-nss)))
