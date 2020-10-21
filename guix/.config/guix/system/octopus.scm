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

(define ds4-udev-rules
  )

(let* ((keyboard-layout (keyboard-layout "us" "altgr-intl"))
       (default-user-name "adrian")
       (os-module ((os-module #:inherit (list core
                                              (exwm)
                                              (no-desktop-environment default-user-name keyboard-layout)
                                              graphical-games
                                              audio
                                              bluetooth
                                              ;; (garbage-collection #:free "10G" #:delete-after "1w")
                                              ;; (delete-generations #:delete-after "1m")
                                              )
                              #:packages (list nss-certs)))))
  (operating-system
    (kernel 
     (let* ((channels
             (list (channel
                    (name 'nonguix)
                    (url "https://gitlab.com/nonguix/nonguix")
                    (commit "a12f42e6781bc965a788ad969b0c5f96da13e10f"))
                   (channel
                    (name 'guix)
                    (url "https://git.savannah.gnu.org/git/guix.git")
                    (commit "6feb7a2107000f9ded547543dcda9d64402c6081"))))
            (inferior (inferior-for-channels channels)))
       (first (lookup-inferior-packages inferior "linux" "5.8.9"))))
    (initrd microcode-initrd)
    (firmware (list iwlwifi-firmware
                    ibt-hw-firmware
                    amdgpu-firmware
                    amd-microcode
                    linux-firmware))
    (host-name "guix-octopus")
    (timezone "America/Chicago")
    (locale "en_US.utf8")
    (keyboard-layout keyboard-layout)
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (target "/boot/efi")
                 (keyboard-layout keyboard-layout)))
    (mapped-devices
     (list (mapped-device
            (source (uuid "fb57d4f1-4cc7-457f-acd4-115adcec25a2"))
            (target "root")
            (type luks-device-mapping))))
    (file-systems (append
                   (list (file-system
                           (device (file-system-label "root"))
                           (mount-point "/")
                           (type "ext4")
                           (dependencies mapped-devices))
                         (file-system
                           (device "/dev/nvme0n1p1")
                           (mount-point "/boot/efi")
                           (type "vfat")))
                   %base-file-systems))
    (users (cons (user-account
                  (name default-user-name)
                  (group "users")
                  (supplementary-groups '("lp" "wheel" "netdev"
                                          "audio" "video")))
                 %base-user-accounts))

    (packages (append (assoc-ref os-module #:packages) %base-packages))
    (services (cons*
               (simple-service 'ld-service-type activation-service-type
                               #~(begin
                                   ;; (system* "mkdir" "-p" "/lib")
                                   ;; (system* "ln" "-sf"
                                   ;;          (string-append #$glibc "/lib/ld-linux-x86.so.2")
                                   ;;          "/lib64/ld-linux-x86-64.so.2")
                                   (system* "mkdir" "-p" "/lib64")
                                   (system* "ln" "-sf"
                                            (string-append #$glibc "/lib/ld-linux-x86-64.so.2")
                                            "/lib64/ld-linux-x86-64.so.2")))
               (simple-service 'set-env-vars
                               session-environment-service-type
                               '(("DPI" . "139")
                                 ("GUIX_RECONF_FILE" . "/home/adrian/.config/guix/system/octopus.scm")
                                 ("GUIX_RECONF_LOAD_DIRECTORY" . "/home/adrian/.config/guix/load/")))
               (service mcron-service-type
                        (mcron-configuration
                         (jobs (assoc-ref os-module #:jobs))))
               (modify-services (assoc-ref os-module #:services)
                 (udev-service-type config =>
                                    (udev-configuration (inherit config)
                                                        (rules (cons*
                                                                (udev-rule
                                                                 "99-ds4-controllers.rules"
                                                                 "
    # DualShock 4 over USB
    KERNEL==\"hidraw*\", ATTRS{idVendor}==\"054c\", ATTRS{idProduct}==\"05c4\", MODE=\"0666\"

    # DualShock 4 Wireless Adapter over USB
    KERNEL==\"hidraw*\", ATTRS{idVendor}==\"054c\", ATTRS{idProduct}==\"0ba0\", MODE=\"0666\"

    # DualShock 4 Slim over USB
    KERNEL==\"hidraw*\", ATTRS{idVendor}==\"054c\", ATTRS{idProduct}==\"09cc\", MODE=\"0666\"

    # DualShock 4 over Bluetooth
    KERNEL==\"hidraw*\", KERNELS==\"*054C:05C4*\", MODE=\"0666\"

    # DualShock 4 Slim over Bluetooth
    KERNEL==\"hidraw*\", KERNELS==\"*054C:09CC*\", MODE=\"0666\"")
                                                                (udev-configuration-rules config))))))))
    (name-service-switch %mdns-host-lookup-nss)))
