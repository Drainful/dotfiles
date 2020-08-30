(define-module (os-modules)
  #:use-module (lib)
  #:use-module (my-packages)
  #:use-module (gnu)
  #:use-module (guix)
  #:use-module (gnu services messaging)
  #:use-module (gnu services base)
  #:use-module (gnu services xorg)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services mcron)
  #:export (garbage-collection
            delete-generations))

(use-service-modules desktop xorg)
(use-package-modules base idutils certs gnome)

(define-public audio
  (os-module #:packages
             (pkgs pulseaudio
                   alsa-lib
                   bluez-alsa
                   sbc)))

(define-public bitlbee
  (os-module #:services
             (list (service
                    bitlbee-service-type
                    (bitlbee-configuration
                     (plugins (list my-bitlbee-discord)))))))

(define-public bluetooth
  (os-module #:packages (pkgs bluez)
             #:services (list (bluetooth-service #:auto-enable? #t))))

(define-public core
  (os-module
   #:packages 
   (pkgs neovim

         git curl wget youtube-dl ffmpeg neofetch file usbutils
         iptables netcat patchelf rlwrap stow openssh groff gnupg
         lm-sensors parted

         ;; pass
         password-store
         pinentry

         ;; 
         tree the-silver-searcher

         ;; archiving
         atool #|unrar|# unzip zip p7zip  

         ;; ntfs filesystem
         ntfs-3g

         udiskie ; mounting utiliity

         ;; direnv ; directory-specific environment variables
         )))

(define-public android
  (os-module #:packages (pkgs adb fastboot gmtp)))

(define-public virtual-machines
  (os-module
   #:services
   (list (service qemu-binfmt-service-type
                  (qemu-binfmt-configuration
                   (platforms (lookup-qemu-platforms "arm" "aarch64" "mips64el"))
                   (guix-support? #t))))))

(define* (delete-generations #:key (delete-after "1m"))
  (os-module
   #:jobs
   (list
    #~(job '(next-hour '(0))
           (string-append "guix package --delete-generations=" #$delete-after " ; "
                          "guix system --delete-generations=" #$delete-after)))))

(define* (garbage-collection #:key (delete-after "1m") (free "5G"))
  (os-module
   #:jobs
   (list
    #~(job '(next-hour '(0)) (string-append "guix gc -d " #$delete-after " -F " #$free)))))
