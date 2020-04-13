(define-module (os-modules)
  #:use-module (lib)
  #:use-module (my-packages)
  #:use-module (gnu)
  #:use-module (guix)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu services messaging)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages video)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages code)
  #:use-module (gnu packages file)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages mpd)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages busybox)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu packages java)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages groff)
  ;; (gnu packages graphvis)
  #:use-module (gnu packages android)
  #:use-module (gnu packages libusb)
  #:use-module (gnu services base)
  #:use-module (gnu services xorg)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services mcron)

  #:export (garbage-collection))

(use-service-modules desktop xorg)
(use-package-modules certs gnome)
(use-package-modules base idutils)

(define-public audio
  (os-module #:packages
             (list pulseaudio
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
  (os-module #:packages (list bluez)
           #:services (list (bluetooth-service #:auto-enable? #t))))

(define-public core
  (os-module
   #:packages 
   (list neovim

         git curl wget youtube-dl ffmpeg neofetch file usbutils
         iptables netcat patchelf rlwrap stow openssh groff

         tree the-silver-searcher

         ;; archiving
         atool #|unrar|# unzip zip

         busybox ; utilities

         direnv ; directory-specific environment variables
         
         ;; used as an application, not for development
         openjdk12)))

(define-public android
  (os-module #:packages (list adb fastboot gmtp)))

(define-public virtual-machines
  (os-module
   #:services
   (list (service qemu-binfmt-service-type
                  (qemu-binfmt-configuration
                   (platforms (lookup-qemu-platforms "arm" "aarch64" "mips64el"))
                   (guix-support? #t))))))

(define* (garbage-collection #:key (delete-after "1m") (free "5G"))
  (os-module
   #:jobs
   (list
    #~(job '(next-hour '(0))
           (string-append "guix gc -d "
                          delete-after
                          " -F "
                          free)))))
