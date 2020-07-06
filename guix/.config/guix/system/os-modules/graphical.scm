(define-module (os-modules graphical)
  #:use-module (lib)
  #:use-module (my-packages)
  #:use-module (os-modules emacs)

  #:use-module (srfi srfi-1)
  #:use-module (gnu)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)

  #:use-module (gnu packages compton)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages fonts)

  #:use-module (gnu packages gimp)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages display-managers)
  #:use-module (gnu packages games)
  #:use-module (games packages minecraft)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages video)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu packages wine)
  #:use-module (nongnu packages wine)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages lxqt)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages syncthing)
  #:use-module (gnu packages freedesktop)
  )

(define-public graphical-games (os-module #:packages (list crawl-tiles
                                                           my-multimc)))

(define-public art (os-module #:packages (list gimp krita)))

(define-public graphical
  (os-module #:inherit (list art)
             #:packages (list
                         qutebrowser
                         xcape
                         xrdb
                         xrandr

                         mpv

                         qbittorrent

                         pavucontrol

                         wine
                         winetricks

                         ;; gparted

                         ;; fonts
                         font-gnu-freefont
                         font-google-noto
                         font-tex-gyre
                         font-ubuntu
                         font-dejavu
                         font-terminus
                         font-liberation
                         font-inconsolata
                         font-gnu-unifont
                         font-adobe-source-han-sans
                         
                         fontconfig
                         xfontsel
                         redshift

                         imagemagick
                         xdpyinfo
                         
                         ;; graphvis
                         mesa-utils

                         screengrab

                         pinentry-emacs

                         syncthing
                         ;; qsyncthingtray

                         udiskie
                         )))

(define-public no-desktop-environment
  (os-module
   #:packages (list
               compton
               slock
               gnome-icon-theme
               brightnessctl)
   #:services (cons*
               (service
                slim-service-type
                (slim-configuration
                 (xorg-configuration
                  (xorg-configuration
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

(define-public exwm
  (os-module #:inherit (list emacs
                             no-desktop-environment
                             graphical)
             #:packages (list my-emacs-exwm ;; emacs-exwm
                              emacs-helm-exwm)))

(define-public gnome
  (os-module #:packages (list gvfs) ; for user mounts
             #:services (list (service gnome-desktop-service-type))))
