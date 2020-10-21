(define-module (os-modules graphical)
  #:use-module (lib)
  #:use-module (my-packages)
  #:use-module (gnu packages)
  #:use-module (os-modules emacs)
  #:use-module (srfi srfi-1)
  #:use-module (gnu)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  ;;#:use-module (nongnu packages wine)
  #:export (exwm no-desktop-environment)
  )

(define-public graphical-games (os-module #:packages (cons*
                                                      my-multimc
                                                      (pkgs crawl-tiles
                                                            mgba))))

(define-public art (os-module #:packages (pkgs gimp krita)))

(define-public wine
  (os-module #:inherit (list)
             #:packages (cons*
                         ;; winetricks
                         (pkgs wine
                               wine64))))

(define-public graphical
  (os-module #:inherit (list)
             #:packages (cons*
                         (pkgs xcape
                               xrdb
                               xrandr
                               xdg-utils
                               xinput

                               mpv

                               gparted

                               ;; fonts
                               font-dejavu
                               font-iosevka
                               font-gnu-freefont
                               font-gnu-unifont
                               font-adobe-source-han-sans

                               redshift

                               sxiv
                               imagemagick
                               xdpyinfo
                               
                               ;; graphvis
                               mesa-utils

                               screengrab

                               pinentry-emacs

                               syncthing
                               ;; qsyncthingtray
                               ))))

(define* (no-desktop-environment default-user-name keyboard-layout #:optional vts)
  (let ((vts (or vts (list "vt7"))))
    (os-module
     #:packages (pkgs compton
                      slock
                      gnome-icon-theme
                      brightnessctl
                      pavucontrol)
     #:services (cons*
                 ;; (service
                 ;;  slim-service-type
                 ;;  (slim-configuration
                 ;;   (default-user default-user-name)
                 ;;   (xorg-configuration
                 ;;    (xorg-configuration
                 ;;     (keyboard-layout keyboard-layout)))))
                 (append
                  (map (lambda (vt display)
                         (service
                          slim-service-type
                          (slim-configuration
                           (default-user default-user-name)
                           (vt vt)
                           (xorg-configuration
                            (xorg-configuration
                             (keyboard-layout keyboard-layout))))))
                       vts
                       (map (lambda (n)
                              (string-append ":" (number->string n)))
                            (iota (length vts))))
                  (remove
                   (lambda (service)
                     (eq? (service-kind service) gdm-service-type))
                   (modify-services %desktop-services
                     (udev-service-type
                      config =>
                      (udev-configuration
                       (inherit config)
                       (rules (cons (pkg brightnessctl)
                                    (udev-configuration-rules config))))))))))))

(define (exwm)
  (os-module #:inherit (list emacs
                             graphical)
             #:packages (cons*
                         ;; my-emacs-exwm
                         (pkgs emacs-exwm emacs-helm-exwm))))

(define-public (gnome)
  (os-module #:packages (pkgs gvfs) ; for user mounts
             #:services (list (service gnome-desktop-service-type))))

;; (gdm-service-type
;;  config => (gdm-configuration
;;             (inherit config)
;;             (default-user default-user-name)
;;             (xorg-configuration
;;              (xorg-configuration
;;               (inherit (gdm-configuration-xorg config))
;;               (keyboard-layout keyboard-layout)))))
