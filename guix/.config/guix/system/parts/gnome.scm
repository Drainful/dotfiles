(use-modules (gnu) (lib))

(use-service-modules desktop xorg)
(use-package-modules certs gnome)

(os-part '()
         (list gvfs) ; for user mounts
         (list (service gnome-desktop-service-type)
               ;; (set-xorg-configuration
               ;;  (xorg-configuration
               ;;   (keyboard-layout keyboard-layout)))
               ))
