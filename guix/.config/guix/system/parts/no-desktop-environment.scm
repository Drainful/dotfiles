(use-modules (gnu)
             (gnu packages compton)
             (gnu packages suckless)
             (gnu packages gnome)
             (gnu packages linux)
             (gnu packages fonts)

             (gnu services base)
             (gnu services xorg)
             (lib))
(os-part (list)
         (list
          compton
          slock
          gnome-icon-theme
          brightnessctl)
         (cons*
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
                            (udev-configuration-rules config)))))))))
