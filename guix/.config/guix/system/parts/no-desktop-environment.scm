(use-modules (gnu)
             (gnu packages compton)
             (gnu packages suckless)
             (gnu packages gnome)
             (gnu packages linux)
             (lib))
(os-part (list)
         (list
          compton
          slock
          gnome-icon-theme
          brightnessctl)
         (list))
