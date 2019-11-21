(use-modules (gnu)
             (gnu packages linux)
             (gnu packages pulseaudio)
             (gnu packages display-managers)
             (my-packages)
             (lib))

(os-part (list (load "./emacs.scm"))
         (list my-emacs-exwm
               ;; sddm
               )
         (list))
