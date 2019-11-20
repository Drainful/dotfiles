(use-modules (gnu)
             (gnu packages emacs)
             (gnu packages emacs-xyz)
             (gnu packages linux)
             (gnu packages display-managers)
             (my-packages)
             (lib))

(os-part (list (load "./emacs.scm"))
         (list my-emacs-exwm
               ;; sddm
               bluez
               aumix
               )
         (list))
