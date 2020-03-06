(use-modules (gnu)
             (gnu packages linux)
             (gnu packages display-managers)
             (gnu packages emacs-xyz)
             (my-packages)
             (lib))

(os-part (list (load "./emacs.scm")
               (load "./no-desktop-environment.scm")
               (load "./graphical.scm"))
         (list my-emacs-exwm ;; emacs-exwm
               emacs-helm-exwm)
         (list))
