(use-modules (gnu)
             (gnu packages emacs)
             (gnu packages emacs-xyz)
             (lib))

(os-part (list (load "./emacs.scm")) (list emacs-exwm) '())
