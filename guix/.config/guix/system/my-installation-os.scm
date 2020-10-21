(define-module (my-installation-os)
  #:use-module (gnu packages)
  #:use-module (gnu system)
  #:use-module (gnu system install)
  #:use-module (lib))

(define my-installation-os
  (operating-system
    (inherit installation-os)
    (packages
     (append (pkgs stow
                   git)
             (operating-system-packages
              installation-os)))))

my-installation-os
