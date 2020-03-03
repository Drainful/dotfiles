(use-modules (srfi srfi-1)
             (gnu)
             (gnu services base)
             (gnu packages linux)
             (lib))
(os-part (list)
         (list
          bluez)
         (list
          (bluetooth-service #:auto-enable? #t)))
