(use-modules (gnu)
             (gnu packages messaging)
             (gnu services messaging)
             ;; (guix packages)
             ;; (guix git-download)
             ;; (guix build-system gnu)
             ;; ((guix licenses) #:prefix license:)
             (my-packages)
             (lib))

(os-part (list)
         (list)
         (list (service
                bitlbee-service-type
                (bitlbee-configuration
                 (plugins (list my-bitlbee-discord))))))

