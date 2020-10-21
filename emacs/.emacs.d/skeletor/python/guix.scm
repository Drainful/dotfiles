(use-modules (guix gexp)
             (guix packages)
             (guix build-system python)
             (gnu packages python))

(define __PROJECT-NAME__
  (package
    (name "__PROJECT-NAME__")
    (version "0")
    (source (local-file "." #:recursive? #t))
    (build-system python-build-system)
    (inputs `())
    (synopsis #f) (description #f) (home-page #f) (license #f)))

__PROJECT-NAME__
