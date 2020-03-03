(use-modules (guix gexp)
             (guix packages)
             (guix packages clojure)
             (guix build-system clojure))

(define __PROJECT-NAME__
  (package
    (name "__PROJECT-NAME__")
    (version "0")
    (source (local-file "." #:recursive? #t))
    (build-system clojure-build-system)
    (synopsis #f) (description #f) (home-page #f) (license #f)))

__PROJECT-NAME__
