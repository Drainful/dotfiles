(use-modules (guix scripts environment))

(define fccache
  (make <service>
    #:provides '(fccache)
    #:docstring "Run 'fc-cache -frv'"
    #:start (lambda ()
              (guix-environment "--ad-hoc" "fontconfig" "--" "fc-cache" "-frv"))
    #:one-shot? #t))

(register-services fccache)
