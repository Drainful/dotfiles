(define-module (lib))


;; include udev rules merging

;; make functions to merge package and service lists and throw error
;; when have 2 of same name.

(define packages-keyword #:packages)
(define services-keyword #:services)

(define-public (os-part inherit-list packages services)
  (if (null? inherit-list)
      (lambda () (list (cons packages-keyword packages)
                  (cons services-keyword services)))
      (lambda () (let ((inherited ((car inherit-list))))
              ((os-part (cdr inherit-list)
                        (append (or (assoc-ref inherited packages-keyword) #nil)
                                packages)
                        (append (or (assoc-ref inherited services-keyword) #nil)
                                services)))))))

(define-macro (os-part-paths inherit-list packages services)
  `(os-part (list ,@(map (lambda (inherit-path)
                           `(load ,inherit-path))
                         inherit-list))
            ,packages
            ,services))
