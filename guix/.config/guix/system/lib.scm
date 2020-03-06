(define-module (lib)
  ;; #:use-module (guix records)
  #:export (os-part
            ;; merge-records
            ))


;; include udev rules merging

;; make functions to merge package and service lists and throw error
;; when have 2 of same name.

(define packages-keyword #:packages)
(define services-keyword #:services)

(define (os-part inherit-list packages services)
  (if (null? inherit-list)
      (lambda () (list (cons packages-keyword packages)
                  (cons services-keyword services)))
      (lambda () (let ((inherited ((car inherit-list))))
              ((os-part (cdr inherit-list)
                        (append (or (assoc-ref inherited packages-keyword) #nil)
                                packages)
                        (append (or (assoc-ref inherited services-keyword) #nil)
                                services)))))))

;; (define-macro (os-part-paths inherit-list packages services)
;;   `(os-part (list ,@(map (lambda (inherit-path)
;;                            `(load ,inherit-path))
;;                          inherit-list))
;;             ,packages
;;             ,services))

(define default-operating-system (operating-system (bootloader #f)
                                                   (host-name #f)
                                                   (file-systems #f)
                                                   (timezone #f)))

(define-syntax %merge-records
  (define (merge obj1 obj2 record-field-names merge-fields-function)
    #`())
  (lambda (s)
    (syntax-case s ()
      ((_ record-type
          record-field-names
          merge-fields-function
          (obj1 obj2 . rest))
       #`(let ((obj1-record-type (and (struct? obj1) (struct-vtable-name (struct-vtable obj1))))
               (obj2-record-type (and (struct? obj2) (struct-vtable-name (struct-vtable obj2)))))
           (if (eq? record-type obj1-record-type obj2-record-type)
               (%merge-records record-type
                               record-field-names
                               merge-fields-function
                               (,(merge obj1 obj2 record-field-names merge-fields-function) . rest))
               (error "Objects provided to merge-records are not all objects of the same record type.")))
       (_ _ _ _ (obj))
       #'obj))))

(define-syntax merge-records
  "Merge a sequence of record objects as if by inheritence,
combining atomic fields according to MERGE-FIELDS-FUNCTION."
  (lambda (s)
    (syntax-case s ()
      ((_ merge-fields-function (obj1 obj2 obj* ...))
       #`(let (())
           (%merge-records merge-fields-function (obj1 obj2 obj* ...)))))))
