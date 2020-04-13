(define-module (lib)
  #:use-module (guix packages)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (os-module
            ;; merge-records
            ))


;; include udev rules merging

;; make functions to merge package and service lists and throw error
;; when have 2 of same name.

(define packages-keyword #:packages)
(define services-keyword #:services)
(define jobs-keyword #:jobs)

#| Create a prototypal struct system that mirrors the API of the
standard struct system (perhaps not defining a new defun for each
field) with the capacity to be reified into guile structs. This will
do for now. |#

(define (assert-all list pred)
  (map
   (lambda (elt)
     (if (pred elt)
         elt
         (error
          (string-append (format #f "os-module argument \"~A\" does not conform to " elt)
                         (symbol->string (procedure-name pred))))))
   list))

(define* (os-module #:key (inherit '()) (packages '()) (services '()) (jobs '()))
  (if (null? inherit)
      (lambda () (list (cons packages-keyword (assert-all packages package?))
                  (cons services-keyword (assert-all services service?))
                  (cons jobs-keyword (assert-all jobs gexp?))))
      (lambda () (let ((inherited ((car inherit))))
              ((os-module #:inherit (cdr inherit)
                          #:packages (append (or (assoc-ref inherited packages-keyword) #nil)
                                             packages)
                          #:services (append (or (assoc-ref inherited services-keyword) #nil)
                                             services)
                          #:jobs (append (or (assoc-ref inherited jobs-keyword) #nil)
                                         jobs)))))))

;; (define-macro (os-module-paths inherit packages services)
;;   `(os-module (list ,@(map (lambda (inherit-path)
;;                            `(load ,inherit-path))
;;                          inherit))
;;             ,packages
;;             ,services))

;; (define default-operating-system (operating-system (bootloader #f)
;;                                                    (host-name #f)
;;                                                    (file-systems #f)
;;                                                    (timezone #f)))

;; (define-syntax %merge-records
;;   (define (merge obj1 obj2 record-field-names merge-fields-function)
;;     #`())
;;   (lambda (s)
;;     (syntax-case s ()
;;       ((_ record-type
;;           record-field-names
;;           merge-fields-function
;;           (obj1 obj2 . rest))
;;        #`(let ((obj1-record-type (and (struct? obj1) (struct-vtable-name (struct-vtable obj1))))
;;                (obj2-record-type (and (struct? obj2) (struct-vtable-name (struct-vtable obj2)))))
;;            (if (eq? record-type obj1-record-type obj2-record-type)
;;                (%merge-records record-type
;;                                record-field-names
;;                                merge-fields-function
;;                                (,(merge obj1 obj2 record-field-names merge-fields-function) . rest))
;;                (error "Objects provided to merge-records are not all objects of the same record type.")))
;;        (_ _ _ _ (obj))
;;        #'obj))))

;; (define-syntax merge-records
;;   "Merge a sequence of record objects as if by inheritence,
;; combining atomic fields according to MERGE-FIELDS-FUNCTION."
;;   (lambda (s)
;;     (syntax-case s ()
;;       ((_ merge-fields-function (obj1 obj2 obj* ...))
;;        #`(let (())
;;            (%merge-records merge-fields-function (obj1 obj2 obj* ...)))))))
