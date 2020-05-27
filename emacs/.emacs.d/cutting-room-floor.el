;;; This was code patched into `helm-highlight-buffers' to provide
;;; custom information for qutebrowser buffers. Relies on variables
;;; local to that function. I decided to just use multiple sources
;;; instead.
(if (string= (downcase (or (with-current-buffer i
                             exwm-class-name)
                           ""))
             "qutebrowser")
    (pcase-let ((`(,name ,url) (split-string i " -- ")))
      (let ((truncurl (when url
                        (substring url
                                   0
                                   (min 100
                                        (length url)))))
            (truncbuf (if (> (string-width name) helm-buffer-max-length)
                          (helm-substring-by-width
                           name helm-buffer-max-length
                           helm-buffers-end-truncated-string)
                        (concat name
                                (make-string
                                 (- (+ helm-buffer-max-length
                                       (length
                                        helm-buffers-end-truncated-string))
                                    (string-width name))
                                 ? )))))
        (concat (funcall helm-fuzzy-matching-highlight-fn
                         truncbuf)
                helm-buffers-column-separator
                (when truncurl (propertize truncurl
                                           'face 'helm-buffer-process)))))
  (concat (funcall helm-fuzzy-matching-highlight-fn
                   truncbuf)
          helm-buffers-column-separator
          (propertize (downcase (or (with-current-buffer i
                                      exwm-class-name)
                                    ""))
                      'face 'helm-buffer-process)))


;;;; lazy list experiments

(defmacro lcons (car cdr)
  `(cons ,car (lambda () ,cdr)))

(defun reify (lazy-list)
  (nreverse
   (cl-labels ((rec (ll accum)
                    (if (consp ll)
                        (rec (funcall (cdr ll)) (cons (car ll) accum))
                      accum)))
     (rec lazy-list nil))))

(defun lmapcar (f &rest lists)
  (when (notany #'null lists)
    (lcons (apply f (mapcar #'car lists))
           (apply #'lmapcar f (mapcar #'cdr lists)))))

(defun liota (n)
  (cl-labels ((rec (i)
                   (if (<= i 0)
                       nil
                     (lcons (- n i) (rec (1- i))))))
    (rec n)))

(defun fcar (fcons)
  (funcall fcons 0))

(defun fcdr (fcons)
  (lambda (n) (funcall fcons (1+ n))))

(defun fiota (n)
  (lambda (i) (if (< i n) i nil)))
