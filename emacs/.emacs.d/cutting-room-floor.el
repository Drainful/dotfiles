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
