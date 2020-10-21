(defun scroll--scaled (v h)
  (cond (v (next::%scroll-up
            :scroll-distance (* v (next:current-zoom-ratio (next:current-buffer)))))
        (h (next::%scroll-right
            :scroll-distance (* h (next:current-zoom-ratio (next:current-buffer)))))))

(define-command scroll-up--scaled ()
  (scroll--scaled (scroll-distance (next:current-buffer))
                  nil))

(define-command scroll-down--scaled ()
  (scroll--scaled (- (scroll-distance (next:current-buffer)))
                  nil))

(define-command scroll-right--scaled ()
  (scroll--scaled nil
                  (horizontal-scroll-distance (next:current-buffer))))

(define-command scroll-left--scaled ()
  (scroll--scaled nil
                  (- (horizontal-scroll-distance (next:current-buffer)))))
