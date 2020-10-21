(in-package :nyxt-user)

(defvar *my-keymap* (make-keymap "My keymap"))

(defmacro keys (&body bindings)
  `(define-key *my-keymap*
       ,@bindings))

(keys
 "y y" 'nyxt/web-mode:copy 
 "p" 'nyxt/web-mode:paste 
 "b" 'switch-buffer
 "s-TAB" 'switch-buffer-previous
 "b" 'switch-buffer
 ", w" 'delete-current-buffer)

(in-package :nyxt-user)

(defun eval-in-emacs (&rest s-exps)
  "Evaluate S-exps with `emacsclient'."
  (let ((s-exps-string
         (write-to-string
          `(progn ,@s-exps) :case :downcase)))
    (log:debug "Sending to Emacs: ~a" s-exps-string)
    (ignore-errors (uiop:run-program
                    (list "emacsclient" "--eval" s-exps-string)))))

(in-package :nyxt-user)

(define-command play-video-in-current-page (&optional (buffer (current-buffer)))
  "Play video in the currently open buffer."
  (uiop:run-program (list "mpv" (url buffer))))

(keys
 ", m" 'play-video-in-current-page)

(in-package :nyxt-user)

(define-mode my-mode ()
  "Dummy mode for the custom key bindings in `*my-keymap*'."
  ((keymap-scheme :initform (keymap:make-scheme
                             scheme:cua *my-keymap*
                             scheme:emacs *my-keymap*
                             scheme:vi-normal *my-keymap*))))

(in-package :nyxt-user)

(define-configuration buffer
    ((default-modes (append '(my-mode vi-normal-mode) %slot-default))))
