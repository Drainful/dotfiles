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

(define-command play-video-in-current-page (&optional (buffer (current-buffer)))
  "Play video in the currently open buffer."
  (uiop:run-program (list "mpv" (url buffer))))

(keys
 ", m" 'play-video-in-current-page)

(define-mode my-mode ()
  "Dummy mode for the custom key bindings in `*my-keymap*'."
  ((keymap-scheme :initform (keymap:make-scheme
                             scheme:cua *my-keymap*
                             scheme:emacs *my-keymap*
                             scheme:vi-normal *my-keymap*))))

(define-configuration (buffer web-buffer)
  ((default-modes (append '(my-mode vi-normal-mode) %slot-default))))

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(ql:quickload :slynk)
(load-after-system :slynk "~/.config/nyxt/my-slynk.lisp")
