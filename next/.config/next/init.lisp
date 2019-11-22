(defmacro aif (test then &optional else)
  (let ((it test)) (if it then else)))

(in-package :next-user)

(defvar *my-keymap* (make-keymap) "My keymap.")

(defmacro keys (&body bindings)
  `(define-key :keymap *my-keymap*
     ,@bindings))

(keys
 "y y" #'next/web-mode:copy 
 "p" #'next/web-mode:paste 
 ", b" #'switch-buffer
 ", w" #'delete-current-buffer)

(in-package :next-user)

(defun current-zoom ()
  (next:current-zoom-ratio (next:current-buffer)))

(define-parenscript %set-zoom ((zoom 1))
  (ps:lisp (setf (current-zoom-ratio next::%buffer) zoom))
  (setf (ps:chain document body style zoom) (ps:lisp zoom)))

(defun set-zoom (zoom)
  (%set-zoom :zoom zoom))

(defun toggle-zoom ()
  (let ((zoom (current-zoom)))
    (if (> zoom 1.5)
        (set-zoom 1.0)
        (set-zoom 2.0))))

(keys
 ", z" #'toggle-zoom)

(defun set-zoom-1 ()
  (set-zoom 1.0))
(hooks:add-to-hook 'next:follow-hint-before-hook
                   #'set-zoom-1)
;; (hooks:remove-from-hook 'next:follow-hint-after-hook
;;                         #'next:unzoom-page)

(in-package :next-user)

(define-parenscript %scroll-to-top--scaled ((scale (current-zoom)))
  (ps:chain window (scroll-by 0 (* (ps:lisp scale)
                                   (- (ps:chain document body scroll-height))))))

(define-parenscript %scroll-to-bottom--scaled
    ((scale (current-zoom)))
  (ps:chain window (scroll-by 0 (* (ps:lisp scale)
                                   (ps:chain document body scroll-height)))))

(define-command scroll-to-top--scaled ()
  (%scroll-to-top--scaled))

(define-command scroll-to-bottom--scaled ()
  (%scroll-to-bottom--scaled))

(keys
 "G" #'scroll-to-bottom--scaled
 "g g" #'scroll-to-top--scaled)

(in-package :next)

(defun my-buffer-details (buffer)
  (setf (zoom-ratio-default buffer) 2.0)
  (unzoom-page :buffer buffer))

(defun my-minibuffer-defaults (minibuffer)
  (setf
   (max-lines minibuffer) 30
   (minibuffer-line-height minibuffer) "1.1em"
   (minibuffer-font-size minibuffer) "1.1em"))

(defun my-window-defaults (window)
  (setf
   (minibuffer-open-height window) 800))

(defun my-interface-defaults ()
  (hooks:add-to-hook (hooks:object-hook *interface* 'buffer-make-hook)
                     #'my-buffer-details)
  (hooks:add-to-hook (hooks:object-hook *interface* 'minibuffer-make-hook)
                     #'my-minibuffer-defaults)
  (hooks:add-to-hook (hooks:object-hook *interface* 'window-make-hook)
                     #'my-window-defaults))

(hooks:add-to-hook '*after-init-hook* #'my-interface-defaults)

(in-package :next-user)

(defvar search-engines
  '(("default" . "https://google.com/search?hl=en&q=~a")
    ("go" . "https://duckduckgo.com/?q=~a")
    ("yt" . "https://www.youtube.com/results?search_query=~a")
    ("wp" . "https://en.wikipedia.org/w/index.php?search=~a")))

(in-package :next-user)

(defun eval-in-emacs (&rest s-exps)
  "Evaluate S-exps with `emacsclient'."
  (let ((s-exps-string
         (write-to-string
          `(progn ,@s-exps) :case :downcase)))
    (log:debug "Sending to Emacs: ~a" s-exps-string)
    (ignore-errors (uiop:run-program
                    (list "emacsclient" "--eval" s-exps-string)))))

(in-package :next-user)

(define-command play-video-in-current-page (&optional (buffer (current-buffer)))
  "Play video in the currently open buffer."
  (uiop:run-program (list "mpv" (url buffer))))

(keys
 ", m" #'play-video-in-current-page)

(in-package :next-user)

(define-mode my-mode ()
  "Dummy mode for the custom key bindings in `*my-keymap*'."
  ((keymap-schemes :initform (list :emacs-map *my-keymap*
                                   :vi-normal *my-keymap*))))

(in-package :next-user)

(defclass my-buffer (buffer)
  ((default-modes
       :initform (append
                  '(my-mode
                    vi-normal-mode)
                  (get-default 'buffer 'default-modes)))
   (scroll-distance :initform 400)
   (horizontal-scroll-distance :initform 400)))

(setq *buffer-class* 'my-buffer)

(in-package :next-user)

(defclass my-remote-interface (remote-interface)
  (; (open-external-link-in-new-window-p :initform t)
   (search-engines :initform search-engines)))

(setq *remote-interface-class* 'my-remote-interface)

(start-swank)
