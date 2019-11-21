(in-package :next-user)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eval-in-emacs (&rest s-exps)
  "Evaluate S-exps with `emacsclient'."
  (let ((s-exps-string
         (write-to-string
          `(progn ,@s-exps) :case :downcase)))
    (log:debug "Sending to Emacs: ~a" s-exps-string)
    (ignore-errors (uiop:run-program
                    (list "emacsclient" "--eval" s-exps-string)))))

(defvar *my-keymap* (make-keymap)
  "My keymap.")

(define-command play-video-in-current-page (&optional (buffer (current-buffer)))
  "Play video in the currently open buffer."
  (uiop:run-program (list "mpv" (url buffer))))
(define-key :keymap *my-keymap* ", m" #'play-video-in-current-page)

(define-key :keymap *my-keymap*
  ;; "y y" #'copy ; which lisp function is copy?
  "C-u" #'scroll-page-up
  "C-d" #'scroll-page-down)

(define-mode my-mode ()
  "Dummy mode for the custom key bindings in `*my-keymap*'."
  ((keymap-schemes :initform (list :emacs-map *my-keymap*
                                   :vi-normal *my-keymap*))))

(defclass my-buffer (buffer)
  ((default-modes
       :initform (append
                  '(my-mode
                    vi-normal-mode)
                  (get-default 'buffer 'default-modes)))))

(setq *buffer-class* 'my-buffer)

(defclass my-remote-interface (remote-interface)
  ((open-external-link-in-new-window-p :initform t)
   (search-engines
    :initform
    '(("default" . "https://google.com/search?hl=en&q=~a")
      ("go" . "https://duckduckgo.com/?q=~a")
      ("yt" . "https://www.youtube.com/results?search_query=~a")
      ("wp" . "https://en.wikipedia.org/w/index.php?search=~a")))))

(setq *remote-interface-class* 'my-remote-interface)

(in-package :next)

;; (add-to-default-list 'vi-normal-mode 'buffer 'default-modes)

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

(start-swank)
