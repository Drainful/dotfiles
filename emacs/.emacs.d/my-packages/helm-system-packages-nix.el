;; (require 'helm-system-packages)

;; ;; Shut up byte compiler
;; (defvar eshell-buffer-name)
;; (declare-function eshell-interactive-process "esh-cmd.el")
;; (declare-function eshell-send-input "esh-mode.el")

;; ;; TODO: Add support for superseded and obsolete packages.
;; ;; TODO: Add support for multiple outputs (install, uninstall, listing...).
;; ;; TODO: Add support for multiple versions.
;; ;; TODO: Use guix.el instead of parsing guix commandline output.

;; (defvar helm-system-packages-nix-help-message
;;   "* Helm nix

;; Requirements:

;; ** Commands
;; \\<helm-system-packages-guix-map>
;; \\[helm-system-packages-toggle-descriptions]\t\tToggle display of package descriptions.")

;; (defvar helm-system-packages-nix-map
;;   (let ((map (make-sparse-keymap)))
;;     (set-keymap-parent map helm-map)
;;     (define-key map (kbd "C-]")   'helm-system-packages-toggle-descriptions)
;;     map))

;; (defun helm-system-packages-guix-transformer (packages)
;;   (let (res (pkglist (reverse packages)))
;;     (dolist (p pkglist res)
;;       (let ((face (cdr (assoc (helm-system-packages-extract-name p)
;;                               (plist-get (helm-system-packages--cache-get) :display)))))
;;         (cond
;;          ((not face)
;;           (when helm-system-packages-guix--show-uninstalled-p
;;             (push p res)))
;;          ((and helm-system-packages-guix--show-explicit-p
;;                (memq 'helm-system-packages-explicit face))
;;           (push (propertize p 'face (car face)) res)))))))
