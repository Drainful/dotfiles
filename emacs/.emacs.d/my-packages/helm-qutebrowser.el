;;; helm-qutebrowser.el -*- lexical-binding: t; -*-
;; Copyright (C) 2019 Adrian Fullmer

;; Author: Adrian Fullmer <adrianfullmer@yahoo.com>
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Provides `helm-qutebrowser', a function to run a Helm session over
;; the list of qutebrowser buffers and open new qutebrowser buffers.

;;; Code:
(require 'helm)
(require 'helm-exwm)

(defun helm-qutebrowser--open (url)
  (let ((process-environment (browse-url-process-environment))
        (url (browse-url-encode-url url)))
    (start-process
     (concat "qutebrowser" url) nil
     "qutebrowser" url)))

(defvar helm-qutebrowser-source-not-found
  (helm-build-dummy-source
      "Open url in new tab"
    :action (helm-make-actions
             "Open url"
             #'helm-qutebrowser--open)))

;; inspired by `helm-exwm-highlight-buffers'. 
(defun helm-qutebrowser-highlight-buffers (buffers)
  "Transformer function to highlight BUFFERS list.
Should be called after others transformers i.e (boring buffers).
Depends upon qutebrowser configuration found within these dotfiles, specifically
  c.window.title_format = \"{title} -- {current_url}\""
  (cl-loop for i in buffers
           for (name url) = (split-string i " -- ")
           for truncurl = (when url (substring url 0 (min 100 (length url))))
           for truncbuf = (if (> (string-width name) helm-exwm-buffer-max-length)
                              (helm-substring-by-width
                               name helm-exwm-buffer-max-length
                               helm-buffers-end-truncated-string)
                            (concat name
                                    (make-string
                                     (- (+ helm-exwm-buffer-max-length
                                           (length helm-buffers-end-truncated-string))
                                        (string-width name))
                                     ? )))
           collect (let ((helm-pattern (helm-buffers--pattern-sans-filters
                                        (and helm-buffers-fuzzy-matching ""))))
                     (cons (if helm-buffer-details-flag
                               (concat
                                (funcall helm-fuzzy-matching-highlight-fn truncbuf)
                                (when truncurl (concat "  " (propertize truncurl 'face 'helm-buffer-process))))
                             (funcall helm-fuzzy-matching-highlight-fn name))
                           (get-buffer i)))))

(defvar helm-qutebrowser-truncate-lines t)

(defvar helm-qutebrowser--buffers-source
  (helm-build-sync-source "Qutebrowser tabs"
    :candidates (lambda () (helm-exwm-candidates (lambda ()
                                              (string= (downcase (or exwm-class-name ""))
                                                       (downcase "qutebrowser")))))
    :candidate-transformer #'helm-qutebrowser-highlight-buffers
    :action '(("Switch to buffer(s)" . helm-buffer-switch-buffers)
              ("Switch to buffer(s) in other window `C-c o'" . helm-buffer-switch-buffers-other-window)
              ("Switch to buffer in other frame `C-c C-o'" . switch-to-buffer-other-frame)
              ("Kill buffer(s) `M-D`" . helm-kill-marked-buffers))
    ;; When follow-mode is on, the persistent-action allows for multiple candidate selection.
    :persistent-action 'helm-buffers-list-persistent-action
    :keymap helm-exwm-map))

(defvar helm-qutebrowser-sources '(helm-qutebrowser--buffers-source
                   helm-qutebrowser-source-not-found))

(defun helm-qutebrowser-build-exwm-source ()
  "Build a Helm source for all non-EXWM buffers."
  (helm-exwm-build-source (lambda ()
                            (not (string= (downcase (or exwm-class-name ""))
                                          (downcase "qutebrowser"))))))

(defun helm-qutebrowser ()
  (interactive)
  (helm :sources helm-qutebrowser-sources
        :truncate-lines helm-qutebrowser-truncate-lines))

(provide 'helm-qutebrowser)
