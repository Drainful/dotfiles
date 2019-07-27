;;; helm-winconf.el -*- lexical-binding: t; -*-
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

;; All I want is simple access to named window configurations. Is that
;; so much to ask? Helm helps.

;;; Code:

(require 'helm)

(defvar helm-winconf--names-alist nil "Maps names to winconfs")

(defun helm-winconf--removef (name)
  "Remove the cons with car `name' from helm-winconf--names-alist"
  (setq helm-winconf--names-alist
        (remove-if (lambda (elt) (string= name (car elt)))
                   helm-winconf--names-alist)))

(defun helm-winconf--save-window-config (config name)
  "Put the cons (name . config) at the top of
helm-winconf--names-alist, and delete all winconfs with the same
name."
  (helm-winconf--removef name)
  (setq helm-winconf--names-alist (cons (cons name config) helm-winconf--names-alist)))

(defun helm-winconf--assoc (name)
  "Return the winconf associated with the given name"
  (cdr (assoc name helm-winconf--names-alist)))

(defun helm-winconf--current ()
  "Return the name of the current winconf (the one on the top of
the alist.)"
  (caar helm-winconf--names-alist))

(defun helm-winconf--candidates () (when helm-winconf--names-alist
                         (append (cdr helm-winconf--names-alist)
                                 (cons (car helm-winconf--names-alist) nil))))

(defun helm-winconf--candidate-transformer (winconfs)
  (mapcar (lambda (name-and-conf)
            (car name-and-conf))
          winconfs))

(defun helm-winconf--new (name)
  "Create and switch to a new winconf with name `name'. If the name is
already in use, override that winconf."
  (interactive "M")
  (helm-winconf--save-window-config (current-window-configuration)
                        name))

(defun helm-winconf--switch-to-cons (name-and-conf)
  "Switch to the named cofiguration given by by the cons cell
`name-and-conf'"
  (let ((current (helm-winconf--current)))
    (unless (equal current
                   (car name-and-conf))
      (helm-winconf--new current)
      (set-window-configuration (cdr name-and-conf))
      (setq helm-winconf--names-alist (cons name-and-conf
                                (remove name-and-conf
                                        helm-winconf--names-alist))))))

(defun helm-winconf-switch (name)
  "Switch to the first winconf candidate."
  (helm-winconf--switch-to-cons (cons name (helm-winconf--assoc name))))

(defun helm-winconf--kill (&optional _candidate)
  "Delete the winconf candidates"
  (interactive)
  (let ((to-kill (helm-marked-candidates)))
    (dolist (name to-kill)
      (if (string= (helm-winconf--current)
                   name)
          (symbol-macrolet ((next (cadr helm-winconf--names-alist)))
            (if next
                (progn
                  (helm-winconf--switch-to-cons next)
                  (helm-winconf--removef name) t)
              nil))
        (helm-winconf--removef name) t) )))

(defvar helm-winconf--map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-D") (lambda (&optional _candidate)
                                  (interactive)
                                  (helm-quit-and-execute-action
                                   #'helm-winconf--kill)))
    map))

(defvar helm-winconf--source-not-found
  (helm-build-dummy-source
      "New window configuration"
    :action (helm-make-actions
             "New winconf" #'helm-winconf--new)))

(defvar helm-winconf--source
  (helm-build-sync-source "Window configurations"
    :candidates (lambda () (helm-winconf--candidates))
    :candidate-transformer #'helm-winconf--candidate-transformer
    :action (helm-make-actions
             "Switch to winconf" #'helm-winconf-switch
             "Kill winconf(s) `M-D'" #'helm-winconf--kill)
    :keymap helm-winconf--map))

(defvar helm-winconf--sources (list helm-winconf--source
                        helm-winconf--source-not-found))

(defun helm-winconf ()
  (interactive)
  (helm :sources helm-winconf--sources))

(defun helm-winconf-swap ()
  (interactive)
  (helm-winconf--switch-to-cons (cadr helm-winconf--names-alist)))

(provide 'helm-winconf)
