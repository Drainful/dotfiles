;;; exwm-named-workspace.el -*- lexical-binding: t; -*-
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

;; Exwm has only numbered workspaces, this package extends that
;; functionality to include named workspaces.

;; TODO handle more than 1 starting workspace and provide a method to
;; change the default workspace names.

;; TODO handle multiple workspaces being initialized with the same
;; name (without using exwm named workspace functions.)

;; TODO and alist (name . (list functions)), like hooks for the
;; creation of the workspace with that name. Could also do the same
;; for entering that workspace.

;;; Code:

(require 'exwm)
(require 'cl-lib)

(defvar exwm-named-workspace--names nil
  "Alist of names of workspaces, (name . emacs frame).")

(defvar exwm-named-workspace--history (cl-copy-list exwm-workspace--list)
  "List of all workspaces. Earliest in this list is
  most recently visited")

(defun exwm-named-workspace--name->frame (name)
  (cdr (or (assoc name exwm-named-workspace--names)
           (error (format "There is no workspace with the name %s" name)))))

(defun exwm-named-workspace--frame->name (frame)
  (car (rassoc frame exwm-named-workspace--names)))

(defun exwm-named-workspace--frame->index (frame)
  (exwm-workspace--position frame))

(defun exwm-named-workspace--index->frame (index)
  (nth index exwm-workspace--list))

(defun exwm-named-workspace--index->name (index)
  (car (rassoc (exwm-named-workspace--index->frame index)
               exwm-named-workspace--names)))

(defun exwm-named-workspace--current-index ()
  (exwm-named-workspace--frame->index (selected-frame)))

(defun exwm-named-workspace--current-name ()
  (exwm-named-workspace--frame->name (selected-frame)))

(defun exwm-named-workspace--name-exists-p (name)
  (cl-find-if (lambda (s) (string= (car s) name)) exwm-named-workspace--names))

(defvar exwm-named-workspace--old-workspace-list (cl-copy-list exwm-workspace--list)
  "The list of workspaces before changes are made. Used for the
  `exwm-workspace-list-change-hook'.")

(defconst exwm-named-workspace--default-workspace-name "default-workspace")

(defvar exwm-named-workspace--new-name exwm-named-workspace--default-workspace-name
  "The name that will be given to the next workspace created by
  `exwm-workspace-add'. For internal use only.")

(defun exwm-named-workspace--post-change ()
  "This function is added to the
`exwm-workspace-list-change-hook' to keep track of names."
  (let* ((new-workspace (cl-find-if (lambda (frame)
                                      (and
                                       (exwm-workspace--workspace-from-frame-or-index frame)
                                       (not (cl-find frame
                                                     exwm-named-workspace--old-workspace-list))))
                                    exwm-workspace--list))
         (deleted-workspace (unless new-workspace
                              (cl-find-if (lambda (frame)
                                            (not (cl-find frame
                                                          exwm-workspace--list)))
                                          exwm-named-workspace--old-workspace-list))))
    (when new-workspace
      ;; (assert (equal (selected-frame) new-workspace))
      ;; (let ((exwm-named-workspace--new-name (labels (())
      ;;                       (if (cl-find-if (lambda (cell)
      ;;                                  (string= exwm-named-workspace--new-name
      ;;                                           (car cell)))
      ;;                                exwm-named-workspace--names)
      ;;                       )))))
      (setq exwm-named-workspace--names (cons (cons exwm-named-workspace--new-name new-workspace)
                          exwm-named-workspace--names))
      (add-to-list 'exwm-named-workspace--history new-workspace))
    (when deleted-workspace
      (setq exwm-named-workspace--names
            (cl-remove-if (lambda (w) (equal deleted-workspace
                                             (cdr w)))
                          exwm-named-workspace--names))
      (setq exwm-named-workspace--history
            (remove deleted-workspace exwm-named-workspace--history))))
  (setq exwm-named-workspace--old-workspace-list (cl-copy-list exwm-workspace--list)))

(add-hook 'exwm-workspace-list-change-hook 'exwm-named-workspace--post-change)

(defconst exwm-named-workspace--history-norecord nil
  "When non-nil, do not put recently switched workspaces on top
  of the history stack.")

(defun exwm-named-workspace-update-history ()
  "Unless the global variable NORECORD is non-nil, put the
current workspace on top of the history stack."

  ;; (when exwm-named-workspace--history-norecord (warn "history not updated. frame: %s" (exwm-named-workspace--frame->name (selected-frame)))) ; DEBUG
  (unless exwm-named-workspace--history-norecord
    ;; (warn (format "history updated. frame: %s" (exwm-named-workspace--frame->name (selected-frame)))) ; DEBUG
    (let ((current-frame (selected-frame)))
      (when (cl-find current-frame exwm-named-workspace--history)
        (setq exwm-named-workspace--history (remove current-frame exwm-named-workspace--history))
        (add-to-list 'exwm-named-workspace--history current-frame)))))

(add-hook 'exwm-workspace-switch-hook #'exwm-named-workspace-update-history)

(defvar exwm-named-workspace-message-on-switch nil
  "If non nil, create a message notifying you of the current
  workspace on switch.")

(defun exwm-named-workspace--current-workspace-msg ()
  (message (format "Current Workspace: [%s]" (exwm-named-workspace--current-name))))

(add-hook 'exwm-workspace-switch-hook
          (lambda ()
            (when exwm-named-workspace-message-on-switch
              (exwm-named-workspace--current-workspace-msg))))

(setq exwm-workspace-index-map
      (lambda (index)
        (or (concat "\"" (exwm-named-workspace--index->name index) "\"")
            (number-to-string index))))

(defun exwm-named-workspace--remove-strings (strings list)
  (mapcar (lambda (string)
            (cl-remove-if (lambda (s) (string= string s))
                          list))
          strings))

(cl-defun exwm-named-workspace-read (&key prompt
                     ;; without-default-workspaces
                      current-last)
  "Read the name of a workspace from the user."
  (let* ((names (mapcar 'exwm-named-workspace--frame->name exwm-named-workspace--history))
         ;; (names (if without-default-workspaces
         ;;            (exwm-named-workspace--remove-strings exwm-named-workspace--default-workspace-names names)
         ;;          names))
         (names (if current-last
                    (append (remove (exwm-named-workspace--current-name)
                                       names)
                            (list (exwm-named-workspace--current-name)))
                  names)))
    (completing-read (or prompt (format "Select Workspace (current: [%s]): "
                                        (exwm-named-workspace--current-name)))
                     names)))

(cl-defun exwm-named-workspace--switch (name &optional norecord)
  "Switch to the workspace represented by NAME.
 If norecord is non-nil, do not update the history."
  (let ((exwm-named-workspace--history-norecord norecord))
    (exwm-workspace-switch (exwm-named-workspace--name->frame name))))

(defun exwm-named-workspace-switch (workspace-name)
  "Switch to the workspace with string name WORKSPACE."
  (interactive (list (exwm-named-workspace-read :current-last t)))
  (exwm-named-workspace--switch workspace-name))

(cl-defun exwm-named-workspace-make (name &optional (from 0))
  (interactive "sWorkspace Name: ")
  "Make a new workspace with name NAME, first switching to the
workspace with index FROM (so that x11 windows do not get stolen
from the current workspace)"
  (if (exwm-named-workspace--name-exists-p name)
      (error (format "The workspace name '%s' is already in use" name))
    (progn (let ((exwm-named-workspace--history-norecord t)
                 (exwm-named-workspace--new-name name))
             (exwm-workspace-switch from)
             (exwm-workspace-add)
             (exwm-named-workspace-update-history)))))

(defun exwm-named-workspace-make-or-switch (name)
  (interactive (list (exwm-named-workspace-read :current-last t)))
  (if (exwm-named-workspace--name-exists-p name)
      (exwm-named-workspace-switch name)
    (exwm-named-workspace-make name)))

(defun exwm-named-workspace-delete (workspace)
  "Delete the workspace with the given name. You may use either
this or `exwm-workspace-delete' to delete your
workspaces. Interactive."
  (interactive (list (exwm-named-workspace-read)))
  (exwm-workspace-delete (exwm-named-workspace--name->frame workspace)))

(defun exwm-named-workspace--circular-copy (list)
  "Return a copy of the given list where the last element points
  to the first, rather than to nil."
  (when list
    (let ((new-list (cl-copy-list list)))
      (setf (cdr (last new-list))
            new-list))))

(defun exwm-named-workspace-history (n &optional norecord)
  "Switch to the Nth element of the workspace history
circularly. If norecord is non-nil, do not update the history."
  (let ((exwm-named-workspace-message-on-switch t))
    (exwm-named-workspace--switch (exwm-named-workspace--frame->name
                                   (nth n (exwm-named-workspace--circular-copy exwm-named-workspace--history)))
                                  norecord)))

(provide 'exwm-named-workspace)
