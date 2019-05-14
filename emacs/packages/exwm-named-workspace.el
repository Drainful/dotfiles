;;; exwm-named-workspace.el                     -*- lexical-binding: t; -*-
;; Copyright (C) 2019 Adrian Fullmer

;; Author: Adrian Fullmer <adrianfullmer@yahoo.com>
;; Keywords: exwm workspace
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

;;; Code:

(provide 'exwm-named-workspace)
(require 'exwm-named-workspace)
(defconst exwm-named-workspace--default-workspace-name  "default-workspace")
(defvar exwm-named-workspace--names (list exwm-named-workspace--default-workspace-name)
  "List of names of workspaces, nil for unnamed.")
(defvar exwm-named-workspace--history (list 0)
  "List of all workspace indicies. Earliest in this list is
  most recently visited")

(setq exwm-named-workspace-index-map
      (lambda (index)
        (or (exwm-named-workspace--index-to-name index)
            (number-to-string index))))

(setq exwm-workspace-index-map
      exwm-named-workspace-index-map)

(defun exwm-named-workspace--index-names ()
  (mapcar exwm-named-workspace-index-map
          (number-sequence 0 (1- (exwm-workspace--count)))))

(defun exwm-named-workspace--current-index ()
  (exwm-workspace--position (selected-frame)))

(defun exwm-named-workspace--current-name ()
  (exwm-named-workspace--index-to-name (exwm-named-workspace--current-index)))

(cl-defun exwm-named-workspace-read (&key prompt without-default-workspace order)
  (let* ((workspace-list (if without-default-workspace
                             (remove exwm-named-workspace--default-workspace-name
                                     (exwm-named-workspace--index-names))
                           (exwm-named-workspace--index-names)))
         (ordered-workspace-list (cond ((equal order :last-visited)
                                        (mapcar exwm-named-workspace-index-map exwm-named-workspace--history))
                                       ((equal order :last-visited-current-last)
                                        (mapcar exwm-named-workspace-index-map
                                                (append (remove (exwm-named-workspace--current-index)
                                                                exwm-named-workspace--history)
                                                        (list (exwm-named-workspace--current-index)))))
                                       ((equal order :newest-first) (reverse workspace-list))
                                       ((equal order :oldest-first) workspace-list)
                                       (t workspace-list))))
    (completing-read (or prompt (format "Select Workspace (current: [%s]): "
                                        (exwm-named-workspace--current-name)))
                     ordered-workspace-list)))

(cl-defun exwm-named-workspace--switch (name-or-index &key (update-history t))
  (unless (exwm-named-workspace-handle-valid-p
           name-or-index)
    (error (format "Workspace %s does not exist" name-or-index)))
  (exwm-workspace-switch (exwm-named-workspace--name-to-index name-or-index))
  (when update-history
    (setq exwm-named-workspace--history
          (remove (exwm-named-workspace--name-to-index name-or-index) exwm-named-workspace--history))
    (add-to-list 'exwm-named-workspace--history (exwm-named-workspace--name-to-index name-or-index))))

(defun exwm-named-workspace-switch (workspace)
  (interactive (list (exwm-named-workspace-read :order :last-visited-current-last)))
  (exwm-named-workspace--switch workspace))

(cl-defun exwm-named-workspace--make (&optional (name nil))
  (cond ((string-to-integer-or-nil name)
         (error "Cannot use a number as the workspace name"))
        ((find-if (lambda (s) (string= s name)) exwm-named-workspace--names)
         (error (format "The workspace name '%s' is already in use" name)))
        (t (exwm-workspace-switch 0)
           (exwm-workspace-add)
           (add-to-list 'exwm-named-workspace--history (exwm-named-workspace--current-index))
           (add-to-list 'exwm-named-workspace--names name t))))

(defun exwm-named-workspace-make (name)
  (interactive  "sWorkspace Name: ")
  (exwm-named-workspace--make name))

(defun exwm-named-workspace-make-or-switch (name)
  (interactive "sWorkspace Name: ")
  (if (find name exwm-named-workspace--names)
      (exwm-named-workspace-switch name)
    (exwm-named-workspace-make name)))

(defun exwm-named-workspace--delete (name-or-index)
  (cond
   ((not (exwm-named-workspace-handle-valid-p name-or-index))
    (error (format "Workspace %s does not exist" name-or-index)))
   ((or (when (stringp name-or-index)
          (string= name-or-index exwm-named-workspace--default-workspace-name))
        (when (numberp name-or-index)
          (= name-or-index 0)))
    (error "Cannot delete the default workspace."))
   (t
    (if (equal (exwm-named-workspace--name-to-index name-or-index)
               (exwm-named-workspace--current-index))
        (exwm-named-workspace--switch (second exwm-named-workspace--history)))
    (setq exwm-named-workspace--history
          (remove (exwm-named-workspace--name-to-index name-or-index) exwm-named-workspace--history))
    (setq exwm-named-workspace--names
          (remove-if (lambda (s)
                       (string= s (exwm-named-workspace--index-to-name name-or-index)))
                     exwm-named-workspace--names))
    (exwm-workspace-delete (exwm-named-workspace--name-to-index name-or-index)))))

(defun exwm-named-workspace-delete (workspace)
  (interactive (list (exwm-named-workspace-read :without-default-workspace t
                                       :order :newest-first)))
  (exwm-named-workspace--delete workspace))

(defun exwm-named-workspace-next ()
  "Switch to the next workspace in order of creation"
  (interactive)
  (exwm-named-workspace--switch (1+ (exwm-named-workspace--current-index)))
  (message (format "Current Workspace: [%s]" (exwm-named-workspace--current-name))))

(defun exwm-named-workspace-previous ()
  "Switch to the previous workspace in order of creation"
  (interactive)
  (exwm-named-workspace--switch (1- (exwm-named-workspace--current-index)))
  (message (format "Current Workspace: [%s]" (exwm-named-workspace--current-name))))

(defun exwm-named-workspace--index-to-name (name-or-index)
  (if (numberp name-or-index)
      (nth name-or-index exwm-named-workspace--names)
    name-or-index))

(defun exwm-named-workspace--name-to-index (name-or-index)
  (if (numberp name-or-index)
      name-or-index
    (or (string-to-integer-or-nil name-or-index)
        (position-if (lambda (s) (string= s name-or-index))
                     exwm-named-workspace--names))))

(defun exwm-named-workspace-handle-valid-p (name-or-index)
  (if (numberp name-or-index)
      (> (exwm-workspace--count) name-or-index)
    (find-if (lambda (s) (string= s name-or-index))
             (exwm-named-workspace--index-names))))
