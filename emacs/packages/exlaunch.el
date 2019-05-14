;;; exlaunch.el -*- lexical-binding: t; -*-
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

;; My implementation of an equivalent of dmenu and the ".desktop file"
;; system for launching and switching between programs quickly

;;; Code:

(provide 'exlaunch)
(require 'exlaunch)

(defconst exlaunch-prefix "exlaunch/")

(defvar exlaunch-shortcut-functions nil
  "List of functions representing exlaunch shortcuts.")

(defun exlaunch--get-exwm-buffer (class)
  "Get the buffer of the exwm program with the given
class (`exwm-class-name')"
  (require 'exwm)
  (let ((last (buffer-list)))
    (while (and last
                (not (with-current-buffer (car last)
                       (and (eq major-mode 'exwm-mode)
                            (string= (downcase (or exwm-class-name "")) (downcase class))))))
      (setq last (cdr last)))
    (car last)))

(cl-defmacro exlaunch-shortcut (name &key shell-command args
                                     (switch-to t)
                                     exwm-class
                                     other-window
                                     before
                                     after
                                     in-named-workspace)
  "Create a function to launch the program given by NAME. 

The following keyword args are available:

SHELL-COMMAND, a string representing the shell command to use
for this program. Defaults to NAME.

ARGS, a string representing the command line arguments to use
for the program.

BEFORE and AFTER, elisp functions to run before and after the
program is launched.


If EXWM is installed, the following keywargs are available:

SWITCH-TO, if true will switch to the EXWM buffer of the running
program rather than starting a second instance. Default t.

EXWM-CLASS, the `exwm-class-name' of the program. Required for
SWITCH-TO. (not currently detected automatically, defaults to
NAME.)

OTHER-WINDOW, if true will open the program in another emacs
window.

If EXWM-NAMED-WORKSPACE is installed, the keyword
IN-NAMED-WORKSPACE can be used to run the program in the given
workspace, switch to that workspace, and create it if it does not
exist."
  (let* ((name-string (symbol-name name))
         (exwm-class (or exwm-class name-string))
         (function-symbol (intern (concat exlaunch-prefix name-string)))
         (full-shell-command (concat
                              (or shell-command name-string)
                              " " args)))
    `(progn
       (defun ,function-symbol ()
         (interactive)
         ,(when before `(funcall ,before))
         ,(if (require 'exwm nil t)
              `(progn
                 ,(when (and (require 'exwm-named-workspace nil t)
                             in-named-workspace)
                    `(exwm-named-workspace-make-or-switch ,in-named-workspace))
                 (if ,(when switch-to `(get-process ,name-string))
                     (,(if other-window
                           'switch-to-buffer-other-window
                         'switch-to-buffer)
                      (exlaunch--get-exwm-buffer ,exwm-class))
                   (progn
                     ,(when other-window '(progn (split-window-sensibly) (other-window 1)))
                     (start-process-shell-command ,name-string nil ,full-shell-command))))
            `(start-process-shell-command ,name-string nil ,full-shell-command))
         ,(when after `(funcall ,after)))
       (add-to-list 'exlaunch-shortcut-functions ',function-symbol))))

(define-multi-macro-clauses exlaunch-shortcuts exlaunch-shortcut
  "Define functions to launch programs. ")

(defun read-exlaunch ()
  (completing-read "Launch: " exlaunch-shortcut-functions))

(defun exlaunch ()
  (interactive)
  (funcall (symbol-function (intern (read-exlaunch)))))

