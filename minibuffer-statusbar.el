;;; minibuffer-statusbar.el --- Display a status bar in the minibuffer window  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Akira Kyle <akira@akirakyle.com>

;; Author: Akira Kyle <akira@akirakyle.com>
;; Keywords: minibuffer statusbar
;; Version: 0.1
;; Package-Requires: ((posframe "3.0.0"))

;; This file is NOT part of GNU Emacs.

;;; License:

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

;; Display a statusbar in the bottom right of the minibuffer when the minibuffer
;; window is not already used for other things

;; See README.md for more details

;;; Code:

(require 'all-the-icons)

;;; Customization

(defgroup minibuffer-statusbar ()
  "Use the idle minibuffer window to display a statusbar."
  :prefix minibuffer-statusbar
  :group 'convenience)

(defcustom minibuffer-statusbar-items 
  '((time :icon ""
          :repeat 1
          :function (lambda ()
                      (format-time-string "%Y-%m-%d • %I:%M:%S %p" (current-time)))
          :properties '('face '(:underline "red")))
    (cpu-freq :icon ""
              :repeat 1
              :function minibuffer-statusbar--cpu-freq
              :properties '('face 'italic))
    (cpu-temp :icon ""
              :repeat 1
              :function minibuffer-statusbar--cpu-temp
              :properties '('face 'italic))
    (battery :icon ""
             :repeat 1
             :function minibuffer-statusbar--battery
             :properties '('face '(:underline "green"))))
  "blah"
  :type '(alist :tag "statusbar item definitions"
		:value-type (plist)))

(defcustom minibuffer-statusbar-line '(battery " | " cpu-freq " | " cpu-temp " | " time)
  "blah"
  :type 'list)

(defcustom minibuffer-statusbar-refresh-interval 1
  "The frequency at which the minibuffer-statusbar is updated, in seconds."
  :type 'integer)

;;; Variables

(defconst minibuffer-statusbar--buffer " *Minibuf-0*")

(defvar minibuffer-statusbar--timers nil)
(defvar minibuffer-statusbar--strings nil)

;;; Private helper functions
(defun minibuffer-statusbar--battery ()
  (concat (string-trim
           (shell-command-to-string
            "cat /sys/class/power_supply/cw2015-battery/capacity"))
           "%"))

(defun minibuffer-statusbar--cpu-temp ()
  (concat (substring
                  (string-trim
                   (shell-command-to-string
                    "cat /sys/class/thermal/thermal_zone0/temp"))
                  0 -3)
                 "°C"))

(defun minibuffer-statusbar--cpu-freq ()
         (concat (number-to-string
                  (let ((a (split-string (shell-command-to-string
                                          "grep 'cpu ' /proc/stat"))))
                    (/ (* (+ (string-to-number (nth 1 a))
                             (string-to-number (nth 3 a)))
                          100)
                       (+ (string-to-number (nth 1 a))
                          (string-to-number (nth 3 a))
                          (string-to-number (nth 4 a))))))
                 "%"))

(defun minibuffer-statusbar--volume ()
  (pulseaudio-control--get-current-volume))

(defun minibuffer-statusbar--update-item (strcons icon fn props)
  (lambda ()
    (setcar strcons (concat icon " " (apply 'propertize (funcall fn) props)))))

(defun minibuffer-statusbar--start-timers ()
  (dolist (item (reverse minibuffer-statusbar-line))
    (if (stringp item)
        (push item minibuffer-statusbar--strings)
      (push "" minibuffer-statusbar--strings)
      (let* ((opts (cdr (assq item minibuffer-statusbar-items)))
             (fn (minibuffer-statusbar--update-item
                  minibuffer-statusbar--strings
                  (plist-get opts :icon)
                  (plist-get opts :function)
                  (plist-get opts :properties))))
        (push (run-at-time nil (plist-get opts :repeat) fn)
              minibuffer-statusbar--timers)))))

(defun minibuffer-statusbar--update ()
  (with-current-buffer minibuffer-statusbar--buffer
    (erase-buffer)
    (insert 
     (let ((str (apply 'concat minibuffer-statusbar--strings)))
       (concat (make-string (- (frame-text-cols)
                               (string-width str)) ? )
               str)))))

;;;###autoload
(define-minor-mode minibuffer-statusbar-mode
  "Display status info in the minibuffer window."
  :global t
  (with-current-buffer minibuffer-statusbar--buffer
    (erase-buffer))
  (when minibuffer-statusbar--timers
    (dolist (timer minibuffer-statusbar--timers) 
      (cancel-timer timer))
    (setq minibuffer-statusbar--timers nil))
  (setq minibuffer-statusbar--strings nil)
  (if minibuffer-statusbar-mode
      (let ((timer (run-at-time nil minibuffer-statusbar-refresh-interval
                                'minibuffer-statusbar--update)))
        (push timer minibuffer-statusbar--timers)
        (minibuffer-statusbar--start-timers))
    (dolist (timer minibuffer-statusbar--timers) 
      (cancel-timer timer))
    (setq minibuffer-statusbar--timers nil)
    (setq minibuffer-statusbar--strings nil)))

(provide 'minibuffer-statusbar)
;;; minibuffer-statusbar.el ends here
