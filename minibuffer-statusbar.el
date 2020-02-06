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
  :prefix "minibuffer-statusbar-"
  :group 'convenience)

(defcustom minibuffer-statusbar-line
  '((minibuffer-statusbar--battery . 30) " | "
    (minibuffer-statusbar--cpu-freq . 3) " | "
    (minibuffer-statusbar--cpu-temp . 10) " | "
    ((lambda ()
       (propertize
        (concat " " (format-time-string "%Y-%m-%d • %I:%M:%S %p" (current-time)))
        'face '(:underline "red"))) . 1))
  "blah"
  :type 'list)

;;; Variables

(defconst minibuffer-statusbar--buffer " *Minibuf-0*")

(defvar minibuffer-statusbar--timers nil)
(defvar minibuffer-statusbar--strings nil)

;;; Private helper functions
(defun minibuffer-statusbar--file-to-string (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun minibuffer-statusbar--battery ()
  "get battery capacity"
  (propertize
   (concat " " 
           (string-trim
            (minibuffer-statusbar--file-to-string 
             "/sys/class/power_supply/cw2015-battery/capacity"))
           "%")
   'face '(:underline "green")))

(defun minibuffer-statusbar--cpu-temp ()
  "get cpu temperature in deg C"
  (propertize
   (concat " "
           (substring (string-trim
                       (minibuffer-statusbar--file-to-string 
                        "/sys/class/thermal/thermal_zone0/temp"))
                      0 -3) "°C")
   'face '(:underline "blue")))

(defun minibuffer-statusbar--cpu-freq ()
  ;(propertize
   (concat " "
           (number-to-string
            (let ((a (split-string (shell-command-to-string
                                    "grep 'cpu ' /proc/stat"))))
              (/ (* (+ (string-to-number (nth 1 a))
                       (string-to-number (nth 3 a)))
                    100)
                 (+ (string-to-number (nth 1 a))
                    (string-to-number (nth 3 a))
                    (string-to-number (nth 4 a))))))
           "%"))
   ;'face '(:underline "yellow")))

(defun minibuffer-statusbar--volume ()
  (pulseaudio-control--get-current-volume))


(defun minibuffer-statusbar--update-item (fn strcons)
  (lambda () (setcar strcons (funcall fn))))

(defun minibuffer-statusbar--start-timers ()
  (dolist (item (reverse minibuffer-statusbar-line))
    (if (stringp item)
        (push item minibuffer-statusbar--strings)
      (push "" minibuffer-statusbar--strings)
      (let ((fn (minibuffer-statusbar--update-item
                 (car item) minibuffer-statusbar--strings))
            (repeat (cdr item)))
      (push (run-at-time nil repeat fn) minibuffer-statusbar--timers)))))

(defun minibuffer-statusbar--update ()
  (with-current-buffer minibuffer-statusbar--buffer
    (erase-buffer)
    (insert 
     (let ((str (apply 'concat minibuffer-statusbar--strings)))
       (concat (make-string (- (frame-text-cols)
                               (+ (string-width str) 4)) ? ) ;; right pad 2 spaces
               str)))))

(defun minibuffer-statusbar--refresh-interval ()
  (seq-reduce
   (lambda (a b) (cond ((and (numberp a) (numberp b)) (min a b))
                       ((numberp a) a)
                       ((numberp b) b)
                       (t nil)))
   (seq-map (lambda (item) (if (stringp item) nil (cdr item)))
            minibuffer-statusbar-line)
   nil))

;;;###autoload
(define-minor-mode minibuffer-statusbar-mode
  "Display status info in the minibuffer window."
  :global t
  (with-current-buffer minibuffer-statusbar--buffer
    (erase-buffer))
  (when minibuffer-statusbar--timers
    (seq-map #'cancel-timer minibuffer-statusbar--timers)
    (setq minibuffer-statusbar--timers nil))
  (setq minibuffer-statusbar--strings nil)
  (cond (minibuffer-statusbar-mode
         (minibuffer-statusbar--start-timers)
         (push (run-at-time nil (minibuffer-statusbar--refresh-interval)
                            'minibuffer-statusbar--update)
               minibuffer-statusbar--timers))
        (t (seq-map #'cancel-timer minibuffer-statusbar--timers)
           (setq minibuffer-statusbar--timers nil)
           (setq minibuffer-statusbar--strings nil))))

(provide 'minibuffer-statusbar)
;;; minibuffer-statusbar.el ends here
