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

;; TODO: disk usage
;; TODO: network
;; https://github.com/tromey/emacs-network-manager/blob/master/NetworkManager.el
;; https://elpa.gnu.org/packages/enwc.html
;; https://github.com/Kodkollektivet/emacs-nm
;; https://github.com/nicferrier/emacs-nm
;; TODO: fix all the icons changing when propertized
;; TODO: maybe do backlight and pulseaudo as simple variables instead of functions

(defgroup minibuffer-statusbar ()
  "Use the idle minibuffer window to display a statusbar."
  :prefix "minibuffer-statusbar-"
  :group 'convenience)

(defcustom minibuffer-statusbar-line
  '((minibuffer-statusbar--battery . 30) " | "
    (minibuffer-statusbar--memory . 10) " | "
    (minibuffer-statusbar--cpu-freq . 3) " | "
    (minibuffer-statusbar--cpu-temp . 10) " | "
    ((lambda ()
       (propertize
        (concat " " (format-time-string "%Y-%m-%d %a • %I:%M %p" (current-time)))
        'face '(:underline "red"))) . 60))
  "blah"
  :type 'list)

;;; Variables

(defconst minibuffer-statusbar--buffer " *Minibuf-0*")

(defvar minibuffer-statusbar--timers nil)
(defvar minibuffer-statusbar--strings nil)
(defvar minibuffer-statusbar--prev-cpus nil)

;;; Private helper functions
(defun minibuffer-statusbar--file-to-string (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun minibuffer-statusbar--file-parser (path parser prefixes)
  (with-temp-buffer
    (insert-file-contents path)
    (mapcar (lambda (prefix)
              (save-excursion
                (re-search-forward (concat "^" prefix "\\(.*\\)$"))
                (funcall parser (match-string 1))))
            prefixes)))

(defun minibuffer-statusbar--memory ()
  "get memory usage"
  (let ((parsed (minibuffer-statusbar--file-parser
                 "/proc/meminfo" 'string-to-number
                 '("MemTotal:" "MemAvailable:" "SwapTotal:" "SwapFree:"))))
    (seq-let (tot avail swp-tot swp-free) parsed
      (let* ((mem (/ (* (- tot avail) 100) tot))
             (swp (/ (- swp-tot swp-free) 1000))
             (swp-str (if (zerop swp) "" (format " %dMB Swapped" swp))))
        (propertize (format " %d%%%s" mem swp-str)
                    'face '(:underline "green"))))))

(defun minibuffer-statusbar--battery ()
  "get battery capacity"
  ;; TODO make icon follow capacity
  ;; TODO message when battery low
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

;; https://stackoverflow.com/questions/23367857/accurate-calculation-of-cpu-usage-given-in-percentage-in-linux
(defun minibuffer-statusbar--cpu-freq ()
  "get cpu usage"
  (let ((cpus nil))
    (with-temp-buffer
      (insert-file-contents "/proc/stat")
      (while (re-search-forward "^cpu\\([[:digit:]]*\\) \\(.*\\)$" nil t)
        (let* ((cpu-str (match-string 1))
               (stats-strs (match-string 2))
               (stats (mapcar 'string-to-number (split-string stats-strs))))
          (seq-let (user nice system idle iowait irq softirq steal
                         guest guest-nice) stats
            (push (list cpu-str
                        (+ user nice system irq softirq steal)
                        (+ idle iowait))
                  cpus)))))
    (let* ((cpus (reverse cpus))
           (fmt-percent-fn
            (lambda (curr prev)
              (if (not (string= (car curr) (car prev)))
                  (error "Cpu order changed? curr:%s prev:%s" (car curr) (car prev)))
              (let* ((cpu (car curr))
                     (cpu-str (if (string= "" cpu) " " (concat " "cpu ":")))
                     (diff-used (- (nth 1 curr) (nth 1 prev)))
                     (diff-idle (- (nth 2 curr) (nth 2 prev)))
                     (percent (/ (* 100 diff-used) (+ diff-used diff-idle))))
                (format "%s%2d%%" cpu-str percent))))
           (strs (seq-mapn fmt-percent-fn cpus minibuffer-statusbar--prev-cpus)))
      (setq minibuffer-statusbar--prev-cpus cpus)
      (propertize (apply 'concat strs) 'face '(:underline "green")))))

(defun minibuffer-statusbar--volume ()
  (pulseaudio-control--get-current-volume))

(defun minibuffer-statusbar--brightness ())
  ;;(pulseaudio-control--get-current-volume))

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
