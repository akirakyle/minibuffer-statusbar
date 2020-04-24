;;; minibuffer-statusbar.el --- Display a status bar in the minibuffer window  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Akira Kyle <akira@akirakyle.com>

;; Author: Akira Kyle <akira@akirakyle.com>
;; Keywords: minibuffer statusbar
;; Version: 0.1
;; Package-Requires: ((all-the-icons "3.0.0"))

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

;; Display a statusbar in the minibuffer when the minibuffer window is not
;; already used for other things

;; See README.org for more details

;;; Code:

(require 'all-the-icons)

;;; Customization

(defgroup minibuffer-statusbar ()
  "Use the idle minibuffer window to display a statusbar."
  :prefix "minibuffer-statusbar-"
  :group 'convenience)

(defcustom minibuffer-statusbar-line
  '( "  "
    (minibuffer-statusbar--disk-free . 30) "   "
    (minibuffer-statusbar--battery . 30)
    "                                                   "
    (minibuffer-statusbar--memory . 5) "   "
    (minibuffer-statusbar--cpu-freq . 3) "   "
    (minibuffer-statusbar--cpu-temp . 10)
    "                                                 "
    (minibuffer-statusbar--time . 60) "   "
    (minibuffer-statusbar--date . 3600))
  "List items to show on the statusbar line list

items can either be a string in which case it is inserted as is
or a list where the car is a function to run and the cdr is the
interval in seconds."
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

(defun minibuffer-statusbar--date ()
  "get date"
  (format "%s %s" (all-the-icons-faicon "calendar" :height 0.8 :v-adjust 0.0)
          (format-time-string "%Y-%m-%d %a" (current-time))))

(defun minibuffer-statusbar--time ()
  "get time"
  (format "%s %s" (all-the-icons-faicon "clock-o" :height 0.8 :v-adjust 0.0)
          (format-time-string "%I:%M %p" (current-time))))

;; consider using `file-system-info' in emacs 27
(defun minibuffer-statusbar--disk-free ()
  "get disk usage"
  (let* ((out (shell-command-to-string "df -h --output=used,size /"))
         (strs (split-string (nth 1 (split-string out "\n"))))
         (icon (all-the-icons-material "data_usage" :height 0.8 :v-adjust 0.0)))
    (seq-let (used size) strs
      (format "%s %s/%s" icon used size))))

(defun minibuffer-statusbar--memory ()
  "get memory usage"
  (let ((parsed (minibuffer-statusbar--file-parser
                 "/proc/meminfo" 'string-to-number
                 '("MemTotal:" "MemAvailable:" "SwapTotal:" "SwapFree:"))))
    (seq-let (tot avail swp-tot swp-free) parsed
      (let* ((icon (all-the-icons-material "memory" :height 0.8 :v-adjust -0.1))
             (mem (/ (* (- tot avail) 100) tot))
             (swp (/ (- swp-tot swp-free) 1000))
             (swp-str (if (zerop swp) "" (format " %dMB Swapped" swp)))
             (str (format "%s %d%%%s" icon mem swp-str)))
        (if (> mem 80)
            (add-face-text-property 0 (length str) '(:foreground "red") nil str))
        (if (> mem 90)
            (message "%s memory at %d%%!!!"
                     (all-the-icons-faicon "exclamation-triangle" :v-adjust 0.0)
                 mem))
        str))))

(defun minibuffer-statusbar--battery ()
  "get battery capacity"
  (let* ((path "/sys/class/power_supply/cw2015-battery/")
         (bat (string-to-number (minibuffer-statusbar--file-to-string
                                 (concat path "capacity"))))
         (status (string-trim (minibuffer-statusbar--file-to-string
                               (concat path "status"))))
         (i-chg (all-the-icons-alltheicon
                 "battery-charging" :height 1.0 :v-adjust 0.0))
         (i-100 (all-the-icons-faicon
                 "battery-full" :height 0.8 :v-adjust 0.0))
         (i-75 (all-the-icons-faicon
                "battery-three-quarters" :height 0.8 :v-adjust 0.0))
         (i-50 (all-the-icons-faicon
                "battery-half" :height 0.8 :v-adjust 0.0))
         (i-25 (all-the-icons-faicon
                "battery-quarter" :height 0.8 :v-adjust 0.0))
         (i-0 (all-the-icons-faicon
               "battery-empty" :height 0.8 :v-adjust 0.0))
         (icon (cond ((string= status "Charging") i-chg)
                     ((> bat 88) i-100)
                     ((> bat 63) i-75) 
                     ((> bat 38) i-50) 
                     ((> bat 15) i-25) 
                     (t          i-0)))
         (str (format "%s %d%%" icon bat)))
    (if (<= bat 5)
        (message "%s battery at %d%%!!!"
                 (all-the-icons-faicon "exclamation-triangle" :v-adjust 0.0)
                 bat))
    (if (<= bat 15) 
        (add-face-text-property 0 (length str) '(:foreground "red") nil str))
    str))

(defun minibuffer-statusbar--cpu-temp ()
  "get cpu temperature in deg C"
  (let* ((icon (all-the-icons-wicon "thermometer" :height 0.6 :v-adjust -0.0))
         (temp-str (minibuffer-statusbar--file-to-string
                    "/sys/class/thermal/thermal_zone0/temp"))
         (temp (/ (string-to-number temp-str) 1000))
         (str (format "%s %d°C" icon temp)))
    (if (> temp 65)
        (add-face-text-property 0 (length str) '(:foreground "red") nil str))
      str))

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
                     (cpu-str (if (string= "" cpu) (all-the-icons-faicon
                                                    "tachometer" :height 0.8 :v-adjust 0.0)
                                (concat cpu ":")))
                     (diff-used (- (nth 1 curr) (nth 1 prev)))
                     (diff-idle (- (nth 2 curr) (nth 2 prev)))
                     (diff-tot (+ diff-used diff-idle))
                     (percent (if (= diff-tot 0) 0 (/ (* 100 diff-used) diff-tot))))
                (format "%s%2d%%" cpu-str percent))))
           (strs (seq-mapn fmt-percent-fn cpus minibuffer-statusbar--prev-cpus)))
      (setq minibuffer-statusbar--prev-cpus cpus)
      (mapconcat 'identity strs " "))))

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
    (insert (apply 'concat minibuffer-statusbar--strings))))
    ;;(insert 
    ;; (let ((str (apply 'concat minibuffer-statusbar--strings)))
    ;;   (concat (make-string (/ (- (frame-text-cols) (string-width str)) 2) ? )
    ;;   ;;(concat (make-string (- (frame-text-cols)
    ;;   ;;                        (+ (string-width str) 6)) ? ) ;; right pad 6 spaces
    ;;           str)))))

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
