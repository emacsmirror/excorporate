;;; excorporate-org.el --- Exchange Org Mode view     -*- lexical-binding: t -*-

;; Copyright (C) 2016-2019 Free Software Foundation, Inc.

;; Author: Thomas Fitzsimmons <fitzsim@fitzsim.org>
;; Keywords: calendar

;; This program is free software: you can redistribute it and/or modify
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

;; Use Org Mode to display daily meetings.

;;; Code:

(require 'org)
(require 'excorporate)

(defvar excorporate-org-buffer-name "*Excorporate*"
  "The buffer into which Org Mode output is inserted.")

(defvar exco-org--temporary-buffers '()
  "A list of per-connection result buffers.")

(defun exco-org-initialize-buffer ()
  "Add initial text to the destination buffer."
  (setq exco-org--temporary-buffers '())
  (with-current-buffer (get-buffer-create excorporate-org-buffer-name)
      (setq buffer-read-only t)
      ;; Some Org mode configurations need `buffer-file-name' to be
      ;; non-nil, or they'll make `org-mode' error out, for example
      ;; `org-startup-with-latex-preview'.  Set `buffer-file-name' to
      ;; something non-nil temporarily during initialization.  Don't
      ;; leave it set or `save-some-buffers' will always prompt about
      ;; *Excorporate*.
      (let ((buffer-file-name excorporate-org-buffer-name))
	(org-mode))
      (use-local-map (copy-keymap org-mode-map))
      (local-set-key "q" 'quit-window)
      (display-buffer (current-buffer))
      (let ((inhibit-read-only t))
	(delete-region (point-min) (point-max))
	(goto-char (point-min))
	(insert "# Updated...\n"))))

(defun exco-org-format-headline (identifier)
  "Format an Org headline using IDENTIFIER."
  (format "* Calendar (%s)\n" identifier))

(defun exco-org-insert-meeting-headline (subject start-time end-time)
  "Insert and schedule a meeting.
SUBJECT is the meeting's subject, START-TIME and END-TIME are the
meeting's start and end times in the same format as is returned
by `current-time'."
  (let* ((now (current-time))
	 (keyword (if (time-less-p now end-time)
		      "TODO"
		    "DONE")))
    (insert (format "** %s %s\n" keyword subject))
    (org-schedule nil (format-time-string "<%Y-%m-%d %a %H:%M>"
					  start-time))
    (forward-line -1)
    (end-of-line)
    (insert  "--" (format-time-string "<%Y-%m-%d %a %H:%M>" end-time))
    (forward-line)
    (org-insert-time-stamp (current-time) t t "+ Retrieved " "\n")))

(defun exco-org-insert-invitees (invitees)
  "Parse and insert a list of invitees, INVITEES."
  (dolist (invitee invitees)
    (insert (format "  + %s\n" invitee))))

(defun exco-org--identifier-buffer (identifier)
  "Return a hidden buffer with a name based on IDENTIFIER."
  (get-buffer-create
   (format " *exco-org-%S*" identifier)))

(defun exco-org-insert-headline (identifier month day year)
  "Insert Org headline for IDENTIFIER on date MONTH DAY YEAR."
  (let ((temporary-buffer (exco-org--identifier-buffer identifier)))
    (push temporary-buffer exco-org--temporary-buffers)
    (with-current-buffer temporary-buffer
      (let ((inhibit-read-only t))
	(delete-region (point-min) (point-max))
	(insert (exco-org-format-headline identifier))
	(org-insert-time-stamp (encode-time 0 0 0 day month year)
			       nil t "  + Date " "\n")))))

(defun exco-org-insert-meeting (subject start end location
					main-invitees optional-invitees)
  "Insert a scheduled meeting.
SUBJECT is a string, the subject of the meeting.  START is the
meeting start time in Emacs internal date time format, and END is
the end of the meeting in the same format.  LOCATION is a string
representing the location.  MAIN-INVITEES and OPTIONAL-INVITEES
are the requested participants."
  (exco-org-insert-meeting-headline subject start end)
  (insert (format "+ Duration: %d minutes\n"
		  (round (/ (float-time (time-subtract end start)) 60.0))))
  (insert (format "+ Location: %s\n" location))
  (when main-invitees
    (insert "+ Invitees:\n")
    (exco-org-insert-invitees main-invitees))
  (when optional-invitees
    (insert "+ Optional invitees:\n")
    (exco-org-insert-invitees optional-invitees)))

(defun exco-org-insert-meetings (identifier response)
  "Insert the connection IDENTIFIER's meetings from RESPONSE."
  (with-current-buffer (get-buffer-create excorporate-org-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (end-of-line)
      (insert (format "%s..." identifier))))
  (with-current-buffer (exco-org--identifier-buffer identifier)
    (let ((inhibit-read-only t))
      (org-insert-time-stamp (current-time) t t "  + Last checked " "\n")
      (exco-calendar-item-iterate
       response (lambda (&rest arguments)
		  (with-current-buffer (exco-org--identifier-buffer identifier)
		    (org-mode)
		    (apply #'exco-org-insert-meeting arguments))))
      (goto-char (point-min))
      (if (save-excursion (org-goto-first-child))
	  (org-sort-entries t ?s)
	(forward-line 3)
	(insert "`♘\n")))))

(defun exco-org-finalize-buffer ()
  "Finalize text in buffer after all connections have responded."
  (with-current-buffer (get-buffer-create excorporate-org-buffer-name)
    ;; Sort top-level entries alphabetically.
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (end-of-line)
      (insert "done.\n")
      (dolist (result-buffer (nreverse exco-org--temporary-buffers))
	(insert-buffer-substring result-buffer)
	(kill-buffer result-buffer))
      (setq exco-org--temporary-buffers '()))))

;;;###autoload
(defun exco-org-show-day (month day year)
  "Show meetings for the date specified by MONTH DAY YEAR."
  (exco-connection-iterate #'exco-org-initialize-buffer
			   (lambda (identifier callback)
			     (exco-org-insert-headline identifier
						       month day year)
			     (exco-get-meetings-for-day identifier
							month day year
							callback))
			   #'exco-org-insert-meetings
			   #'exco-org-finalize-buffer))

(provide 'excorporate-org)

;;; excorporate-org.el ends here
