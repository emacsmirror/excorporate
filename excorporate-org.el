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

(defun exco-org--connection-identifier-at-point ()
  "Return the connection identifier associated with point."
  (let* ((calendar-headline
	  (save-excursion (org-up-heading-safe) (org-element-at-point)))
	 (headline (org-element-property :raw-value calendar-headline)))
    (string-match "Calendar (\\(.*\\))$" headline)
    (car (read-from-string (match-string 1 headline)))))

(defun exco-org--is-meeting ()
  "Return t if the entry at point is a meeting, not an appointment."
  (save-excursion
    (org-back-to-heading)
    (let ((element (org-element-at-point)))
      ;; Rule out top Calendar item.
      (when (equal (org-element-property :level element) 2)
	(not (null
	      (re-search-forward
	     "^\+ Invitees:$"
	     (org-element-property :end (org-element-at-point)) t)))))))

(defun exco-org--organizer ()
  "Return a string representing the item at point's organizer."
  (save-excursion
    (org-back-to-heading)
    (let* ((element (org-element-at-point))
	   (begin (org-element-property :begin element))
	   (end (org-element-property :end element))
	   (entry-text (buffer-substring-no-properties begin end)))
      ;; Rule out top Calendar item.
      (when (equal (org-element-property :level element) 2)
	(string-match "^+ Organizer: \\(.*\\)$" entry-text)
	(match-string 1 entry-text)))))

(defun exco-org--organizer-matches-connection ()
  "Return non-nil if the entry at point is owned by the connection owner."
  (let ((identifier (exco-org--connection-identifier-at-point))
	(organizer (exco-org--organizer)))
    (cond
     ((stringp identifier)
      (equal identifier organizer))
     ((consp identifier)
      (equal (car identifier) organizer))
     (t
      (error "Did not recognize error")))))

(defun exco-org-cancel-meeting ()
  "Cancel the meeting at point, prompting for a cancellation message."
  (interactive)
  (unless (exco-org--is-meeting)
    (error (concat "This looks like an appointment,"
		   " try `exco-org-delete-appointment' instead.")))
  (let ((identifier (exco-org--connection-identifier-at-point))
	(item-identifier
	 (org-entry-get (car (org-get-property-block)) "Identifier")))
    ;; Make sure the meeting owner matches the connection owner before
    ;; attempting to cancel the meeting.
    (unless (exco-org--organizer-matches-connection)
      (error (concat "exco-org will only attempt to delete"
		     " meetings for which you are the organizer")))
    (when item-identifier
      (exco-calendar-item-meeting-cancel
       identifier
       (car (read-from-string item-identifier))
       (read-from-minibuffer "Cancellation message: ")
       (lambda (identifier response)
	 (let ((response-code
		(exco-extract-value '(ResponseMessages
				      CreateItemResponseMessage
				      ResponseCode)
				    response)))
	   (if (equal response-code "NoError")
	       (with-current-buffer (get-buffer-create
				     excorporate-org-buffer-name)
		 (save-excursion
		   (org-back-to-heading)
		   (let* ((inhibit-read-only t)
			  (element (org-element-at-point))
			  (begin (org-element-property :begin element))
			  (end (org-element-property :end element)))
		     (kill-region begin end)
		     (message
		      "excorporate-org: Successfully cancelled meeting"))))
	     (message "excorporate-org: Failed to cancel meeting: %S"
		      response-code))))))))

(defun exco-org-delete-appointment ()
  "Delete the appointment at point."
  (interactive)
  (when (exco-org--is-meeting)
    (error "This looks like a meeting, try `exco-org-cancel-meeting' instead"))
  (let ((identifier (exco-org--connection-identifier-at-point))
	(item-identifier
	 (org-entry-get (car (org-get-property-block)) "Identifier")))
    (when item-identifier
      (exco-calendar-item-appointment-delete
       identifier
       (car (read-from-string item-identifier))
       (lambda (identifier response)
	 (let ((response-code
		(exco-extract-value '(ResponseMessages
				      DeleteItemResponseMessage
				      ResponseCode)
				    response)))
	   (if (equal response-code "NoError")
	       (with-current-buffer (get-buffer-create
				     excorporate-org-buffer-name)
		 (save-excursion
		   (org-back-to-heading)
		   (let* ((inhibit-read-only t)
			  (element (org-element-at-point))
			  (begin (org-element-property :begin element))
			  (end (org-element-property :end element)))
		     (kill-region begin end)
		     (message
		      "excorporate-org: Successfully deleted appointment"))))
	     (message "excorporate-org: Failed to delete appointment: %S"
		      response-code))))))))

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
	(insert "# Updated..."))))

(defun exco-org-format-headline (identifier)
  "Format an Org headline using IDENTIFIER."
  (format "* Calendar (%S)\n" identifier))

(defun exco-org-insert-meeting-headline (subject
					 start-time end-time
					 &optional item-identifier)
  "Insert and schedule a meeting.
SUBJECT is the meeting's subject, START-TIME and END-TIME are the
meeting's start and end times in the same format as is returned
by `current-time'.  ITEM-IDENTIFIER is the item identifier in the
form (ItemId (Id . ID-STRING) (ChangeKey . CHANGEKEY-STRING))."
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
    (org-set-property "Identifier" (format "%S" item-identifier))
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
				main-invitees optional-invitees
				&optional item-identifier organizer identifier)
  "Insert a scheduled meeting.
SUBJECT is a string, the subject of the meeting.  START is the
meeting start time in Emacs internal date time format, and END is
the end of the meeting in the same format.  LOCATION is a string
representing the location.  MAIN-INVITEES and OPTIONAL-INVITEES
are the requested participants.  ITEM-IDENTIFIER is the item
identifier in the form
\(ItemId (Id . ID-STRING) (ChangeKey . CHANGEKEY-STRING)).
ORGANIZER is a string containing the organizer of the meeting, in
server-internal form.  IDENTIFIER is the connection identifier."
  ;; The Organizer email is in the server's internal format.  Resolve
  ;; it synchronously, for simplicity.
  (let ((organizer-email-address
	 (exco-extract-value
	  '(ResponseMessages
	    ResolveNamesResponseMessage
	    ResolutionSet
	    Resolution
	    Mailbox
	    EmailAddress)
	  (with-timeout
	      (1 (error "Server did not respond in time"))
	    (exco-operate-synchronously
	     identifier "ResolveNames"
	     `(((UnresolvedEntry . ,organizer)) nil nil nil))))))
    (exco-org-insert-meeting-headline subject start end item-identifier)
    (insert (format "+ Duration: %d minutes\n"
		    (round (/ (float-time (time-subtract end start)) 60.0))))
    (insert (format "+ Location: %s\n" location))
    (insert (format "+ Organizer: %s\n" organizer-email-address))
    (when main-invitees
      (insert "+ Invitees:\n")
      (exco-org-insert-invitees main-invitees))
    (when optional-invitees
      (insert "+ Optional invitees:\n")
      (exco-org-insert-invitees optional-invitees))))

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
      (exco-calendar-item-iterate-general
       response (lambda (&rest arguments)
		  (with-current-buffer (exco-org--identifier-buffer identifier)
		    (org-mode)
		    (apply #'exco-org-insert-meeting
			   ;; Gross, but keeps exco-org-insert-meeting
			   ;; signature backward compatible.
			   (append arguments (list identifier)))))
       subject start-internal end-internal
       location main-invitees optional-invitees item-identifier organizer)
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
	(save-excursion (org-up-heading-safe) (org-cycle-hide-drawers 'all))
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
