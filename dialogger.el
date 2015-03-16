;;; dialogger.el --- dialog editing support

;; Copyright (C) 2015  Sean Allred

;; Author: Sean Allred <code@seanallred.com>
;; Keywords: extensions, docs, tools

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

;; Introduces functions fit for transcribing dialogue.  Also recommend
;; checking out `transcription-mode' for audio control.  Hooks into
;; Org's C-c C-c handler for easy input.

;; Recommend binding `dialogger-log' to C-c M-l:
;;
;;     (global-set-key (kbd "C-c M-l") #'dialogger-log)
;;
;; Once there is a log line to use, Org's C-c C-c will be able to
;; catch it.

;;; Code:


;;; Keeping State
(defvar dialogger-key-speaker-alist nil
  "Alist mapping keys to speakers.")
(make-variable-buffer-local
 'dialogger-key-speaker-alist)

(defun dialogger-defspeaker (speaker key)
  "Defines SPEAKER to be activated with KEY.
If KEY is already present in `dialogger-key-speaker-alist', set
the value stored to SPEAKER.  Otherwise, add \(SPEAKER . KEY\) to
the alist.

Returns the new `dialogger-key-speaker-alist'."
  (let ((keystr (help-key-description (vector key) nil)))
    (if (assoc keystr dialogger-key-speaker-alist)
        (setcdr (assoc keystr dialogger-key-speaker-alist)
                speaker)
      (add-to-list 'dialogger-key-speaker-alist
                   (cons keystr speaker)))))

(defun dialogger--get-speaker-for-key (key)
  "Retrieves the speaker for KEY.
Returns a string."
  (cdr (assoc (help-key-description (vector key) nil)
              dialogger-key-speaker-alist)))

(defun dialogger--read-speaker (prompt)
  "Read a speaker's name, prompting with PROMPT.
Returns the speaker's name."
  (interactive)
  (read-string prompt))

(defun dialogger--read-key (prompt)
  "Read a key, prompting with PROMPT.
Returns the key code entered."
  (interactive)
  (read-char prompt))

(defun dialogger-save-config ()
  "Save `dialogger-key-speaker-alist' to Local Variables.
When the file is opened again, all of the speakers will be
available just as they were when the file was last used."
  (interactive)
  (save-excursion
    (add-file-local-variable
     'dialogger-key-speaker-alist
     dialogger-key-speaker-alist)))

(defun dialogger-reset ()
  "Revert the speaker definition to the Local Variables value.
This uses the same logic as `read-file-local-variable' and comes
with all of its shortcomings.  It assumes the Local Variables
section is at the end of the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (let ((case-fold-search t))
      (search-backward "Local Variables:" (max (- (point-max) 3000) (point-min)) t))
    (if (search-forward "dialogger-key-speaker-alist: " nil t)
        (setq dialogger-key-speaker-alist
              (read
               (buffer-substring-no-properties
                (point) (save-excursion (end-of-line) (point)))))
      (dialogger-save-config))))


;;; On-The-Fly Configuration
(defun dialogger-new-speaker ()
  "Define a new speaker interactively.
Also updates the file-local variable for
`dialogger-key-speaker-alist'."
  (interactive)
  (let* ((key (dialogger--read-key "[dialogger] Key:"))
         (keystr (help-key-description (vector key) nil))
         (name (dialogger--read-speaker
                (format
                 "[dialogger] Bind `%s' to Name: "
                 keystr))))
    (dialogger-defspeaker name key)
    (dialogger-save-config)
    (message "Speaker `%s' is now \"%s\"" keystr name)))


;;; Writing Logs
(defun dialogger-format (speaker &optional time)
  "Return a formatted string for SPEAKER speaking at TIME.
If TIME is nil, the current time is used."
  (format "- [%s] %s :: " (format-time-string "%FT%T" time) speaker))

(defun dialogger-log (arg)
  "Log a speaker.
Starts a new line under current point and inserts
`dialogger-format' output.  With a prefix argument, force
appending to the bottom of a list."
  (interactive "P")
  (let*  ((key (read-char "Speaker? (`&' to create)"))
          (speaker (dialogger--get-speaker-for-key key))
          (keystr (help-key-description (vector key) nil)))
    (if (= key ?&)
        (dialogger-new-speaker)
        (if (not speaker)
            (user-error "No such speaker `%s'" keystr)
          (when arg
            (forward-paragraph)
            (forward-line -1))
          (end-of-line)
          (newline)
          (insert (dialogger-format speaker))
          (message "Logging for speaker `%s' \"%s\""
                   keystr speaker)))))

(defun dialogger-org-log ()
  "If point is on a log line, insert a new log.
Otherwise, return nil."
  (when (save-excursion
          (beginning-of-line)
          (looking-at-p
           (rx line-start "- ["
               (repeat 4 digit) "-"
               (repeat 2 digit) "-"
               (repeat 2 digit)
               "T"                      ;(format-time-string "%FT%T" time)
               (repeat 2 digit) ":"
               (repeat 2 digit) ":"
               (repeat 2 digit)
               "] " (* any) " :: ")))
    (dialogger-log current-prefix-arg)))

(require 'org)
(add-hook 'org-ctrl-c-ctrl-c-hook
          #'dialogger-org-log)

(provide 'dialogger)
;;; dialogger.el ends here
