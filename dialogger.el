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

;;; Code:

(defvar dialogger-key-speaker-map (make-hash-table)
  "Hashmap of keys to speakers.")
(make-variable-buffer-local
 'dialogger-key-speaker-map)

(defun dialogger-defspeaker (speaker key)
  "Defines SPEAKER to be activated with KEY."
  (puthash key speaker dialogger-key-speaker-map))

(defun dialogger--read-string (prompt)
  (interactive)
  (read-string prompt))
(defun dialogger--read-char (prompt)
  (interactive)
  (read-char prompt))

(defun dialogger-new-speaker ()
  "Define a new speaker interactively and updates file-local variables."
  (interactive)
  (let ((name (dialogger--read-string "[dialogger] Name: "))
        (key (dialogger--read-char "[dialogger] Key:")))
    (dialogger-defspeaker name key)
    (dialogger-save-config)
    (message "Speaker `%s' is now \"%s\""
             (help-key-description (vector key) nil)
             name)))

(defun dialogger-save-config ()
  (interactive)
  (save-excursion
    (add-file-local-variable
     'dialogger-key-speaker-map
     dialogger-key-speaker-map)))

(defun dialogger-format (speaker &optional time)
  "Return a formatted string for SPEAKER speaking at TIME.
If TIME is nil, the current time is used."
  (format "- [%s] %s :: " (format-time-string "%FT%T" time) speaker))

(defun dialogger-log (arg)
  (interactive "P")
  (let*  ((key (read-char "Speaker? (`&' to create)"))
          (speaker (gethash key dialogger-key-speaker-map))
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
  "Checks context and acts appropriately."
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
