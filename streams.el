;; streams.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Maintainer: René Trappel <rtrappel@gmail.com>
;; URL:
;; Version: 0.1
;; Package-Requires: emacs "26"
;; Keywords: web

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A package to collect webstreams and start them from your favorite
;; editor/os.
;;
;; 0.1
;; - Initial release

(defvar streams-player-cmd "mpv --force-window ") ;; for audio streams

(defun streams-add-webstream ()
  "Adds a webstream to the list of webstreams."
  (interactive)
  (let* ((streams-list-webstreams (streams-get-list-of-webstreams))
	 (new-webstream (read-from-minibuffer "Enter a web stream URL: "))
	 (name (read-from-minibuffer "Please provide a name for the new stream: "))
	 (name (replace-regexp-in-string "[\"'?:;\\\/]" "_" name)))
    (when (not streams-list-webstreams)
      (setq streams-list-webstreams (make-hash-table :test 'equal)))
    (puthash (concat name "::main::") new-webstream streams-list-webstreams)
    (with-temp-buffer
      (let* ((json-data (json-encode streams-list-webstreams)))
	(insert json-data)
	(write-file "~/.streams-webstream-list")))))

(defun streams-modify-group ()
 "Adds or removes a webstream to a group."
 (interactive)
 (let* ((streams-groups (streams-get-all-groups))
	(group))
   (when (= (length streams-groups) 1)
       (if (yes-or-no-p (format "There are no groups defined. Create one? "))
	 (setq group (read-from-minibuffer "Provide a name of the group: "))))
   (when (> (length streams-groups) 1)
     (setq streams-groups (remove "main" streams-groups)) ;; "main" should never be modified.
     (sort streams-groups 'string<)    
     (setq streams-groups (streams-presorted-completion-table streams-groups))
     (setq group (completing-read "Select group to modify or create new: " streams-groups)))
   (let* ((streams-list-webstreams (streams-get-list-of-webstreams))
	  (webstreams (hash-table-keys streams-list-webstreams))
	  (webstreams-pure (streams-pure-list webstreams))
	  (existing-webstreams)
	  (selection))
     (while (not (string-empty-p selection))
       (setq existing-webstreams (streams-return-set-webstreams group))
        (setq webstreams-pure (streams-presorted-completion-table webstreams-pure)) 
       (setq selection (completing-read (format "Currently the group \"%s\" includes: %s. Select stream to add or remove. Empty selection ends the process: " group (streams-prepare-results-list existing-webstreams)) webstreams-pure))
       (when (not (string-empty-p selection))
	 (streams-add-or-remove-from-group selection group))))))
	  
(defun streams-add-or-remove-from-group (pure-webstream group)
 "Adds a webstream to a group or removes it."
 (let* ((existing-key (streams-return-full-name-from-pure-selection pure-webstream))
	(existing-webstreams (streams-return-set-webstreams group))
	(new-key))
   (when (string-match-p (concat "::" group "::") existing-key)
       (setq new-key (replace-regexp-in-string (regexp-quote (concat "::" group "::")) "" existing-key)))
   (when (not (string-match-p (concat "::" group "::") existing-key))
     (setq new-key (concat existing-key "::" group "::")))
   (streams-update existing-key new-key)))

(defun streams-update (existing-key new-key)
 "Updates a search engine entries." 
 (let ((streams-list-webstreams (make-hash-table :test 'equal))
       (value))
   (with-temp-buffer
     (insert-file-contents "~/.streams-webstream-list")
     (if (fboundp 'json-parse-buffer)
	 (setq streams-list-webstreams (json-parse-buffer))))
   (setq value (gethash existing-key streams-list-webstreams))
   (remhash existing-key streams-list-webstreams)
   (puthash new-key value streams-list-webstreams)
   (with-temp-buffer
     (let* ((json-data (json-encode streams-list-webstreams)))
       (insert json-data)
       (write-file "~/.streams-webstream-list")))))

(defun streams-return-set-webstreams (group)
 "Returns all webstreams for search set."
 (let* ((streams-list-webstreams (streams-get-list-of-webstreams))
	(webstreams (hash-table-keys streams-list-webstreams))
	(results))
    (dolist (item webstreams)
      (if (string-match-p (concat "::" group "::") item)
	  (add-to-list 'results item)))
    (setq results (streams-pure-list results))
    results))

(defun streams-prepare-results-list (results)
  "Turning the list into a nice string"
  (setq results (mapconcat 'identity results ", ")))

(defun streams-presorted-completion-table (list)
  "Maintains the sorting of the list (for completing-read)."
  (let ((list-completion ()))
    (setq list-completion 
	  (lambda (string pred action)
	    (if (eq action 'metadata)
		`(metadata (display-sort-function . ,#'identity))
	    (complete-with-action action list string pred))))
list-completion))
	 
(defun streams-get-all-groups ()
  "Returns a list of all sets."
  (let* ((streams-list-webstreams (streams-get-list-of-webstreams))
	  (webstreams (hash-table-keys streams-list-webstreams))
 	  (search-sets))
     (dolist (item webstreams)
       (with-temp-buffer
	 (insert item)
	 (goto-char (point-min))
	 (while (and (re-search-forward "::\\(.*?\\)\\::" nil t)
		     (not (string-empty-p (match-string 1))))
	   (add-to-list 'search-sets (match-string 1)))))
     search-sets))

(defun streams-remove-webstream ()
  "Remove a webstream from the list."
  (interactive)
  (let* ((streams-list-webstreams (streams-get-list-of-webstreams))
	 (webstreams (hash-table-keys streams-list-webstreams))
	 (webstreams-pure (streams-pure-list webstreams))
	 (json-data)
	 (selection))
 (setq selection
 (completing-read "Which webstream should be removed? " webstreams-pure))
 (setq selection (streams-return-full-name-from-pure-selection selection))
  (if (not (member selection webstreams))
      (message "This webstream does not exist.")
    (if (yes-or-no-p (format "Are you sure you want to remove %s as a webstream? " (gethash selection streams-list-webstreams)))
	  (progn
	    (remhash selection streams-list-webstreams)
	    (with-temp-buffer
	      (setq json-data (json-encode streams-list-webstreams))
	      (insert json-data)
	      (write-file "~/.streams-webstream-list")))))))

(defun streams-pure-list (webstreams)
  "Create a list of webstreams without sets."
 (let ((webstreams-pure))
   (dolist (item webstreams)
      (push (replace-regexp-in-string "\\(::.*\\)" "" item) webstreams-pure))
   (sort webstreams-pure 'string<)    
   webstreams-pure))

(defun streams-return-full-name-from-pure-selection (selection)
  "Return the full name of webstream with sets."
  (let* ((streams-list-webstreams (streams-get-list-of-webstreams))
	 (webstreams (hash-table-keys streams-list-webstreams))
	 (full-name))
    (dolist (item webstreams)
      (when (string-prefix-p (concat selection "::main::") item)
       (setq full-name item)))
    full-name))

(defun streams-get-list-of-webstreams ()
 "Return streams-name-webstream, a hashtable that includes a list of names and locations of all webstreams."
 (let ((streams-file-exists (streams-check-for-webstream-file)))
   (when streams-file-exists
     (let ((streams-list-webstreams (make-hash-table :test 'equal)))
       (with-temp-buffer
	 (insert-file-contents "~/.streams-webstream-list")
	 (if (fboundp 'json-parse-buffer)
	     (setq streams-list-webstreams (json-parse-buffer))))
streams-list-webstreams))))

(defun streams-check-for-webstream-file ()
  "Checks for a webstream file in ~/.streams-webstream-list."
  (let ((streams-file-exists nil)
	(streams-list-webstreams (make-hash-table :test 'equal))
	(length-of-list))
  (when (file-exists-p "~/.streams-webstream-list")
    (with-temp-buffer
	 (insert-file-contents "~/.streams-webstream-list")
	 (if (fboundp 'json-parse-buffer)
	     (setq streams-list-webstreams (json-parse-buffer)))
	 (setq length-of-list (length (hash-table-values streams-list-webstreams)))
	 (when (not (zerop length-of-list))
	   (setq streams-file-exists t))))
  streams-file-exists))

(defun streams-return-webstream-for-selection (selection)
  "Returns a webstream URL from selection."
  (let* ((streams-list-webstreams (streams-get-list-of-webstreams))
	(webstreams (hash-table-keys streams-list-webstreams))
	(result))
    (setq result (gethash selection streams-list-webstreams))))

(defun streams ()
  "Start a webstream from the list."
  (interactive)
  (let* ((streams-list-webstreams (streams-get-list-of-webstreams))
	 (webstreams (hash-table-keys streams-list-webstreams))
	 (webstreams-pure (streams-pure-list webstreams))
	 (json-data)
	 (selection))
    (setq webstreams-pure (streams-presorted-completion-table webstreams-pure)) 
    (setq selection
	  (completing-read "Select webstream: " webstreams-pure))
    (setq selection (streams-return-full-name-from-pure-selection selection))
    (if (not (member selection webstreams))
	(message "This webstream does not exist.")
      (start-process-shell-command "streams.el" nil (concat streams-player-cmd "\"" (streams-return-webstream-for-selection selection) "\"")))))

(defun streams-select-stream-from-group ()
  "Select a stream from a group."
  (interactive)
  (let ((streams-file-exists (streams-check-for-webstream-file)))
    (when (not streams-file-exists)
      (message "In order to use streams you first need to add a webstream via streams-add-webstream."))
    (when streams-file-exists
      (let* ((streams-list-webstreams (streams-get-list-of-webstreams))
	     (webstreams (hash-table-keys streams-list-webstreams))
	     (streams-groups (streams-get-all-groups))
	     (selected-group)
	     (selected-stream)
	     (matched-webstreams))
	(setq streams-groups (remove "main" streams-groups)) ;; "main" should not be an option here.
	(sort streams-groups 'string<)
	(setq selected-group (completing-read "Chose a group: " (streams-presorted-completion-table streams-groups)))
      (when (member selected-group streams-groups)
	(setq matched-webstreams (streams-return-webstreams-for-group selected-group))
	(setq matched-webstreams (streams-pure-list matched-webstreams))
	(setq matched-webstreams (streams-presorted-completion-table matched-webstreams)) 
	(setq selected-stream (completing-read "Select webstream: " matched-webstreams))
	(setq selected-stream (streams-return-full-name-from-pure-selection selected-stream))
	 (if (not (member selected-stream webstreams))
	     (message "This webstream does not exist.")
	   (start-process-shell-command "streams.el" nil (concat streams-player-cmd "\"" (streams-return-webstream-for-selection selected-stream) "\""))))
	(when (not (member selected-group streams-groups))
	  (message "This group does not exist."))))))

(defun streams-return-webstreams-for-group (set)
  "Returns a list with all values for a search-set."
  (let* ((streams-list-webstreams (streams-get-list-of-webstreams))
	(webstreams (hash-table-keys streams-list-webstreams))
	(results))
    (dolist (item webstreams)
      (when (string-match-p (concat "::" set "::") item)
	(add-to-list 'results item)))
    results))


(provide 'streams)
