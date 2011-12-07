;;; wolfram.el --- Wolfram Alpha Integration

;; Copyright (C) 2011  Hans Sjunnesson

;; Author: Hans Sjunnesson <hans.sjunnesson@gmail.com>
;; Keywords: math

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

;; 

;;; Vars:

(defvar wolfram-app-id "2JTYAL-2XAYU3XVE3"
  "The Wolfram Alpha App ID")

(defvar wolfram-alpha-query-history nil
  "History for `wolfram-alpha' prompt.")

(defgroup wolfram-alpha nil
  "Wolfram Alpha customization group")

(defcustom wolfram-alpha-buffer-name "*WolframAlpha*"
  "The name of the WolframAlpha query results buffer."
  :group 'wolfram-alpha
  :type 'string)

(defcustom wolfram-alpha-prefer-plaintext t
  "Whether to show plaintext version when there's both a plaintext and an image result."
  :group 'wolfram-alpha
  :type 'boolean)


;;; Code:

(require 'url)
(require 'xml)

(defun wolfram--url-for-query (query)
  "Formats a WolframAlpha API url."
  (format "http://api.wolframalpha.com/v2/query?appid=%s&input=%s&format=image,plaintext"
	  wolfram-app-id
	  query))

(defun wolfram--xml-for-query (query)
  "Returns XML for a query"
  (let* ((url (wolfram--url-for-query query))
	 (data (when url (with-current-buffer (url-retrieve-synchronously url)
			   (buffer-string)))))
    (if data
	(with-temp-buffer
	  (erase-buffer)
	  (insert data)
	  (car (xml-parse-region (point-min) (point-max)))))))

(defun wolfram--pods-for-query (query)
  "Runs a query, return pods as an alist."
  (xml-get-children (wolfram--xml-for-query query) 'pod))

(defun wolfram--append-pod (pod)
  "Appends a pod to the current buffer."
  (let ((title (xml-get-attribute pod 'title))
  	(err (equal "true" (xml-get-attribute pod 'error))))
    ;; First insert pod
    (insert
     (when title
       (format "* %s%s\n"
	       title
	       (if err " *error*" ""))))
    ;; Then subpods
    (dolist (subpod (xml-get-children pod 'subpod))
      (let ((plaintext (car (xml-get-children subpod 'plaintext)))
	    (image (car (xml-get-children subpod 'img))))
    	(insert
	 (concat
	  (when plaintext
	    (format "%s\n"
		    (car (last plaintext))))
	  (when image
	    (format "%s\n"
		    (xml-get-attribute image 'src)))
	  "\n"))
	))
    ))

(defun wolfram--create-pods-buffer (pods)
  "Creates the buffer to show the pods."
  (let ((buffer (get-buffer-create wolfram-alpha-buffer-name)))
    (unless (eq (current-buffer) buffer)
      (switch-to-buffer-other-window buffer))
    (when (functionp 'org-mode) (org-mode))
    (when (functionp 'iimage-mode) (iimage-mode))
    (goto-char (point-max))
    (dolist (pod pods)
      (wolfram--append-pod pod))
    buffer))

(defun wolfram-alpha (query)
  "Sends a query to Wolfram Alpha, returns the resulting data as a list of pods."
  (interactive
   (list
    (if (and transient-mark-mode mark-active)
	(buffer-substring-no-properties
         (region-beginning) (region-end))
      (read-string "Query: " nil 'wolfram-alpha-history))))
  (if (and (stringp query)
	   (> (length query) 0))
      (wolfram--create-pods-buffer
       (wolfram--pods-for-query query))))

(provide 'wolfram)
;;; wolfram.el ends here
