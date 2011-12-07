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

(defvar wolfram-alpha-query-history nil
  "History for `wolfram-alpha' prompt.")

(defgroup wolfram-alpha nil
  "Wolfram Alpha customization group")

(defcustom wolfram-alpha-app-id ""
  "The Wolfram Alpha App ID."
  :group 'wolfram-alpha
  :type 'string)

(defcustom wolfram-alpha-buffer-name "*WolframAlpha*"
  "The name of the WolframAlpha query results buffer."
  :group 'wolfram-alpha
  :type 'string)

(defcustom wolfram-alpha-prefer-plaintext nil
  "Whether to show plaintext version when there's both a plaintext and an image result."
  :group 'wolfram-alpha
  :type 'boolean)


;;; Code:

(require 'url)
(require 'xml)

(defun wolfram--url-for-query (query)
  "Formats a WolframAlpha API url."
  (format "http://api.wolframalpha.com/v2/query?appid=%s&input=%s&format=image,plaintext"
	  wolfram-alpha-app-id
	  query))

(defun wolfram--async-xml-for-query (query callback)
  "Returns XML for a query"
  (let* ((url (wolfram--url-for-query query)))
    (when url (with-current-buffer
		  (url-retrieve url callback)))))

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
       (format "** %s%s\n"
	       title
	       (if err " *error*" ""))))
    ;; Then subpods
    (dolist (subpod (xml-get-children pod 'subpod)) (wolfram--append-subpod subpod))))

(defun wolfram--append-subpod (subpod)
  "Appends a subpod to the current buffer."
  (let ((plaintext (car (xml-get-children subpod 'plaintext)))
	(image (car (xml-get-children subpod 'img))))
    (insert
     (concat
      (when plaintext
	(format "%s\n"
		(car (last plaintext))))
      ;; (when image
      ;; 	(format "%s\n"
      ;; 		(xml-get-attribute image 'src)))
      "\n"))
    ))

(defun wolfram--switch-to-wolfram-buffer ()
  "Switches to (creates if necessary) the wolfram alpha results buffer."
  (let ((buffer (get-buffer-create wolfram-alpha-buffer-name)))
    (unless (eq (current-buffer) buffer)
      (switch-to-buffer buffer))
    (when (functionp 'org-mode) (org-mode))
    (when (functionp 'iimage-mode) (iimage-mode))
    buffer))

(defun wolfram--create-wolfram-buffer (query)
  "Creates the buffer to show the pods."
  (wolfram--switch-to-wolfram-buffer)
  (goto-char (point-max))
  (insert (format "* \"%s\" (searching)\n" query)))

(defun wolfram--append-pods-to-buffer (buffer pods)
  "Appends all of the pods to a specific buffer."
  (goto-char (point-max))
  (search-backward " (searching)")
  (replace-match "")
  (goto-char (point-max))
  (dolist (pod pods)
    (wolfram--append-pod pod)))

(defun wolfram-alpha (query)
  "Sends a query to Wolfram Alpha, returns the resulting data as a list of pods."
  (interactive
   (list
    (if (and transient-mark-mode mark-active)
	(buffer-substring-no-properties
         (region-beginning) (region-end))
      (read-string "Query: " nil 'wolfram-alpha-history))))
  (wolfram--create-wolfram-buffer query)
  (wolfram--async-xml-for-query
   query
   (lambda (args)
     (let ((pods 
	    (xml-get-children
	     (let ((data (buffer-string)))
	       (with-temp-buffer
		 (erase-buffer)
		 (insert data)
		 (car (xml-parse-region (point-min) (point-max)))))
	     'pod)))
       (let ((buffer (wolfram--switch-to-wolfram-buffer)))
	 (wolfram--append-pods-to-buffer
	  buffer
	  pods))
       )
     ))
  )

(provide 'wolfram)
;;; wolfram.el ends here
