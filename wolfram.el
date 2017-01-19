;;; wolfram.el --- Wolfram Alpha Integration

;; Copyright (C) 2011-2017  Hans Sjunnesson

;; Author: Hans Sjunnesson <hans.sjunnesson@gmail.com>
;; Keywords: math
;; Version: 1.1

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
  "Wolfram Alpha customization group"
  :group 'wolfram)

(defcustom wolfram-alpha-app-id ""
  "The Wolfram Alpha App ID."
  :group 'wolfram-alpha
  :type 'string)

(defvar wolfram-alpha-buffer-name "*WolframAlpha*")


;;; Code:

(require 'url)
(require 'xml)
(require 'url-cache)

(defun wolfram--url-for-query (query)
  "Formats a WolframAlpha API url."
  (format "http://api.wolframalpha.com/v2/query?appid=%s&input=%s&format=image,plaintext&parsetimeout=15&scantimeout=15&podtimeout=15&formattimeout=15"
          wolfram-alpha-app-id
          (url-hexify-string query)))

(defun wolfram--async-xml-for-query (query callback)
  "Returns XML for a query"
  (let* ((url (wolfram--url-for-query query)))
    (when url (with-current-buffer
                  (url-retrieve url callback)))))

(defun wolfram--append-pod (pod)
  "Appends a pod to the current buffer."
  (let ((title (xml-get-attribute pod 'title))
        (err (equal "true" (xml-get-attribute pod 'error))))
    ;; First insert pod
    (insert
     (when title
       (format "\n## %s%s\n\n"
               title
               (if err " *error*" ""))))
    ;; Then subpods
    (dolist (subpod (xml-get-children pod 'subpod)) (wolfram--append-subpod subpod))))

(defun wolfram--insert-image (image)
  "Inserts an image xml into the current buffer"
  (let* ((url (xml-get-attribute image 'src))
         (temp-file (make-temp-file "wolfram"))
         (data (url-retrieve-synchronously url)))
    (switch-to-buffer data)
    (goto-char (point-min))
    (search-forward "\n\n")
    (write-region (point-min) (point-max) temp-file)
    (kill-buffer)
    (wolfram--switch-to-wolfram-buffer)
    (insert (format "%s" temp-file))
    (goto-char (point-max))
    ))

(defun wolfram--append-subpod (subpod)
  "Appends a subpod to the current buffer."
  (let ((plaintext (car (xml-get-children subpod 'plaintext)))
        (image (car (xml-get-children subpod 'img))))
    (when (and plaintext image)
      (wolfram--insert-image-from-url (xml-get-attribute image 'src)))
    (when (and plaintext (not image))
      (insert (format "%s\n" (car (last plaintext)))))
    (when (and image (not plaintext))
      (wolfram--insert-image (xml-get-attribute image 'src)))
    (insert "\n")))

(defun wolfram--switch-to-wolfram-buffer ()
  "Switches to (creates if necessary) the wolfram alpha results buffer."
  (let ((buffer (get-buffer-create wolfram-alpha-buffer-name)))
    (unless (eq (current-buffer) buffer)
      (switch-to-buffer buffer))
    (special-mode)
    (when (functionp 'iimage-mode) (iimage-mode))
    buffer))

(defun wolfram--create-wolfram-buffer (query)
  "Creates the buffer to show the pods."
  (wolfram--switch-to-wolfram-buffer)
  (goto-char (point-max))
  (let ((inhibit-read-only t))
    (insert (format "# \"%s\" (searching)\n" query))))

(defun wolfram--append-pods-to-buffer (buffer pods)
  "Appends all of the pods to a specific buffer."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (search-backward " (searching)")
    (replace-match "")
    (goto-char (point-max))
    (dolist (pod pods)
      (wolfram--append-pod pod))
    (insert "\n")))

(defun wolfram--insert-image-from-url (url)
  "Fetches an image and inserts it in the buffer."
  (unless url (error "No URL."))
  (let ((buffer (url-retrieve-synchronously url)))
    (unwind-protect
        (let ((data (with-current-buffer buffer
                      (goto-char (point-min))
                      (search-forward "\n\n")
                      (buffer-substring (point) (point-max)))))
          (insert-image (create-image data nil t)))
      (kill-buffer buffer))))

;;;###autoload
(defun wolfram-alpha (query)
  "Sends a query to Wolfram Alpha, returns the resulting data as a list of pods."
  (interactive
   (list
    (if (use-region-p)
        (buffer-substring-no-properties
         (region-beginning) (region-end))
      (read-string "Query: " nil 'wolfram-alpha-history))))
  (unless (and (bound-and-true-p wolfram-alpha-app-id)
               (not (string= "" wolfram-alpha-app-id)))
    (error "Custom variable wolfram-alpha-app-id not set."))
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
