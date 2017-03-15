;;; helm-books.el --- Books searcher with helm interface
;; Author: grugrut <grugruglut+github@gmail.com>
;; URL: https://github.com/grugrut/helm-books
;; Version: 0.1

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

;; Books searcher with helm interface.

;;; Code:

(require 'helm)
(require 'json)

(defgroup helm-books nil
  "Books searcher with helm interface"
  :prefix "helm-books-"
  :group 'helm)

(defun helm-books--url-retrieve-from-google ()
  "."
  (switch-to-buffer
   (url-retrieve-synchronously
    "https://www.googleapis.com/books/v1/volumes?q=Emacs"))
  (goto-char (point-min))
  (re-search-forward "\n\n")
  (setq response-string
        (buffer-substring-no-properties
         (point) (point-max)))
  (kill-buffer (current-buffer))
  (json-read-from-string (decode-coding-string response-string 'utf-8)))

(defun helm-books--extract-values (item)
  "."
  (dolist (i item)
    (when (string= "volumeInfo" (car i))
      (dolist (j (cdr i))
        (when (string= "title" (car j))
          (setq title (cdr j)))
        (when (string= "publisher" (car j))
          (setq publisher (cdr j)))
        (when (string= "publishedDate" (car j))
          (setq publishedDate (cdr j)))
        )))
  (format "Title:%s, Publisher:%s, PublishedDate:%s" title publisher publishedDate))

(defun helm-books--candidates ()
  "."
  (mapcar 'helm-books--extract-values (cdr (nth 2 (helm-books--url-retrieve-from-google)))))

(defvar helm-books--source
  (helm-build-sync-source "Books"
    :candidates #'helm-books--candidates))

;;;###autoload
(defun helm-books ()
  "Books searcher with helm interface."
  (interactive)
  (helm :sources '(helm-books--source)
        :prompt "Search books: "
        :buffer "*helm books*"))

(provide 'helm-books)

;;; helm-books.el ends here
