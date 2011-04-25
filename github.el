;; github.el --- Way to interact with the Github site through Emacs

;; Maintainer: Abhi Yerra <abhi@berkeley.edu>
;; Author: Abhi Yerra <abhi@berkeley.edu>
;; Version: 0.1
;; Created: 23 May 2010
;; Keywords: github

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.


(defconst github-version "0.1"
  "Version of github.el")

(defcustom github-login ""
  "Login for github"
  :type 'string
  :group 'github)

(defcustom github-token ""
  "Token for github. Get it here https://github.com/account/admin"
  :type 'string
  :group 'github)

;; The repos to autocomplete. Should be of the form username/repo.
;; Ex. "abhiyerra/txtdrop" "abhiyerra/vayu"
(defvar github-autocomplete-repos '())


(defun github-repo-complete ()
  (completing-read "Repository: " github-autocomplete-repos nil nil (concat github-login "/")))

;; (defun github-repos ()
;;   "Reload all of user's repos"
;;   (interactive)
;;   (github-api-request "GET" (concat "repos/show/" github-login) ""))

;; (switch-to-buffer (github-api-request "GET" (concat "repos/show/" github-login) ""))

;; Display issues for repo.
;;  - d - Close issue
;;  - l - Add label to issue
;;  - m - Add milestone?
;;  - c - Comment on issue
;;  - <RET> - View issue
(define-derived-mode github-issues-list-mode fundamental-mode
  "github-issues-list-mode"
  "Major mode for listing Github issues."
  (org-set-local
   'header-line-format
   "github issues. Finish `C-c C-c'.")

  (define-key github-issues-list-mode-map "\C-c\C-c" 'github-issues-list-close))

(defun github-issues-list-close ()
  "Close the window."
  (interactive)
  (kill-buffer))

(defun github-issues-list ()
  "List all the issues for a repository"
  (interactive)
  (save-excursion
    (let ((repo (github-repo-complete)))
      (let ((buf (get-buffer-create
                (concat "*" repo " Issues*"))))
        (with-current-buffer buf
          (switch-to-buffer buf)
          (github-issues-list-mode)
          (let ((issues (github-grab-issues repo)))
            (switch-to-buffer buf)
            (mapcar
             (lambda (x)
               (github-columnize-and-insert-list
                (list
                 (plist-get x :title)
                 (plist-get x :state))))
               issues)
            (setq buffer-read-only t)
            (buffer-disable-undo)
            ))))))

;(github-issues-list)

;; user, title, comments, labels created_at
(defun github-grab-issues (repo)
  "Display the results for the repo."
  (save-excursion
    (switch-to-buffer (github-api-request "GET" (concat "issues/list/" repo "/open") ""))
    (goto-char (point-min))
    (re-search-forward "^\n")
    (beginning-of-line)
    (let ((response-string (buffer-substring (point) (buffer-end 1)))
          (json-object-type 'plist))
      (let ((issues (plist-get (json-read-from-string response-string) :issues)))
        (kill-buffer)
        issues))))



(defun github-columnize-and-insert-list (list &optional pad-width)
  "Insert LIST into the current buffer in as many columns as possible.
The maximum number of columns is determined by the current window
width and the longest string in LIST."
  (unless pad-width
    (setq pad-width 3))
  (let ((width (window-width))
        (max (+ (apply #'max (mapcar #'length list))
                pad-width)))
    (let ((columns (/ width max)))
      (when (zerop columns)
        (setq columns 1))
      (while list
        (dotimes (i (1- columns))
          (insert (concat (car list) (make-string (- max (length (car list)))
                                                  ?\s)))
          (setq list (cdr list)))
        (when (not (null list))
          (insert (pop list)))
        (insert "\n")))))



;; (insert testing-github)

;; Show an issue
;; - c - Add a comment
;; - d - Close issue
;; - e - Edit the issue
;; - l - Add label
(defun github-issues-show ()
  "Show a particular issue"
  nil)



;; Create new issues

;; TODO: Should be based on markdown-mode
(define-derived-mode github-issues-new-mode fundamental-mode
  "github-issues-new-mode"
  "Major mode for entering new Github issues."
  (org-set-local
   'header-line-format
   "github issues ticket. Finish `C-c C-c', abort `C-c C-k'.")

  (define-key github-issues-new-mode-map "\C-c\C-c" 'github-issues-new-create)
  (define-key github-issues-new-mode-map "\C-c\C-k" 'github-issues-new-cancel))

;; Display a new buffer to enter ticket.
(defun github-issues-new ()
  "Open a window to enter a new issue."
  (interactive)
  (save-excursion
    (let ((buf (get-buffer-create "*New Github Issue*")))
      (with-current-buffer buf
        (switch-to-buffer buf)
        (github-issues-new-mode)))))


;; The first line is the title, everything else is the body.
(defun github-issues-new-create ()
  "Create the issue on github."
  (interactive)
  (goto-char (point-min))
  (github-api-request
   "POST"
   (concat "issues/open/" (github-repo-complete))
   (concat "title=" (url-hexify-string
                     (buffer-substring-no-properties
                      (point-min)
                      (line-end-position)))
           "&body=" (url-hexify-string
                     (buffer-substring-no-properties
                      (line-end-position)
                      (buffer-end 1)))))
    (kill-buffer))


(defun github-issues-new-cancel ()
  "Cancel the creation of the the issue."
  (interactive)
  (kill-buffer))

;; (defun github-issues-issue ()
;;   "Show an individual issue from repo's issues."
;;   nil)

;; (defun github-issues-issue-comment-new ()
;;   "Add a comment to an issue"
;;   nil)

;; (defun github-issues-issue-comment-delete ()
;;   "Add a comment to an issue"
;;   nil)


;; The main way to make a request to github.
(defun github-api-request (method url params)
  "Make a call to github"
  (let ((url-request-method method)
        (url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data    (concat
                              "login=" github-login
                              "&token=" github-token
                              "&"
                              params)))
    (url-retrieve-synchronously
     (concat "http://github.com/api/v2/json/" url))))



(provide 'github)
