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


(defvar github-login "")
(defvar github-token "")

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

  (define-key github-issues-list-mode-map "\C-c\C-c" 'github-issues-list-close)
  (define-key github-issues-list-mode-map "\C-c\C-k" 'github-issues-new-close))

(defun github-issues-list ()
  "List all the issues for a repository"
  (interactive)
  (save-excursion
    (let ((repo (github-repo-complete)))
      (let ((buf (get-buffer-create
                (concat "*" repo " Issues*"))))
        (with-current-buffer buf
          (switch-to-buffer buf)
          (github-display-issues repo)
          (setq buffer-read-only t)
          (github-issues-list-mode))))))

(defun github-display-issues (repo)
  "Display the results for the repo."
  ())


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
  (goto-line 1)
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
