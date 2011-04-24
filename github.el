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


(defconst github-login "")
(defconst github-token "")

;; The repos to autocomplete. Should be of the form username/repo.
;; Ex. "abhiyerra/txtdrop" "abhiyerra/vayu"
(defconst github-autocomplete-repos '())


;; (defun github-repos ()
;;   "Reload all of user's repos"
;;   (interactive)
;;   (github-api-request "GET" (concat "repos/show/" github-login) ""))

; (switch-to-buffer (github-api-request "GET" (concat "repos/show/" github-login) ""))


;; TODO: Be able to add body.
(defun github-issue-new ()
  "List issues"
  (interactive)
  (github-api-request
   "POST"
   (concat "issues/open/"
           (completing-read "Repository: " github-autocomplete-repos nil nil (concat github-login "/")))
   (concat "title="
           (url-hexify-string (read-string "Ticket Title: ")))))

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

;; TODO: Be able to list issues
;; TODO: Be able to comment on issues
;; TODO: Be able to close issues


(provide 'github)
