# Description

A way to interact with github through Emacs. Currently only supports
posting issues.

# Config

    (load "~/path/to/github.el/github.el")
    (require 'github)

    (setq github-login "abhiyerra")
    ;; Enter your github password.
    (setq github-password "Password")
    ;; The repos that you want to have autocompleted
    (setq github-autocomplete-repos
          '("abhiyerra/txtdrop"
            "abhiyerra/rutt"
            "abhiyerra/Ravana"))

# Help

 - M-x github-issues-new - Create a new issue for a repository.
 - M-x github-issues-list - List issues for a repository.
