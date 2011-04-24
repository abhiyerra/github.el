# Description

A way to interact with github through Emacs. Currently only supports
posting issues.

# Config

    (load "~/path/to/github.el/github.el")
    (require 'github)

    (setq github-login "abhiyerra")
    ;; Get the token here: https://github.com/account/admin
    (setq github-token "EnterGithubTokenHere")
    ;; The repos that you want to have autocompleted
    (setq github-autocomplete-repos
          '("abhiyerra/txtdrop"
            "abhiyerra/rutt"
            "abhiyerra/Ravana"))

# Help

    M-x github-issue-new
