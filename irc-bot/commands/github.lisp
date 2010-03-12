(in-package :nisp.i)


(flet ((github-apply (function string)
         (apply function (github::parse-github-repository-notation string))))
  (define-simple-command github
    (network-tree::next-node))
  (define-simple-command github-show
    (network-tree::next-node))
  (define-simple-command github-show-followers
    (reply (cl-github:show-followers (network-tree::remaining-parameters))))
  (define-simple-command github-show-following
    (reply (cl-github:show-following (network-tree::remaining-parameters))))
  (define-simple-command github-show-repositories
    (reply
     (mapcar #'cl-github::repository-name
             (cl-github::show-user-repositories (network-tree::remaining-parameters)))))
  (define-simple-command github-show-watched
    (network-tree::next-node))
  (define-simple-command github-show-watched-repositories
    (reply
     (set-difference
      (mapcar #'github::github-repository-notation
              (github::watched-repositories (network-tree::remaining-parameters)))
      (mapcar #'cl-github::github-repository-notation
              (cl-github::show-user-repositories (network-tree::remaining-parameters)))
      :test #'equal)))
  (defmethod one-line-description ((repo github::repository))
    (flet ((format-fork ()
             (if (github:repository-fork-p repo)
                 " (fork)"
                 ""))
           (format-people (total)
             (case total
               (1 "1 person")
               (0 "nobody")
               (t (format nil "~A people" total))))
           (format-issues (total)
             (case total
               (1 "1 open issue")
               (0 "no open issues")
               (t (format nil "~A open issues" total)))))
      (format nil
              "~A's ~A~A <~A> is watched by ~A, forked by ~A, has ~A, and described as: ~A"
              (string-capitalize (github:repository-owner repo))
              (github:repository-name repo)
              (format-fork)
              (github:repository-url repo)
              (format-people (github:repository-watchers-count repo))
              (format-people (github:repository-forks-count repo))
              (format-issues (github:repository-open-issues-count repo))
              (github:repository-description repo))))

  (define-simple-command github-show-repository
    (reply (one-line-description
            (github-apply #'github::show-repository
                          (network-tree::remaining-parameters)))))

  (define-simple-command github-show-collaborators
    (reply (github-apply #'github::show-collaborators
                         (network-tree::remaining-parameters))))

  (define-simple-command github-show-tags
    (reply (mapcar #'car
                   (github-apply #'github::show-tags
                                 (network-tree::remaining-parameters)))))

  (define-simple-command github-show-languages
    (reply (princ-to-string (github-apply #'github::show-languages
                                          (network-tree::remaining-parameters)))))

  (define-simple-command github-show-branches
    (reply (format nil "~{~A~^, ~}"
            (mapcar #'car
                    (github-apply #'github::show-branches
                                  (network-tree::remaining-parameters)))))))