;; auto-doc.el is just an helper script to generate the README.md

(setq myregexp-key
      (rx
       (submatch
        (and word-start
             (one-or-more anything))) "\nkey "))

(defconst gpl3 "[![License GPLv3](https://img.shields.io/badge/license-GPL_v3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.html) ")

(defconst passing "[ ![Codeship Status for yassinphilip/faustine](https://app.codeship.com/projects/c2385cd0-5dc6-0135-04b2-0a800465306c/status?branch=master)](https://app.codeship.com/projects/238325)")

(defun doc-a-mode ()
  "Search for next define-derived-mode and print markdown documentation."
  (interactive)

  (if (file-regular-p "README.md") (delete-file "README.md"))

  (with-temp-buffer
    (insert (documentation 'faust-mode))
    (goto-char (point-min))
    (when (re-search-forward myregexp-key) (setq heading (match-string 1)))

    (let* ((mylist (split-string (buffer-string) "^\\s-*$" t "\n"))
           (one (nth 0 mylist))
           (fiv (nth 4 mylist))
           (cmds (split-string fiv (rx
                                    (and line-start
                                         (zero-or-more not-newline))
                                    (submatch "faustine"))))
           (clean (split-string fiv (rx (and "\n")))))

      (with-temp-buffer
        (insert (format "\# Faustine\n\n%s\n---\n" heading))
        (insert gpl3)
        (insert passing)
        (insert "\n\n## Keys\n")

        (mapc (lambda (x)
                (insert (format "\n- %s" x)))
              (cdr clean))

        (insert "\n\n## Interactive functions")
        (mapcar (lambda (x)
                  (insert (format
                           "\n\n### %s"
                           (replace-regexp-in-string
                            (rx
                             (and "is an interactive" (zero-or-more not-newline)
                                  (or line-end "\n"))) "" x))))
                (mapcar (lambda (x)
                          (describe-function
                           (eval (read (format "(function faustine%s)" x)))))
                        (cdr cmds)))
        (goto-char (point-min))
        (while (re-search-forward
                (rx (and line-start
                         "\(")) nil t nil)
          (beginning-of-line)
          (kill-line 2))
        (goto-char (point-min))
        (while (re-search-forward
                (rx (and
                     "faustine.el")) nil t nil)
          (beginning-of-line)
          (kill-line 2)
          (write-region (buffer-string) nil "README.md" nil 0 nil nil))))))
