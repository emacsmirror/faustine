
(setq myregexp
      (rx
       (submatch
        (and word-start
             (one-or-more word) "." (eval faustine-faust-extension) ":" (one-or-more digit)))))

(submatch (and (or "./" "/") (one-or-more (any word "-"))))

(setq myregexp
      (rx
       "define-derived-mode"
       space
       (submatch
        (and word-start
             (one-or-more (any word "-"))
             ;; (eval faustine-faust-extension)
             ;; ":"
             ;; (one-or-more digit)
             )
        )

       ;; (submatch
       ;;  (and
       ;;   line-start
       ;;   ;; (one-or-more not-newline)
       ;;   ))
       ))

(setq myregexp-key
      (rx
       (submatch
        ;; line-start
        ;; (one-or-more word)
        (and word-start
             (one-or-more anything))
        )
       "\nkey "
       ;; space
       ;; (submatch
       ;;  (and word-start
       ;;       (one-or-more (any word "-"))))
       ))


(setq myregexp-keys
      (rx
       "-------\n"
       (submatch
        (and "\n" (not blank)
             (one-or-more not-newline)
             "\n" (not blank)
             (one-or-more not-newline)
             "\n" (not blank)))
       (submatch
        (and
         ;; line-end
         line-start
         (one-or-more not-newline)
         ;; (and "\n" (one-or-more not-newline))
         ;; (one-or-more not-newline)
         ;; line-end
         ;; (one-or-more (not blank))
         ;; "\n" (not blank)
         ;; "C-M-q"
         ;; (one-or-more not-newline)
         ;; line-start
         ;; blank
         )
        ;; line-start
        ;; "\n"
        ;; (and
        ;;  ;; (one-or-more (any word "\n"))
        ;;  ;; (one-or-more word)
        ;;  )
        ;; "\n" (not blank)
        ;; (and (or "\n") (one-or-more (any word "\n")))


        ;; (and "\n" (not blank))
        )
       ;; space
       ;; (submatch
       ;;  (and word-start
       ;;       (one-or-more (any word "-"))))
       ))

(documentation 'faust-mode)

Faustine is a lightweight IDE that leverages the mighty power of the faust executable.

Use `faustine-configure' (M-x faustine-configure) to set it up.
Available commands while editing Faust (*.dsp) files:

key             binding
---             -------

C-c             Prefix Command
ESC             Prefix Command

C-c C-b         faustine-build
C-c C-c         faustine-syntax-check
C-c C-d         faustine-diagram
C-c C-h         faustine-online-doc
C-c RET         faustine-mdoc
C-c C-o         faustine-toggle-output-buffer
C-c C-p         faustine-configure
C-c C-s         faustine-source-code
C-c r           faustine-run
C-c C-S-b       faustine-build-all
C-c C-S-d       faustine-diagram-all

C-M-q           prog-indent-sexp



In addition to any hooks its parent mode `prog-mode' might have run,
this mode runs the hook `faust-mode-hook', as the final step
during initialization.

(defun skipline ()
  (interactive)
  (message "point: %s" (point))
  (let ((start (point))
        (end (progn (forward-paragraph) (point))))
    (identity start) (identity end)))

(defun killreg ()
  "Plop."
  (interactive)
  ;; (goto-char (point-min))
  (if (re-search-forward myregexp-key)
      (progn
        (message "Found (%s) AND (%s)"
                 (match-string 0)
                 (match-string 1)
                 ;; (match-string 2)
                 ))))

(require 'cl)

(setq myregexp-key
      (rx
       (submatch
        (and word-start
             (one-or-more anything))) "\nkey "))

;; [![License GPLv3](https://img.shields.io/badge/license-GPL_v3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.html)


(defun doc-a-mode ()
  "Searches for next define-derived-mode and print markdown documentation."
  (interactive)

  (if (file-regular-p "README.md") (delete-file "README.md"))

  (with-temp-buffer
    (insert (documentation 'faust-mode))
    (goto-char (point-min))
    (when (re-search-forward myregexp-key)
      (setq heading (match-string 1))
      (message "Heading (%s)" heading))

    (let* ((mylist (split-string (buffer-string) "^\\s-*$" t "\n"))
           (one (nth 0 mylist))
           (fiv (nth 4 mylist))
           (cmds (split-string fiv (rx
                                    (and
                                     line-start
                                     (zero-or-more not-newline))
                                    (submatch
                                     "faustine"))))
           (clean (split-string fiv (rx (and "\n")))))

      (with-temp-buffer
        (insert "\# Faustine\n")
        (insert (format "\n%s\n\n" heading))
        (insert "\n---\n")
        (insert "[![License GPLv3](https://img.shields.io/badge/license-GPL_v3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.html) ")

        (insert "[ ![Codeship Status for yassinphilip/faustine](https://app.codeship.com/projects/c2385cd0-5dc6-0135-04b2-0a800465306c/status?branch=master)](https://app.codeship.com/projects/238325)")

        (insert "\n## Keys\n")
        (insert "\n<kbd>CTRL</kbd>+<kbd>Z</kbd>\n")

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
          (write-region (buffer-string) nil "README.md" 'append 0 nil nil)
          (find-file "README.md"))))))

(require 'markdown-mode)

(format "%s" (replace-regexp-in-string
              (format
               "\n\n# %s"
               (replace-regexp-in-string
                (rx
                 (and
                  "is an interactive"
                  (zero-or-more not-newline)
                  line-end))
                "" "faustine-build is an interactive blah

(faustine-build &optional BUILD-ALL)

Build the current buffer/file executable(s).
If BUILD-ALL is set, build all Faust files referenced by this one."))
(rx
 (and
  word-start
  "current"

  )) "plop"))

(write-region "</div>
</body>
</html>\n" nil faustine-diagram-page-name 'append 0 nil nil)

;; (if (re-search-forward
;;      (rx
;;       "define-derived-mode"
;;       space
;;       (submatch
;;        (and
;;         word-start
;;         (one-or-more
;;          (any word "-"))
;;         ))))
;;     (progn
;;       (setq mode (buffer-substring-no-properties
;;                   (match-beginning 1) (match-end 1)))
;;       (message "Found Mode: (%s)" mode)

(let ((printable (describe-function
                  (eval (read (format "(function %s)" mode))))))
  (message "%s" printable))

;;       ;; (documentation 'faust-mode)
;;       ;; (message "Doc: %s" (documentation 'faust-mode))
;;       ))


(define-derived-mode faustine-output-mode fundamental-mode
  "The Faust output buffer mode."
  (kill-all-local-variables)
  (setq font-lock-defaults '(faustine-output-mode-font-lock-keywords t))
  (font-lock-fontify-buffer))

plop // plop

(defvar faustine-faust-extension "dsp")

(defun testoy (args)
  "plop"
  (interactive "P")
  (let ((plop "plop"))
    (message "plop: %s" plop))

  )

(defcustom faustine-faust-extension "dsp"
  "The Faust files extension."
  :type '(string)
  :group 'faustine)

(defvar faustine-regexp-faust-file
  (rx
   "\"" (submatch (and word-start (one-or-more word) "." (eval faustine-faust-extension))) "\"")
  "The regexp to match `something.faust'.")

comp =  component("zob.dsp");
;; comp =  component("rr.dsp");

comp =  component("comp.dsp");

;; piop

(defun replop ()
  "plop"
  (interactive)

  (while
      (re-search-forward faustine-regexp-faust-file nil t nil)
    (if
        (not (eq 'comment (syntax-ppss-context (syntax-ppss))))
        (message "found: (%s)" (match-string-no-properties 1)))
    )
  )

(defun skipit ()
  "plop"
  (interactive)
  (when
      (or
       (looking-at (rx
                    (submatch (zero-or-more not-newline))
                    (syntax comment-start)
                    ))
      (looking-back (rx
                     (and
                      (syntax comment-start)
                      (zero-or-more not-newline)))))
    (progn
      (forward-line))))

(defun comment-p ()
  "Return non-nil iff current line has only a comment."
  (interactive)
  (save-excursion
    (end-of-line) ; plop
    (if (eq 'comment (syntax-ppss-context (syntax-ppss)))
        (message "comment!") ; test
      (message "not comment! Line: %s"
               (buffer-substring-no-properties
                (line-beginning-position) (line-end-position))) ; test
      ;; (with-syntax-table faustine-mode-syntax-table
      ;;   (forward-comment arg))
      (back-to-indentation)
      ;; (looking-at (rx (or (syntax comment-start) line-end)))
      )
    )
  (forward-line)
  (with-syntax-table emacs-lisp-mode-syntax-table
    (forward-comment (point-max)))
  )

(defun python-skip-comments/blanks (&optional backward)
  "Skip comments and blank lines.
BACKWARD non-nil means go backwards, otherwise go forwards.
Backslash is treated as whitespace so that continued blank lines
are skipped.  Doesn't move out of comments -- should be outside
or at end of line."
  (interactive)
  (let ((arg (if backward
                 ;; If we're in a comment (including on the trailing
                 ;; newline), forward-comment doesn't move backwards out
                 ;; of it.  Don't set the syntax table round this bit!
                 (let ((syntax (syntax-ppss)))
                   (if (nth 4 syntax)
                       (goto-char (nth 8 syntax)))
                   (- (point-max)))
               (point-max))))
    ;; (with-syntax-table faustine-mode-syntax-table
    ;;   (forward-comment arg))
    (with-syntax-table emacs-lisp-mode-syntax-table
      (forward-comment arg))))

(setq myregexp
      (rx

       (and
        line-start
        (zero-or-more blank)
        (syntax comment-start)
        (one-or-more not-newline)
        line-end)

       ;; (and
       ;;  line-start
       ;;  (zero-or-more (not blank))
       ;;  (syntax comment-start)
       ;;  ;; (not-syntax comment-start)
       ;;  ;; (zero-or-more (not blank))
       ;;  ;; (zero-or-more not-newline)
       ;;  ;; (not-syntax comment-end)
       ;;  line-end
       ;;  )
       ))

(rx bol (group (+ (not (syntax comment-start)))))
(rx bol (group (+ (not (syntax comment-start)))))


(setq regexp-nocomment
      (rx
       bol
       (group
        (+
         (not
          (syntax comment-start)
          )
         )
        )
       ))

(setq regexp-comment ; commentaire
      (rx
       ;; line-st
       (and
        ;; word-start
            ;; word
            (one-or-more anything)
            ;; space
            ;; (one-or-more space)
            ;; (zero-or-more anything)
            ;; (syntax comment-start)
            )
            )
       ;; (submatch
       ;;  (syntax comment-start))
       )

plip  ;; plopz

sdhjkqsd qshqskjhq qsdhkqsdjh ;; ceci n'est pas du code

(setq regexp-string
      (rx
       (and
        word-start
        "plop")
       ))

;;
ploup ; plop
;; plop
plip  ;; plop
plop
zob

(defun plop ()
  (interactive)
  (setq mypoint (point))
  (save-excursion
    (if
        (re-search-forward regexp-nocomment (line-end-position) t 1)

        (progn
          (setq fmatch (match-string-no-properties 0))
          (save-excursion
            (if
              (re-search-forward regexp-comment (line-end-position) t 1)
              (progn
                (message "Found (%s) but it contains comments" fmatch))
            (progn
              (message "duh")
              (goto-char mypoint)
                (if
                    (re-search-forward regexp-string (line-end-position) t 1)
                    (progn
                      (message "Found (%s)!" (match-string-no-properties 0)))
                  (progn
                    (message "doh")
                    (message "point: %s" (point))))))))
      (progn
        (message "comments! (or empty)")
        (message "point: %s" (point))))))


(looking-at (rx (or (syntax comment-start) line-end))); plop

(setq myregexp
      (rx
       (submatch
        (and
         (or "./" "/")
         (one-or-more (any word "/"))))
       ";"))

plop

(defun testreg ()
  "plop"
  (interactive)
  (let (lines)
    (when (re-search-forward (rx bol (not (syntax comment-start)))
                             (line-end-position)
                             t)
      (push (buffer-substring-no-properties
             (line-beginning-position)
             (line-end-position))
            lines))
    (message "lines: %s" lines))
  )

(save-excursion
  (goto-char (point-min))
  (let (lines)
    (while (< (point) (point-max))
      (when (re-search-forward (rx bol (group (+ (not (syntax comment-start)))))
                               (line-end-position)
                               t)
        (push (match-string-no-properties 1) lines))
      (forward-line))
    (nreverse lines)))

foo

;; bar
baz
alice ; bob

(defun faustine-project-files (fname blist)
  "Recursively find all Faust links in FNAME, canonicalize and put them in BLIST, return BLIST."
  (add-to-list 'blist (expand-file-name fname))
  (with-temp-buffer
    (insert-file-contents-literally fname)
    (goto-char (point-min))
    (while (re-search-forward faustine-regexp-faust-file nil t)
      (when (match-string 0)
        (let ((uri (expand-file-name (match-string 1))))
          (if (not (member uri blist))
              (setq blist (faustine-project-files uri blist))))))
    (identity blist)))

;; plzefkjzef plop.dsp
plzefkjzef plop.dsp:05
;; 01:41:03 | Check:"panpot.dsp":20 finished

plop./plop/plop/plop;./plip/ploup;


(defun killreg ()
  (interactive)
  ;; (goto-char (point-min))
  (if (re-search-forward myregexp)
      (progn
        (message "Found %s"
                 (buffer-substring-no-properties
                  (match-beginning 0) (match-end 0))

                 ))))

ins 2
outs 2

physical input system:capture_1

The buffer size is now


The sample rate is now 44100/sec
physical input system:capture_2

(defvar faustine-faust-parameters-regexp "\\[.*?\\]")

(defun test-reg ()
  (interactive)
  ;; (goto-char (point-min))
  (if (re-search-forward (rx bol (group (+ (not (syntax comment-start)))))
                         (line-end-position) t 1)
      (message "1: %s" (match-string-no-properties 1)

               )
    (progn (message "doh!")
           (forward-line))
    ))

;; "\(.*?[A-Za-z0-9.-]+\);"
;; "\\(.*?[A-Za-z0-9.-]+\\);"

./plop/plop/plop;/plip/ploup;

/plop/plop/plop


;; ".*[\\/]*\\;"
;; ".*[\/]*\;"
;; "\(.*/?[A-Za-z0-9.-]+\);"


"zzplop.lib"
lkjlkj.lib:12


(string-remove-prefix "\"" (string-remove-suffix "\"" (message "%s" "\"plop\"")))





"plop.lib"

"\\([^\"\\]*\\(?:\\.[^\"\\]*\\)lib*\\)"

"\"\\(.+.lib\\)\""

"\\\"\\(.+.lib\\)\\\""
