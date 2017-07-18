;; faust-ide-mode.el --- a purely IMAP based email client for EMACS

;; Copyright (C) 2017, 2018 Yassin Philip
;; URL: https://bitbucket.org/yassinphilip/faust-ide-mode

;; Author: Yassin Philip <xaccrocheur@gmail.com>
;; Keywords: faust
;; Version 0.5b

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

;; (defun faust-ide-mode-indent-line ()
;;   "Indent current line as Faust code"
;;   (interactive)
;;   (beginning-of-line))

(require 'smie)

(defvar faust-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((exp ("{" insts "}"))
      '((assoc ";"))
      '((assoc ":"))
      '((assoc ","))
      ;; (insts (exp) (insts ";" insts))
      )
    )))

;; (defun factor-smie-rules (kind token)
;;   (pcase (cons kind token)
;;     (`(:elem . basic) 4)
;;     (`(:after . ,(or `"HELLO")) 4)
;;     ))

;; (defun faust-rules (kind token)
;;   (pcase (cons kind token)
;;     (`(:elem . basic) fsharp-indent-level)
;;     (`(:after . "do") fsharp-indent-level)
;;     (`(:after . "then") fsharp-indent-level)
;;     (`(:after . "else") fsharp-indent-level)
;;     (`(:after . "try") fsharp-indent-level)
;;     (`(:after . "with") fsharp-indent-level)
;;     (`(:after . "finally") fsharp-indent-level)
;;     (`(:after . "in") 0)
;;     (`(:after . ,(or `"[" `"]" `"[|" `"|]")) fsharp-indent-level)
;;     (`(,_ . ,(or `";" `",")) (if (smie-rule-parent-p "begin")
;;                                  0
;;                                (smie-rule-separator kind)))
;;     (`(:after . "=") fsharp-indent-level)
;;     (`(:after . ";;") (smie-rule-separator kind))
;;     (`(:before . ";;") (if (smie-rule-bolp)
;;                            0))
;;     ))


(defun faust-ide-mode-indent-line ()
  "Indent current line of Faust code."
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
        (indent (condition-case nil (max (sample-calculate-indentation) 0)
                  (error 0))))
    (if savep
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))


(if (bobp)  ; Check for rule 1
    (indent-line-to 0))

(defvar faust-keywords
  '("process" "with" "case" "seq" "par" "sum" "prod"
    "include" "import" "component" "library" "environment" "declare"
    "define" "undef" "error" "pragma" "ident"
    "if" "def" "else" "elif" "endif" "line" "warning"))

(defvar faust-functions
  '("mem" "prefix" "int" "float"
    "rdtable" "rwtable" "select2" "select3"
    "ffunction" "fconstant" "fvariable"
    "attach" "acos" "asin" "atan" "atan2" "cos" "sin" "tan" "exp"
    "log" "log10" "pow" "sqrt" "abs" "min" "max" "fmod"
    "remainder" "floor" "ceil" "rint"))

(defvar faust-ui-keywords
  '("button" "checkbox" "vslider" "hslider" "nentry"
    "vgroup" "hgroup" "tgroup" "vbargraph" "hbargraph"))

(defgroup faust-ide-mode nil
  "Mail-bug - A lightweight IDE.
Customize `faust-ide-mode-build-options' for a lucky build"
  :group 'applications)

(defcustom faust-ide-mode-build-options "plop"
  "The type of build"
  :type '(string)
  :group 'faust-ide-mode)

;; (defvar faust-ide-mode-map nil "Keymap for `faust-ide-mode'")

(defvar faust-ide-mode-map
   (let ((map (make-sparse-keymap)))
     (define-key map [?\C-c ?\C-b] 'faust-ide-mode-build)
     map)
   "Keymap for `faust-ide-mode'.")

 (defvar faust-ide-mode-syntax-table
   (let ((st (make-syntax-table)))
     (modify-syntax-entry ?/  ". 124b" st)
     (modify-syntax-entry ?*  ". 23" st)
     (modify-syntax-entry ?\n "> b" st)
     (modify-syntax-entry ?\^m "> b" st)
     st)
   "Syntax table for `faust-ide-mode'.")

(defvar faust-variables-regexp "[A-Za-z][A-Za-z]*")
(defvar faust-arguments-regexp "[0-9]")
(defvar faust-operator-regexp "\\([~!_@,<>:;]\\)")
(defvar faust-math-op-regexp "[=\+\{\}()/*-]")
(defvar faust-keywords-regexp (regexp-opt faust-keywords 'words))
(defvar faust-function-regexp (regexp-opt faust-functions 'words))
(defvar faust-ui-keywords-regexp (regexp-opt faust-ui-keywords 'words))

;; create the list for font-lock.
(defvar faust-ide-mode-font-lock-keywords
  `(
    (,faust-function-regexp . font-lock-type-face)
    (,faust-ui-keywords-regexp . font-lock-builtin-face)
    (,faust-math-op-regexp . font-lock-function-name-face)
    (,faust-operator-regexp . font-lock-constant-face)
    (,faust-keywords-regexp . font-lock-keyword-face)
    ;;    (,faust-variables-regexp . font-lock-variable-name-face)
    ;;    (,faust-arguments-regexp . font-lock-warning-face)
    ))

;;;###autoload
(define-derived-mode faust-ide-mode c-mode "Faust-Ide-Mode" "
         .' '.
-        .   .            \\\\       faust-ide-mode
 `.        .         .  -{{{:}     A lightweight IDE.
   ' .  . ' ' .  . '      //

Type \\[customize-group] faust-ide-mode (or use the menu)  to set it up.
Available commands while editing Faust (*.dsp) files:

\\{faust-ide-mode-map}"
  (kill-all-local-variables)

  ;; :syntax-table faust-ide-mode-syntax-table
  (set-syntax-table faust-ide-mode-syntax-table)
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  ;; (setq-local comment-start-skip "#+\\s-*")
  (setq-local font-lock-defaults
              '(faust-ide-mode-font-lock-keywords))
  ;; (setq-local indent-line-function 'faust-ide-mode-indent-line)
  ;; (smie-setup nil #'ignore)
  (smie-setup faust-grammar #'ignore)

  ;; (setq c-default-style "linux"
  ;;       c-basic-offset 4)

  (use-local-map faust-ide-mode-map)

  (add-to-list 'auto-mode-alist '("\\.dsp\\'" . faust-ide-mode))
  (setq mode-name "faust-ide-mode")
  (setq major-mode 'faust-ide-mode)
  (message "########### MODE OK"))

;; Functions

(defun faust-ide-mode-build ()
  "actually build"
  (interactive)
  (setq dsp-buffer (current-buffer))
  (with-current-buffer (get-buffer-create "Faust output")
    (pop-to-buffer "Faust output" nil t)
    ;; (previous-window)
    (goto-char (point-max))
    (start-process-shell-command "Build" (current-buffer) "faust2svg *.dsp")
    (other-window -1)
    (pop-to-buffer dsp-buffer nil t)
    ))

(defun faust-ide-mode-diagram ()
  "Show Faust diagram(s) in a web page using default browser"
  (interactive)
  (if (string-match-p "^[a-z0-9A-Z]?+\\.dsp$" (file-name-nondirectory buffer-file-name))
      (progn
        (message "Building diagrams")
        (setq temp-file-name "~/tmp/faust-graphs.html")
        (setq mylist (directory-files (file-name-directory buffer-file-name) nil "^[a-z0-9A-Z]?+\\.dsp$"))
        (faust-ide-mode-build-temp-file mylist temp-file-name)
        (faust-ide-mode-show-graph temp-file-name))
    (message "Not a regular Faust file")))

(defun faust-ide-mode-build-temp-file (list temp-file-name)
  "Print each element of LIST on a line of its own."
  (if (file-regular-p temp-file-name)
      (delete-file temp-file-name))
  (write-region "<html>
<style>
html {
  background-color: #ccc;
  font-family: sans-serif;
}
figure {
  float: right;
  width: 30%;
  text-align: center;
  font-style: italic;
  font-size: smaller;
  text-indent: 0;
  border: thin silver solid;
  margin: 0.2em;
  padding: 0.1em;
}
img.scaled {
  width: 100%;
}
</style>
<body>\n" nil tempfile)
  (write-region (format "<h4>Rendered %s</h4>\n" (current-time-string)) nil tempfile 'append 0 nil nil)
  (while list
    (if (file-regular-p (car list))
        (progn
          (setq dsp-element (file-name-sans-extension (car list)))
          (setq dsp-dir (file-name-directory buffer-file-name))
          (let ((dsp-element (file-name-sans-extension (car list)))
                (dsp-file-name (car list))
                (dsp-dir (file-name-directory buffer-file-name)))
            (write-region
             (format "
<figure>
  <a href='%s%s-svg/process.svg'>
<img class=scaled src='%s%s-svg/process.svg'
    alt='%s'></a>
  <figcaption>%s</figcaption>
</figure>
\n"
                     dsp-dir
                     dsp-element
                     dsp-dir
                     dsp-element
                     dsp-file-name dsp-file-name) nil tempfile 'append 0 nil nil))))
    (setq list (cdr list)))
  (write-region "</body>
</html>\n" nil tempfile 'append 0 nil nil))

(defun faust-ide-mode-show-graph (html-page)
  "Show Faust diagram(s) in a web page using default browser"
  (interactive)
  (browse-url-of-file html-page))

(provide 'faust-ide-mode)
