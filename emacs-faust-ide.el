;; emacs-faust-ide.el --- a Faust code editor for Emacs
;;
;; FAUST (Functional Audio Stream) is a functional programming language
;; specifically designed for real-time signal processing and synthesis.
;; FAUST targets high-performance signal processing applications and audio plug-ins
;; for a variety of platforms and standards.
;; http://faust.grame.fr
;;
;; Copyright (C) 2017, 2018 Yassin Philip
;; URL: https://bitbucket.org/yassinphilip/emacs-faust-ide-mode

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

;; (defun emacs-faust-ide-mode-indent-line ()
;;   "Indent current line as Faust code"
;;   (interactive)
;;   (beginning-of-line))

(require 'smie)

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

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dsp\\'" . emacs-faust-ide-mode))

(defun emacs-faust-ide-mode-indent-line ()
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

(defgroup emacs-faust-ide nil
  "Emacs Faust IDE - A lightweight IDE.
Customize `emacs-faust-ide-mode-build-options' for a lucky build"
  :group 'applications)

(defcustom emacs-faust-ide-build-target 'faust2jaqt
  "*The builder"
  ;;:type 'symbol
  :type '(choice
          (const :tag "faust2alsa" faust2alsa)
          (const :tag "faust2firefox" faust2firefox)
          (const :tag "faust2netjackconsole" faust2netjackconsole)
          (const :tag "faust2sigviewer" faust2sigviewer)
          (const :tag "faust2alsaconsole" faust2alsaconsole)
          (const :tag "faust2graph" faust2graph)
          (const :tag "faust2netjackqt" faust2netjackqt)
          (const :tag "faust2smartkeyb" faust2smartkeyb)
          (const :tag "faust2android" faust2android)
          (const :tag "faust2graphviewer" faust2graphviewer)
          (const :tag "faust2octave" faust2octave)
          (const :tag "faust2sndfile" faust2sndfile)
          (const :tag "faust2androidunity" faust2androidunity)
          (const :tag "faust2ios" faust2ios)
          (const :tag "faust2osxiosunity" faust2osxiosunity)
          (const :tag "faust2supercollider" faust2supercollider)
          (const :tag "faust2api" faust2api)
          (const :tag "faust2jack" faust2jack)
          (const :tag "faust2owl" faust2owl)
          (const :tag "faust2svg" faust2svg)
          (const :tag "faust2asmjs" faust2asmjs)
          (const :tag "faust2jackconsole" faust2jackconsole)
          (const :tag "faust2paqt" faust2paqt)
          (const :tag "faust2unity" faust2unity)
          (const :tag "faust2atomsnippets" faust2atomsnippets)
          (const :tag "faust2jackinternal" faust2jackinternal)
          (const :tag "faust2pdf" faust2pdf)
          (const :tag "faust2vst" faust2vst)
          (const :tag "faust2au" faust2au)
          (const :tag "faust2jackserver" faust2jackserver)
          (const :tag "faust2plot" faust2plot)
          (const :tag "faust2vsti" faust2vsti)
          (const :tag "faust2bela" faust2bela)
          (const :tag "faust2jaqt" faust2jaqt)
          (const :tag "faust2png" faust2png)
          (const :tag "faust2w32max6" faust2w32max6)
          (const :tag "faust2caqt" faust2caqt)
          (const :tag "faust2juce" faust2juce)
          (const :tag "faust2puredata" faust2puredata)
          (const :tag "faust2w32msp" faust2w32msp))
  :group 'emacs-faust-ide)

(defcustom emacs-faust-ide-mode-build-options "plop"
  "The type of build"
  :type '(string)
  :group 'emacs-faust-ide)

;; (defvar emacs-faust-ide-mode-map nil "Keymap for `emacs-faust-ide-mode'")

(defvar emacs-faust-ide-mode-map
   (let ((map (make-sparse-keymap)))
     (define-key map [?\C-c ?\C-b] 'emacs-faust-ide-mode-build)
     (define-key map [?\C-c ?\C-d] 'emacs-faust-ide-mode-diagram)
     (define-key map [?\C-c ?\C-r] 'emacs-faust-ide-mode-run)
     (define-key map [?\C-c ?\C-h] 'emacs-faust-ide-mode-web-doc)
     map)
   "Keymap for `emacs-faust-ide-mode'.")

 (defvar emacs-faust-ide-mode-syntax-table
   (let ((st (make-syntax-table)))
     (modify-syntax-entry ?/  ". 124b" st)
     (modify-syntax-entry ?*  ". 23" st)
     (modify-syntax-entry ?\n "> b" st)
     (modify-syntax-entry ?\^m "> b" st)
     st)
   "Syntax table for `emacs-faust-ide-mode'.")

(defvar faust-variables-regexp "[A-Za-z][A-Za-z]*")
(defvar faust-arguments-regexp "[0-9]")
(defvar faust-operator-regexp "\\([~!_@,<>:;]\\)")
(defvar faust-math-op-regexp "[=\+\{\}()/*-]")
(defvar faust-keywords-regexp (regexp-opt faust-keywords 'words))
(defvar faust-function-regexp (regexp-opt faust-functions 'words))
(defvar faust-ui-keywords-regexp (regexp-opt faust-ui-keywords 'words))

;; create the list for font-lock.
(defvar emacs-faust-ide-mode-font-lock-keywords
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
(define-derived-mode emacs-faust-ide-mode c-mode "Emacs-Faust-Ide-Mode" "
         .' '.
-        .   .            \\\\       Emacs Faust IDE
 `.        .         .  -{{{:}      A lightweight IDE.
   ' .  . ' ' .  . '      //

Type \\[customize-group] emacs-faust-ide (or use the menu)  to set it up.
Available commands while editing Faust (*.dsp) files:

\\{emacs-faust-ide-mode-map}"
  (kill-all-local-variables)
  (set-syntax-table emacs-faust-ide-mode-syntax-table)
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  ;; (setq-local comment-start-skip "#+\\s-*")
  (setq-local font-lock-defaults
              '(emacs-faust-ide-mode-font-lock-keywords))
  ;; (setq-local indent-line-function 'emacs-faust-ide-mode-indent-line)
  ;; (smie-setup nil #'ignore)
  ;; (smie-setup faust-grammar #'ignore)

  (use-local-map emacs-faust-ide-mode-map)

  (setq mode-name "emacs-faust-ide-mode")
  (setq major-mode 'emacs-faust-ide-mode)
  (message "########### MODE OK & emacs-faust-ide-build-target : %s" emacs-faust-ide-build-target))

;; Functions

(defun emacs-faust-ide-mode-web-doc (start end)
  "Websearch selected string on the faust.grame.fr web site"
  (interactive "r")
  (let ((q (buffer-substring-no-properties start end)))
    (browse-url (concat "http://faust.grame.fr/library.html#"
                        (url-hexify-string q)))))

(defun emacs-faust-ide-mode-build ()
  "Build the executable generated by this Faust code file using `emacs-faust-ide-build-target'"
  (interactive)
  (setq dsp-buffer (current-buffer))
  (with-current-buffer (get-buffer-create "Faust output")
    (pop-to-buffer "Faust output" nil t)
    ;; (previous-window)
    (goto-char (point-max))
    (start-process-shell-command "Build executable"
                                 (current-buffer)
                                 (format "%s *.dsp" emacs-faust-ide-build-target))

    (start-process-shell-command "Build diagrams"
                                 (current-buffer)
                                 "faust2svg *.dsp")
    (other-window -1)
    (pop-to-buffer dsp-buffer nil t)))

(defun emacs-faust-ide-mode-diagram ()
  "Show Faust diagram(s) in a web page using default browser"
  (interactive)
  (if (string-match-p "^[a-z0-9A-Z]?+\\.dsp$" (file-name-nondirectory buffer-file-name))
      (progn
        (message "Building diagrams")
        (with-current-buffer (get-buffer-create "Faust output")
          (pop-to-buffer "Faust output" nil t)
          (goto-char (point-max))
          (call-process "/bin/bash" nil t nil "-c" "faust2svg *.dsp")
          (other-window -1)
          (pop-to-buffer dsp-buffer nil t))
        (setq temp-file-name "faust-graphs.html")
        (setq mylist (directory-files (file-name-directory buffer-file-name) nil "^[a-z0-9A-Z]?+\\.dsp$"))
        (emacs-faust-ide-mode-build-temp-file mylist temp-file-name)
        (emacs-faust-ide-mode-show-graph temp-file-name)
        )
    (message "Not a regular Faust file")))

(defun emacs-faust-ide-mode-run ()
  "Run the executable generated by the Faust code file in the current buffer"
  (interactive)
  (setq dsp-buffer (current-buffer))
  (setq dsp-buffer-name (buffer-name))
    (with-current-buffer (get-buffer-create "Faust output")
    (pop-to-buffer "Faust output" nil t)
    ;; (previous-window)
    (goto-char (point-max))
    (start-process-shell-command "Build" (current-buffer)
                               (format "./%s" (file-name-sans-extension
                                               (file-name-nondirectory
                                                dsp-buffer-name))))
    (other-window -1)
    (pop-to-buffer dsp-buffer nil t)))

(defun emacs-faust-ide-mode-diagram ()
  "Show Faust diagram(s) in a web page using default browser"
  (interactive)
  (if (string-match-p "^[a-z0-9A-Z]?+\\.dsp$" (file-name-nondirectory buffer-file-name))
      (progn
        (message "Building diagrams")
        (setq temp-file-name "faust-graphs.html")
        (setq mylist (directory-files (file-name-directory buffer-file-name) nil "^[a-z0-9A-Z]?+\\.dsp$"))
        (emacs-faust-ide-mode-build-temp-file mylist temp-file-name)
        (emacs-faust-ide-mode-show-graph temp-file-name))
    (message "Not a regular Faust file")))

(defun emacs-faust-ide-mode-show-graph (html-page)
  "Show Faust diagram(s) in a web page using default browser"
  (interactive)
  (browse-url-of-file html-page))

(defun emacs-faust-ide-mode-show-mdoc (html-page)
  "Show Faust full mathdoc PDF in default browser"
  (interactive)
  (browse-url-of-file html-page))

(defun emacs-faust-ide-mode-build-temp-file (list temp-file-name)
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
<body>\n" nil temp-file-name)
  (write-region (format "<h4>Rendered %s</h4>\n" (current-time-string)) nil temp-file-name 'append 0 nil nil)
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
                     dsp-file-name dsp-file-name) nil temp-file-name 'append 0 nil nil))))
    (setq list (cdr list)))
  (write-region "</body>
</html>\n" nil temp-file-name 'append 0 nil nil))

(provide 'emacs-faust-ide)
