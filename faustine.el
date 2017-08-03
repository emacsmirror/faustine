;;; faustine.el --- Edit, visualize, build and run Faust code
;;; Version:1.0
;; Package-Requires: ((emacs "24"))
;;
;; FAUST (Functional Audio Stream) is a functional programming language
;; specifically designed for real-time signal processing and synthesis.
;; FAUST targets high-performance signal processing applications and audio plug-ins
;; for a variety of platforms and standards.
;; http://faust.grame.fr
;;
;; Copyright (C) 2017, 2018 Yassin Philip
;; URL: https://bitbucket.org/yassinphilip/faustine
;;
;; Author: Yassin Philip <xaccrocheur@gmail.com>
;; Keywords: tools, modes, faust
;;
;; This file is NOT part of GNU Emacs.
;;
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
;;
;;; Commentary:
;; See README.md for details.
;;
;;; Code:


(require 'smie)

(defgroup faustine nil
  "Faustine - A lightweight Emacs Faust IDE"
  :group 'tools)

(defgroup keyboard-shortcuts nil
  "Faustine keyboard shortcuts"
  :group 'faustine)

(defcustom faustine-kb-configure "C-c C-p"
  "Configure Faustine."
  :type 'string
  :group 'keyboard-shortcuts)

(defcustom faustine-kb-build "C-c C-b"
  "Build the current buffer/file executable using the `faustine-build-backend' script."
  :type '(string)
  :group 'keyboard-shortcuts)

(defcustom faustine-kb-build-all "C-c C-S-b"
  "Build all project files."
  :type '(string)
  :group 'keyboard-shortcuts)

(defcustom faustine-kb-diagram "C-c C-d"
  "Generate the current buffer/file Faust diagram."
  :type '(string)
  :group 'keyboard-shortcuts)

(defcustom faustine-kb-diagram-all "C-c C-S-d"
  "Generate all project files Faust diagrams."
  :type '(string)
  :group 'keyboard-shortcuts)

(defcustom faustine-kb-online-doc "C-c C-h"
  "Websearch the selected string on the faust.grame.fr library web site."
  :type '(string)
  :group 'keyboard-shortcuts)

(defcustom faustine-kb-toggle-output-buffer "C-c C-o"
  "Show/hide Faust output buffer."
  :type '(string)
  :group 'keyboard-shortcuts)

(defcustom faustine-kb-mdoc "C-c C-m"
  "Generate Faust mdoc of the current faust buffer/file."
  :type '(string)
  :group 'keyboard-shortcuts)

(defcustom faustine-kb-run "C-c r"
  "Run the current buffer/file executable."
  :type '(string)
  :group 'keyboard-shortcuts)

(defcustom faustine-kb-source-code "C-c C-s"
  "Generate C++ source code of the current faust buffer/file."
  :type '(string)
  :group 'keyboard-shortcuts)

(defcustom faustine-kb-syntax-check "C-c C-c"
  "Check if Faust code buffer compiles."
  :type '(string)
  :group 'keyboard-shortcuts)

(defcustom faustine-pop-output-buffer nil
  "Pop open the Faust output buffer at each command call."
  :type '(boolean)
  :group 'faustine)

(defcustom faustine-output-buffer-name "*Faust*"
  "The name of the Faust output buffer.
Surround it with \"*\" to hide it in special buffers."
  :type '(string)
  :group 'faustine)

(defcustom faustine-c++-buffer-name "*Faust C++*"
  "The name of the Faust C++ code output buffer.
Surround it with \"*\" to hide it in special buffers."
  :type '(string)
  :group 'faustine)

(defcustom faustine-diagram-page-name "faust-graphs.html"
  "The name of the Faust diagrams HTML page."
  :type '(string)
  :group 'faustine)

(defcustom faustine-faust-libs-dir "/usr/local/share/faust/"
  "The Faust library directory for direct linking.
This is only for use with the command `faustine-online-doc'."
  :type '(string)
  :group 'faustine)

(defcustom faustine-faust-extension "dsp"
  "The Faust files extension."
  :type '(string)
  :group 'faustine)

(defcustom faustine-build-backend 'faust2jaqt
  "The Faust code-to-executable build backend."
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
  :group 'faustine)

(defvar ac-sources)
(defvar ac-modes)
(defvar ac-user-dictionary)
(defvar faustine-path (file-name-directory (or load-file-name (buffer-file-name))))
(defvar faustine-process-source-buffer nil
  "Source buffer from which the current process is generating mdoc.")

(make-variable-buffer-local 'faustine-process-source-buffer)

(defvar faustine-green-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for the function `faustine-green-mode'.")

(defvar faustine-red-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for the function `faustine-red-mode'.")

(defvar faustine-regexp-dsp
  (concat "\"\\([^\"\\(]+\\.\\(" faustine-faust-extension "\\)\\)\"")
  "The regexp to search for `\"something.faust\"'.")

(defvar faustine-regexp-log
  (concat "\\([A-Za-z]*\." faustine-faust-extension "\\)\:\\([0-9]+\\)")
  "The regexp to search for `something.faust'.")

(defconst faustine-regexp-lib
  "\\\"\\([^\\\"\\\\(]+\\.lib\\)\\\""
  "The regexp to search for `\"something.lib\"'.")

(defconst faustine-regexp-exe
  "\\(.*?[A-Za-z0-9.-]+\\);"
  "The regexp to search for `/some/thing;'.")

(easy-menu-define
  faustine-green-mode-menu
  faustine-green-mode-map
  "Green bug menu"
  '("Faustine"
    ["Syntax: OK" faustine-toggle-output-buffer t]
    "----------------"
    ["Generate C++ code" faustine-source-code t]
    ["Generate diagram" faustine-diagram t]
    ["Build executable" faustine-build t]
    ["Run executable" faustine-run t]
    ("Project"
     ["Generate all linked diagrams" faustine-diagram-all t]
     ["Build all linked executables" faustine-build-all t])
    ["Preferences" faustine-configure t]))

(easy-menu-define
  faustine-red-mode-menu
  faustine-red-mode-map
  "Red bug menu"
  '("Faustine"
    ["Syntax: ERROR" faustine-toggle-output-buffer t]
    "----------------"
    ["Preferences" faustine-configure t]))

(defvar faustine-green-mode-bug
  (list
   " "
   (propertize
    "Syntax: OK"
    'display
    `(image :type xpm
            :ascent center
            :file ,(expand-file-name "icons/greenbug.xpm" faustine-path)))))

(defvar faustine-red-mode-bug
  (list
   " "
   (propertize
    "Syntax: ERROR"
    :help-echo "Tst string"
    'display
    `(image :type xpm
            :ascent center
            :file ,(expand-file-name "icons/redbug.xpm" faustine-path)))))

(put 'faustine-green-mode-bug 'risky-local-variable t)
(put 'faustine-red-mode-bug 'risky-local-variable t)

(define-minor-mode faustine-green-mode
  "Minor mode to display a green bug in the mode-line."
  :lighter faustine-green-mode-bug
  :keymap faustine-green-mode-map)

(define-minor-mode faustine-red-mode
  "Minor mode to display a red bug in the mode-line."
  :lighter faustine-red-mode-bug
  :keymap faustine-red-mode-map)

(defconst faustine-faust-keywords
  '("process" "with" "case" "seq" "par" "sum" "prod"
    "include" "import" "component" "library" "environment" "declare"
    "define" "undef" "error" "pragma" "ident"
    "if" "def" "else" "elif" "endif" "line" "warning"))

(defconst faustine-faust-functions
  '("mem" "prefix" "int" "float"
    "rdtable" "rwtable" "select2" "select3"
    "ffunction" "fconstant" "fvariable"
    "attach" "acos" "asin" "atan" "atan2" "cos" "sin" "tan" "exp"
    "log" "log10" "pow" "sqrt" "abs" "min" "max" "fmod"
    "remainder" "floor" "ceil" "rint"))

(defconst faustine-faust-ui-keywords
  '("button" "checkbox" "vslider" "hslider" "nentry"
    "vgroup" "hgroup" "tgroup" "vbargraph" "hbargraph"))

(defvar faustine-mode-map
   (let ((map (make-sparse-keymap)))
     map)
   "Keymap for `faustine-mode'.")

 (defvar faustine-mode-syntax-table
   (let ((st (make-syntax-table)))
     (modify-syntax-entry ?/  ". 124b" st)
     (modify-syntax-entry ?*  ". 23" st)
     (modify-syntax-entry ?\n "> b" st)
     (modify-syntax-entry ?\^m "> b" st)
     st)
   "Syntax table for `faustine-mode'.")

(defvar faustine-faust-variables-regexp "[A-Za-z][A-Za-z]*")
(defvar faustine-faust-arguments-regexp "[0-9]")
(defvar faustine-faust-operator-regexp "\\([~!_@,<>:;]\\)")
(defvar faustine-faust-math-op-regexp "[=\+\{\}()/*-]")
(defvar faustine-faustine-faust-keywords-regexp (regexp-opt faustine-faust-keywords 'words))
(defvar faustine-faust-function-regexp (regexp-opt faustine-faust-functions 'words))
(defvar faustine-faustine-faust-ui-keywords-regexp (regexp-opt faustine-faust-ui-keywords 'words))

(defvar faustine-mode-font-lock-keywords
  `((,faustine-faust-function-regexp . font-lock-type-face)
    (,faustine-faustine-faust-ui-keywords-regexp . font-lock-builtin-face)
    (,faustine-faust-math-op-regexp . font-lock-function-name-face)
    (,faustine-faust-operator-regexp . font-lock-constant-face)
    (,faustine-faustine-faust-keywords-regexp . font-lock-keyword-face)))

(define-derived-mode faustine-output-mode fundamental-mode
  "Faust output"
  (font-lock-fontify-buffer))

;;;###autoload
(define-derived-mode faustine-mode prog-mode "Faustine - A lightweight Emacs Faust IDE" "
Faustine is a lightweight IDE that leverages the mighty power of the faust executable.

Use \\[faustine-configure] to set it up.
Available commands while editing Faust (*.dsp) files:

\\{faustine-mode-map}"

  (kill-all-local-variables)

  (setq mode-name "Faust"
        major-mode 'faustine-mode
        comment-start "// "
        comment-end ""
        font-lock-defaults '(faustine-mode-font-lock-keywords))

  (add-hook 'after-save-hook 'faustine-syntax-check nil t)
  (add-hook 'find-file-hook 'faustine-syntax-check nil t)
  ;; (add-hook 'find-file-hook 'faustine-buttonize-buffer-lib nil t)
  ;; (add-hook 'find-file-hook 'faustine-buttonize-buffer-dsp nil t)

  (add-hook 'find-file-hook '(lambda ()
                               (faustine-buttonize-buffer "dsp")) nil t)

  (add-hook 'find-file-hook '(lambda ()
                               (faustine-buttonize-buffer "lib")) nil t)

  (set-syntax-table faustine-mode-syntax-table)

  (use-local-map faustine-mode-map)

  (define-key faustine-mode-map (kbd faustine-kb-configure) 'faustine-configure)
  (define-key faustine-mode-map (kbd faustine-kb-build) 'faustine-build)
  (define-key faustine-mode-map (kbd faustine-kb-build-all) '(lambda ()
                                                                 (interactive)
                                                                 (faustine-build t)))
  (define-key faustine-mode-map (kbd faustine-kb-diagram) 'faustine-diagram)
  (define-key faustine-mode-map (kbd faustine-kb-diagram-all) '(lambda ()
                                                                 (interactive)
                                                                 (faustine-diagram t)))
  (define-key faustine-mode-map (kbd faustine-kb-online-doc) 'faustine-online-doc)
  (define-key faustine-mode-map (kbd faustine-kb-toggle-output-buffer) 'faustine-toggle-output-buffer)
  (define-key faustine-mode-map (kbd faustine-kb-mdoc) 'faustine-mdoc)
  (define-key faustine-mode-map (kbd faustine-kb-run) 'faustine-run)
  (define-key faustine-mode-map (kbd faustine-kb-source-code) 'faustine-source-code)
  (define-key faustine-mode-map (kbd faustine-kb-syntax-check) 'faustine-syntax-check)

  (smie-setup nil #'ignore)

  (font-lock-add-keywords 'faustine-output-mode
                          '(("started" . font-lock-keyword-face)
                            ("finished" . font-lock-keyword-face)
                            ("Build" . font-lock-string-face)
                            ("\\b\\(Check\\)\\b" . font-lock-string-face)
                            ("Diagram" . font-lock-string-face)
                            ("Mdoc" . font-lock-string-face)
                            ("Run" . font-lock-string-face)
                            ("error" . font-lock-warning-face)
                            ("jack" . font-lock-warning-face)
                            ("exited abnormally with code" . font-lock-warning-face)))

  (setq ac-user-dictionary (append
                            faustine-faust-keywords
                            faustine-faust-functions
                            faustine-faust-ui-keywords))

  (add-to-list 'ac-modes 'faustine-mode)

  (run-hooks 'change-major-mode-after-body-hook 'after-change-major-mode-hook))

(defun faustine-configure ()
  "Use `cutomize-group' to set up Faustine preferences."
  (interactive)
  (customize-group 'faustine))

(define-button-type 'faustine-link-lib
  'follow-link t
  'action #'faustine-link-lib-action)

(define-button-type 'faustine-link-dsp
  'follow-link t
  'action #'faustine-link-dsp-action)

(define-button-type 'faustine-link-log
  'follow-link t
  'action #'faustine-link-log-action)

(define-button-type 'faustine-link-exe
  'follow-link t
  'action #'faustine-link-exe-action)

(defun faustine-link-lib-action (button)
  "Search Faust library file and insert BUTTON."
  (find-file (format "%s%s"
                     faustine-faust-libs-dir
                     (buffer-substring
                      (button-start button) (button-end button))))
  (faustine-mode)
  (faustine-buttonize-buffer "lib"))

(defun faustine-link-dsp-action (button)
  "Search Faust file and insert BUTTON."
  (find-file (format "%s%s"
                     (file-name-directory buffer-file-name)
                     (buffer-substring
                      (button-start button) (button-end button)))))

(defun faustine-link-log-action (button)
  "Search Faust output buffer and insert BUTTON."
  (let ((buffer (car (split-string
                      (buffer-substring-no-properties
                       (button-start button) (button-end button)) "\\:")))
        (line (cadr (split-string
                     (buffer-substring-no-properties
                      (button-start button) (button-end button)) "\\:"))))
    (find-file-other-window buffer)
    (goto-char (point-min))
    (forward-line (1- (string-to-number line)))))

(defun faustine-link-exe-action (button)
  "Search Faust file and insert BUTTON."
  ;; (message "Button: %s" (buffer-substring-no-properties
  ;;                        (button-start button) (button-end button)))
  (message "plop!")
  )

(defun faustine-buttonize-buffer (type)
  "Turn all file paths into buttons of type TYPE."
  (save-excursion
    (goto-char (point-min))
    (let ((regexp (cond ((string= type "dsp") faustine-regexp-dsp)
                        ((string= type "log") faustine-regexp-log)
                        ((string= type "exe") faustine-regexp-exe)
                        ((string= type "lib") faustine-regexp-lib)))
          (end (cond ((string= type "log") 2)
                     (t 1))))
      (while (re-search-forward regexp nil t nil)
        (message "Found: %s" (buffer-substring-no-properties
                              (match-beginning 1) (match-end 1)))
        (make-button (match-beginning 1) (match-end end)
                     :type (intern-soft (concat "faustine-link-" type)))
        ))))

(defun faustine-online-doc (start end)
  "Websearch selected (or just under point) string on the faust.grame.fr library web site.
Build a button from START to END."
  (interactive "r")
  (let ((selection (if (use-region-p)
                       (buffer-substring-no-properties start end)
                     (current-word))))
    (browse-url (concat "http://faust.grame.fr/library.html#"
                        (url-hexify-string selection)))))

(defun faustine-source-code ()
  "Generate Faust c++ code of the current faust file, display it in a buffer."
  (interactive)
  (let ((oldbuf (current-buffer)))
    (with-current-buffer (get-buffer-create faustine-c++-buffer-name)
      (pop-to-buffer faustine-c++-buffer-name nil t)
      (erase-buffer)
      (c++-mode)
      (call-process "/bin/bash" nil t nil "-c" (format "faust %s" oldbuf))
      (goto-char (point-min))
      (other-window -1)
      (pop-to-buffer oldbuf nil t))
    (if faustine-pop-output-buffer
        (faustine-open-output-buffer))))

(defun faustine-syntax-check ()
  "Check if Faust code buffer compiles."
  (interactive)
  (let ((output-check (shell-command-to-string (format "faust %s > /dev/null" (buffer-name)))))
    (if (string= "" output-check)
        (progn
          (faustine-log-to-buffer "Check" "finished")
          (faustine-red-mode 0)
          (faustine-green-mode t))
      (progn
        (faustine-log-to-buffer "Check" (format "%s" output-check))
        (faustine-green-mode 0)
        (faustine-red-mode t)))
    (if faustine-pop-output-buffer
        (faustine-open-output-buffer))))

(defun faustine-toggle-output-buffer ()
  "Show/hide Faust output buffer."
  (interactive)
  (if (get-buffer-window faustine-output-buffer-name `visible)
      (delete-window (get-buffer-window faustine-output-buffer-name `visible))
    (faustine-open-output-buffer)))

(defun faustine-open-output-buffer ()
  "Show Faust output buffer."
  (let ((oldbuf (current-buffer)))
    (with-current-buffer (get-buffer-create faustine-output-buffer-name)
      (display-buffer faustine-output-buffer-name)
      (if (> (+ 1 -16)
             (window-resizable
              (get-buffer-window faustine-output-buffer-name `visible) -16 nil))
          (window-resize (get-buffer-window faustine-output-buffer-name `visible) -16 nil)))))

(defun faustine-project-files (fname blist)
  "Recursively find all Faust links in FNAME, canonicalize and put them in BLIST, return BLIST."
  (add-to-list 'blist (expand-file-name fname))
  (with-temp-buffer
    (insert-file-contents-literally fname)
    (goto-char (point-min))
    (while (re-search-forward faustine-regexp-dsp nil t)
      (when (match-string 0)
        (let ((uri (expand-file-name (match-string 1))))
          (if (not (member uri blist))
              (setq blist (faustine-project-files uri blist))))))
    (identity blist)))

(defun faustine-mdoc (&optional build-all)
  "Generate mdoc of the current file, display it in a buffer.
If BUILD-ALL is set, generate all linked files."
  (interactive)
  (let* ((files-to-build (if build-all
                             (mapconcat 'identity (faustine-project-files (buffer-name) '()) " ")
                           (current-buffer)))
         (process (start-process-shell-command "Mdoc"
                                               faustine-output-buffer-name
                                               (format "faust2svg %s" files-to-build))))
    (with-current-buffer (process-buffer process)
      (setq faustine-process-source-buffer (current-buffer)))
    (set-process-sentinel process 'faustine-mdoc-sentinel)))

(defun faustine-build (&optional build-all)
  "Build the current buffer/file executable(s).
If BUILD-ALL is set, build all Faust files referenced by this one."
  (interactive)
  (faustine-log-to-buffer "Build" "started")
  (let* ((files-to-build (if build-all
                             (mapconcat 'identity (faustine-project-files (buffer-name) '()) " ")
                           (current-buffer)))
         (process (start-process-shell-command
                   "Build"
                   faustine-output-buffer-name
                   (format "%s %s" faustine-build-backend files-to-build))))
    (set-process-sentinel process 'faustine-log-to-buffer)))

(defun faustine-run ()
  "Run the executable generated by the current Faust code buffer."
  (interactive)
  (faustine-log-to-buffer "Run" "started")
  (let ((process (start-process-shell-command
                  "Run"
                  faustine-output-buffer-name
                  (format "./%s" (file-name-sans-extension
                                  (file-name-nondirectory
                                   (buffer-name)))))))
    (set-process-sentinel process 'faustine-log-to-buffer)
    (if faustine-pop-output-buffer
        (faustine-open-output-buffer))))

(defun faustine-mdoc-sentinel (process event)
  "Mdoc sentinel: Log PROCESS and EVENT to output buffer."
  (let ((pdf-file (format "%s-mdoc/pdf/%s.pdf"
                          (file-name-sans-extension
                           (buffer-name faustine-process-source-buffer))
                          (file-name-sans-extension
                           (buffer-name faustine-process-source-buffer)))))
    (faustine-log-to-buffer process event)
    (when (string-prefix-p "finished" event)
      (browse-url-of-file pdf-file))
    (if faustine-pop-output-buffer
        (faustine-open-output-buffer))))

(defun faustine-log-to-buffer (process event)
  "Log PROCESS and EVENT to output buffer."
  (let ((buffer-name (buffer-name)))
    (with-current-buffer (get-buffer-create faustine-output-buffer-name)
      (faustine-output-mode)
      (font-lock-fontify-buffer)
      (goto-char (point-max))
      (insert (format "%s | %s %s %s\n"
                      (format-time-string "%H:%M:%S")
                      buffer-name
                      process
                      event))
      (faustine-buttonize-buffer "log")
      (faustine-buttonize-buffer "exe")
      (when (get-buffer-window faustine-output-buffer-name `visible)
        (with-selected-window (get-buffer-window (current-buffer))
          (goto-char (point-max)))))))

(defun faustine-diagram (&optional build-all)
  "Generate Faust diagram(s).
If BUILD-ALL is set, build all `faustine-faust-extension` files referenced by this one."
  (interactive)
  (faustine-log-to-buffer "Diagram" "started")
  (let ((mylist nil)
        (files-to-build
         (if build-all (faustine-project-files (buffer-name) '()) (list (buffer-name))))
        (display-mode
         (if build-all "all" "single")))
    (let ((command-output (shell-command-to-string (format "faust2svg %s" (mapconcat 'identity files-to-build " ")))))
      (if (string= "" command-output)
        (progn
          (faustine-log-to-buffer "Diagram" "finished")
          (faustine-build-html-file files-to-build (buffer-name) display-mode)
          (browse-url-of-file faustine-diagram-page-name))
        (faustine-log-to-buffer "Diagram" (format "Error: %s" command-output))))))

(defun faustine-build-html-file (list diagram display-mode)
  "Build a minimal HTML (web) page to display Faust diagram(s).
LIST is the list of files to display, DIAGRAM is the current file, and DISPLAY-MODE is the mode."
  (when (file-regular-p faustine-diagram-page-name)
      (delete-file faustine-diagram-page-name))
  (let ((flex-value (if (equal display-mode "all")
                        ""
                      "100%")))
    (write-region (format "<!DOCTYPE html>
<html>
</head>
<style>

html {
    background-color: #ddd;
    font-family: sans-serif;
}

a:link {
    color: #ddd;
}
a:visited {
    color: #aaa;
}
a:hover {
    color: #F44800;
}
a:active {
    color: #fff;
}
h1 {
    font-size: 80%%;
    margin: 0 0 0 0;
}
figcaption span {
    float:right;
}
div.wrap {
    display:flex;
/*    flex-flow: row wrap; */
    flex-wrap: wrap;
    justify-content: space-around;
}
div.item {
    color: #eee;
    float: right;
    width: 30%%;
    height:100%%;
    background-color: rgba(10,10,10,0.8);
    border: thin dimgrey solid;
    margin: 0.2em;
    padding: 0.1em;
    order:2;
    flex: %s;
}
div.focus {
    color: #F44800;
    background-color: rgba(50,50,50,0.9);
    order:1;
}
div.focus img {
/*    outline: 2px #F44800 solid; */
}
img.scaled {
    width: 100%%;
}
</style>
<title>Faust diagram</title>
</head>
<body>
<h1>Rendered on %s</h1>
<div class='wrap'>\n" flex-value (format-time-string "%A %B %d, %H:%M:%S")) nil faustine-diagram-page-name)
    (while list
      (if (file-regular-p (car list))
          (let* ((dsp-element (file-name-sans-extension (car list)))
                 (i 0)
                 (dsp-file-name (car list))
                 (class (if (equal diagram (file-name-nondirectory (car list)))
                            "focus"
                          "normal"))
                 (order (if (equal diagram (car list))
                            0
                          (+ 1 i)))
                 (dsp-dir (file-name-directory buffer-file-name))
                 (svg-dir (format "%s%s-svg/" dsp-dir (file-name-nondirectory dsp-element)))
                 (svg-file (concat svg-dir "process.svg")))
            (write-region
             (format "
<div class='item %s'>
  <a href='%s'><img class='scaled %s' src='%s' alt='%s' title='Click to view %s'></a>
  <figcaption>%s<span><a href='%s' title='All diagrams in %s'>%s</a></span></figcaption>
</div>
\n"
                     class
                     svg-file
                     class
                     svg-file
                     svg-file
                     svg-file
                     (file-name-nondirectory dsp-file-name)
                     svg-dir
                     svg-dir
                     (file-name-nondirectory (directory-file-name svg-dir))
                     ) nil faustine-diagram-page-name 'append 0 nil nil)))
      (setq list (cdr list)))
    (write-region "</div>
</body>
</html>\n" nil faustine-diagram-page-name 'append 0 nil nil)))

;;;###autoload
(add-to-list 'auto-mode-alist
               '("\\.dsp\\'" . faustine-mode))


(provide 'faustine)

;;; faustine.el ends here
