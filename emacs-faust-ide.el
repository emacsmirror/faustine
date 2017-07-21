;; emacs-faust-ide.el --- a Faust code editor for Emacs
;; Package-Requires: ((emacs "24"))
;;
;; FAUST (Functional Audio Stream) is a functional programming language
;; specifically designed for real-time signal processing and synthesis.
;; FAUST targets high-performance signal processing applications and audio plug-ins
;; for a variety of platforms and standards.
;; http://faust.grame.fr "plop.lib"
;;
;; Copyright (C) 2017, 2018 Yassin Philip
;; URL: https://bitbucket.org/yassinphilip/emacs-faust-ide
;;
;; Author: Yassin Philip <xaccrocheur@gmail.com>
;; Keywords: faust
;; Version 0.5b
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
;; Commentary:
;; See README.md for details.
;;
;; Code:


(require 'smie)
(require 'easymenu)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dsp\\'" . emacs-faust-ide-mode))

(defvar emacs-faust-ide-module-path (file-name-directory load-file-name))

(defvar emacs-faust-ide-minor-mode-green-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'test-mouse)
    map)
  "Keymap for `emacs-faust-ide-minor-mode-green'.")

(defvar emacs-faust-ide-minor-mode-red-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'test-mouse)
    map)
  "Keymap for `emacs-faust-ide-minor-mode-red'.")

(define-button-type 'emacs-faust-ide-link-lib
  'follow-link t
  'action #'emacs-faust-ide-link-lib)

(define-button-type 'emacs-faust-ide-link-dsp
  'follow-link t
  'action #'emacs-faust-ide-link-dsp)

(setq emacs-faust-ide-regexp-lib "\\(\\\\[\\\\\"]\\|[^\\\\\"]\\)*.lib"
      emacs-faust-ide-regexp-dsp "\\(\\\\[\\\\\"]\\|[^\\\\\"]\\)*.dsp")

(easy-menu-define emacs-faust-ide-minor-mode-green-menu
  emacs-faust-ide-minor-mode-green-map
  "Green bug menu"
  '("Syntax check: OK"
    "-"))

(easy-menu-define
  my-mode-mapemacs-faust-ide-minor-mode-red-menu
  emacs-faust-ide-minor-mode-red-map
  "My own menu"
  '("My Stuff"
    ["One entry" my-function t]
    ("Sub Menu"
     ["My subentry" my-obscure-function t])))

(easy-menu-define jrk-menu global-map "MyMenu"
  '("My Files"))

;; (define-button-type 'help-xref
;;   'follow-link t
;;   'action #'help-button-action)

(defvar emacs-faust-ide-module-path (file-name-directory load-file-name))

(defvar display-mode)
(defvar temp-file-name)
(defvar emacs-faust-ide-directory-files)
(defvar output-visible)
(defvar dsp-buffer)
(defvar emacs-faust-ide-output-openp)
(defvar dsp-buffer-name)
(defvar files-to-build)
(defvar output-check)

(defvar emacs-faust-ide-minor-mode-green-bug
  (list
   " "
   (propertize
    "Syntax: OK"
    :mouse-action 'test-mouse
    'font-lock-face '(:foreground "forest green")
    'help-echo "Test mode help message."
    'display
    `(image :type xpm
            :ascent center
            :file ,(expand-file-name "greenbug.xpm" emacs-faust-ide-module-path)))))

(defvar emacs-faust-ide-minor-mode-red-bug
  (list
   " "
   (propertize
    "Syntax: ERROR"
    ;; :keymap (purecopy (make-mode-line-mouse-map
    ;;                       'mouse-1
    ;;                       #'test-mouse))
    :help-echo "Tst string"
    :follow-link 'test-mouse
    'display
    `(image :type xpm
            :ascent center
            :file ,(expand-file-name "redbug.xpm" emacs-faust-ide-module-path)))))

(put 'emacs-faust-ide-minor-mode-green-bug 'risky-local-variable t)
(put 'emacs-faust-ide-minor-mode-red-bug 'risky-local-variable t)

(add-hook 'emacs-faust-ide-mode-hook
          (lambda ()
            (setq ac-sources '(ac-source-words-in-buffer
                               ac-source-symbols
                               ac-source-abbrev
                               ac-source-dictionary
                               ac-source-features
                               ac-source-filename
                               ac-source-files-in-current-dir
                               ac-source-functions
                               ac-source-symbols
                               ac-source-variables
                               ac-source-words-in-all-buffer
                               ac-source-words-in-buffer
                               ac-source-words-in-same-mode-buffers))))

(define-minor-mode emacs-faust-ide-minor-mode-green
  "Minor mode to display a green bug in the mode-line."
  :lighter emacs-faust-ide-minor-mode-green-bug
  :keymap emacs-faust-ide-minor-mode-green-map)

(define-minor-mode emacs-faust-ide-minor-mode-red
  "Minor mode to display a red bug in the mode-line."
  :lighter emacs-faust-ide-minor-mode-red-bug
  :keymap emacs-faust-ide-minor-mode-red-map)

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
Customize `emacs-faust-ide-build-options' for a lucky build"
  :group 'applications)

(defcustom emacs-faust-ide-build-target 'faust2jaqt
  "The Faust code-to-executable build backend."
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

(defcustom emacs-faust-ide-build-options "plop"
  "The type of build."
  :type '(string)
  :group 'emacs-faust-ide)

(defcustom emacs-faust-ide-output-buffer "Faust output"
  "Buffer."
  :type '(string)
  :group 'emacs-faust-ide)

(defcustom emacs-faust-ide-faust-libs-dir "/usr/local/share/faust/"
  "The Faust library directory for direct linking."
  :type '(string)
  :group 'emacs-faust-ide)

(defvar emacs-faust-ide-mode-map
   (let ((map (make-sparse-keymap)))
     (define-key map [?\C-c ?\C-b] 'emacs-faust-ide-build)
     (define-key map [?\C-c ?\C-\S-b] 'emacs-faust-ide-build-all)
     (define-key map [?\C-c ?\C-d] 'emacs-faust-ide-diagram)
     (define-key map [?\C-c ?\C-\S-d] 'emacs-faust-ide-diagram-all)
     (define-key map [?\C-c ?\C-h] 'emacs-faust-ide-online-doc)
     (define-key map [?\C-c ?\C-m] 'emacs-faust-ide-mdoc)
     (define-key map [?\C-c ?\C-r] 'emacs-faust-ide-run)
     (define-key map [?\C-c ?\C-s] 'emacs-faust-ide-source-code)
     (define-key map [?\C-c ?\C-c] 'emacs-faust-ide-syntax-check)

     (define-key map (vector 'mode-line 'mouse-1)
       `(lambda (e)
          (interactive "e")
          (switch-to-buffer emacs-faust-ide-output-buffer)))

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

(defvar emacs-faust-ide-mode-font-lock-keywords
  `((,faust-function-regexp . font-lock-type-face)
    (,faust-ui-keywords-regexp . font-lock-builtin-face)
    (,faust-math-op-regexp . font-lock-function-name-face)
    (,faust-operator-regexp . font-lock-constant-face)
    (,faust-keywords-regexp . font-lock-keyword-face)))

(define-derived-mode emacs-faust-ide-output-mode fundamental-mode
  "Emacs Faust IDE output buffer mode."
  (font-lock-fontify-buffer))

;;;###autoload
(define-derived-mode emacs-faust-ide-mode fundamental-mode "Emacs Faust IDE Mode" "
Emacs Faust IDE is a lightweight IDE that leverages the mighty power of the faust executable.

Use \\[emacs-faust-ide-set-preferences] to set it up.
Available commands while editing Faust (*.dsp) files:

\\{emacs-faust-ide-mode-map}"
  (kill-all-local-variables)
  (add-hook 'after-save-hook 'emacs-faust-ide-syntax-check-continuous-hook nil t)
  (add-hook 'find-file-hook 'emacs-faust-ide-syntax-check-continuous-hook nil t)

  (add-hook 'find-file-hook 'emacs-faust-ide-buttonize-buffer-lib nil t)
  (add-hook 'find-file-hook 'emacs-faust-ide-buttonize-buffer-dsp nil t)

  ;; (add-hook 'emacs-faust-ide-mode-hook 'auto-complete nil t)
  (setq mode-name "emacs-faust-ide-mode")
  (set-syntax-table emacs-faust-ide-mode-syntax-table)
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local font-lock-defaults
              '(emacs-faust-ide-mode-font-lock-keywords))
  (smie-setup nil #'ignore)

  (use-local-map emacs-faust-ide-mode-map)

  (font-lock-add-keywords 'emacs-faust-ide-output-mode
                          '(("Process Build finished" . font-lock-keyword-face)
                            ("Process Build started" . font-lock-keyword-face)
                            ("Process Diagram started" . font-lock-keyword-face)
                            ("ERROR" . font-lock-warning-face)))

  (font-lock-add-keywords 'c++-mode
                          '(("Process Build finished" . font-lock-keyword-face)
                            ("Process Build started" . font-lock-keyword-face)
                            ("Process Diagram started" . font-lock-keyword-face)
                            ("ERROR" . font-lock-warning-face)))

  (auto-complete-mode t)
  (setq ac-user-dictionary (append faust-keywords faust-functions faust-ui-keywords))
  (setq ac-auto-show-menu t)
  (setq ac-auto-start t)

  (setq major-mode 'emacs-faust-ide-mode)
  (message "########### MODE OK & emacs-faust-ide-build-target : %s" emacs-faust-ide-build-target)
  (run-hooks 'change-major-mode-after-body-hook 'after-change-major-mode-hook)
  )

;; Functions

(defun emacs-faust-ide-set-preferences ()
  "Use `cutomize-group' to set up Emacs Faust IDE preferences "
  (interactive)
  (customize-group 'emacs-faust-ide))

(defun emacs-faust-ide-syntax-check-continuous-hook ()
  "Used in `after-save-hook'."
    (emacs-faust-ide-syntax-check))

(defun emacs-faust-ide-link-lib (button)
  "Open library file"
  (find-file (format "%s%s"
                     emacs-faust-ide-faust-libs-dir
                     (buffer-substring
                      (button-start button) (button-end button)))))

(defun emacs-faust-ide-link-dsp (button)
  "Open Faust file"
  (message "####### XXX: %s" (file-name-directory buffer-file-name))
  (find-file (format "%s%s"
                     (file-name-directory buffer-file-name)
                     (buffer-substring
                      (button-start button) (button-end button)))))

(defun emacs-faust-ide-buttonize-buffer-dsp ()
  "turn all file paths into buttons"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward emacs-faust-ide-regexp-dsp nil t)
      (make-button (match-beginning 0) (match-end 0) :type 'emacs-faust-ide-link-dsp))))

(defun emacs-faust-ide-buttonize-buffer-lib ()
  "turn all file paths into buttons"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward emacs-faust-ide-regexp-lib nil t)
      (make-button (match-beginning 0) (match-end 0) :type 'emacs-faust-ide-link-lib))))

(defun test-mouse ()
  (interactive)
  (message "plop"))

(defun emacs-faust-ide-online-doc (start end)
  "Websearch selected string on the faust.grame.fr library web site."
  (interactive "r")
  (let ((q (buffer-substring-no-properties start end)))
    (browse-url (concat "http://faust.grame.fr/library.html#"
                        (url-hexify-string q)))))

(defun emacs-faust-ide-build-all ()
  "Build all executables using `emacs-faust-ide-build'"
  (interactive)
  (emacs-faust-ide-build 1))

(defun emacs-faust-ide-diagram-all ()
  "Build all executables using `emacs-faust-ide-build'"
  (interactive)
  (emacs-faust-ide-diagram 1))

(defun emacs-faust-ide-build (&optional build-all)
  "Build the executable(s) using the `emacs-faust-ide-build-target' executable. If BUILD-ALL is set, build all .dsp files in the current directory."
  (interactive)
  (setq dsp-buffer (current-buffer))
  (with-current-buffer (get-buffer-create emacs-faust-ide-output-buffer)
    (pop-to-buffer emacs-faust-ide-output-buffer nil t)
    (emacs-faust-ide-output-mode)
    (goto-char (point-max))
    (insert "Process Build started\n")
    (if build-all
        (progn (setq-local files-to-build "*.dsp")
               (message "Building ALL"))
      (progn (message "Building just %s" dsp-buffer)
             (setq files-to-build dsp-buffer)))
    (start-process-shell-command
     "Build"
     (current-buffer)
     (format "%s %s" emacs-faust-ide-build-target files-to-build))
    (other-window -1)
    (pop-to-buffer dsp-buffer nil t)))

(defun emacs-faust-ide-source-code ()
  "Generate Faust c++ code of the current faust file, display it in a buffer."
  (interactive)
  (setq dsp-buffer (current-buffer))
  (with-current-buffer (get-buffer-create "Faust c++")
    (pop-to-buffer "Faust c++" nil t)
    (erase-buffer)
    (c++-mode)
    (call-process "/bin/bash" nil t nil "-c" (format "faust %s" dsp-buffer))
    (goto-char (point-min))
    (other-window -1)
    (pop-to-buffer dsp-buffer nil t)))

(defun emacs-faust-ide-syntax-check ()
  "Check if Faust code buffer compiles."
  (interactive)
  (setq dsp-buffer (buffer-name))
  (setq-local output-check (shell-command-to-string (format "faust %s > /dev/null" dsp-buffer)))
  ;; (message "Output: (%s)" output-check)
  (if (string= "" output-check)
      (progn
        (emacs-faust-ide-minor-mode-red 0)
        (emacs-faust-ide-minor-mode-green t))
    (progn
      (emacs-faust-ide-minor-mode-green 0)
      (emacs-faust-ide-minor-mode-red t))))

(defun emacs-faust-ide-run ()
  "Run the executable generated by the current Faust code buffer."
  (interactive)
  (setq dsp-buffer (current-buffer))
  (setq dsp-buffer-name (buffer-name))
  (with-current-buffer (get-buffer-create emacs-faust-ide-output-buffer)
    (pop-to-buffer emacs-faust-ide-output-buffer nil t)
    ;; (previous-window)
    (goto-char (point-max))
    (start-process-shell-command "Run" (current-buffer)
                                 (format "./%s" (file-name-sans-extension
                                                 (file-name-nondirectory
                                                  dsp-buffer-name))))
    (other-window -1)
    (pop-to-buffer dsp-buffer nil t)))

(defun emacs-faust-ide-show (file)
  "Show FILE in a web page using default browser."
  (interactive)
  (browse-url-of-file file))

(defun emacs-faust-ide-mdoc ()
  "Generate Faust mdoc of the current faust file, display it in a buffer."
  (interactive)
  (setq dsp-buffer (current-buffer))
  (setq dsp-buffer-name (buffer-name))
  (with-current-buffer (get-buffer-create emacs-faust-ide-output-buffer)
    (pop-to-buffer emacs-faust-ide-output-buffer nil t)
    (emacs-faust-ide-output-mode)
    (goto-char (point-max))
    (insert (format "Process Mdoc started"))
    (call-process "/bin/bash" nil t nil "-c" (format "faust2mathdoc %s" dsp-buffer-name))
    (other-window -1)
    (pop-to-buffer dsp-buffer nil t))
  (setq temp-file-name (format "%s-mdoc/pdf/%s.pdf"
                               (file-name-sans-extension
                                 dsp-buffer-name)
                               (file-name-sans-extension
                                 dsp-buffer-name)))
  (emacs-faust-ide-show temp-file-name))

(defun run-sentinel (process event)
  "Run the program"

  (let ((oldbuf (current-buffer)))
    (with-current-buffer (get-buffer-create "Out")
      (insert (format "Process: %s Event: %s\n" process event)))
    ))

(defun test-sentinel ()
  "plop"
  (interactive)
  (set-process-sentinel
   (start-process-shell-command "Build" nil "ls ~/tmp/") 'run-sentinel))

(defun emacs-faust-ide-diagram (&optional build-all)
  "Generate Faust diagram(s)."
  (interactive)
  (setq dsp-buffer (current-buffer))
  (setq dsp-buffer-name (buffer-name))

  ;; (if (get-buffer emacs-faust-ide-output-buffer)
  ;;     (message "exists")
  ;;   (message "don't exist"))

  (cond ((eq emacs-faust-ide-output-buffer (window-buffer (selected-window)))
         (progn
           (setq output-visible t)
           (message "Visible and focused")))
        ((get-buffer-window emacs-faust-ide-output-buffer `visible)
         (progn
           (setq output-visible t)
           (message "Visible and unfocused")))
        (t
         (progn
           (message "not visible")
           (setq output-visible nil))))

  (if (string-match-p (regexp-quote emacs-faust-ide-output-buffer) (format "%s" (buffer-list)))
      (progn (message "YES")
           (setq emacs-faust-ide-output-openp t))
    (progn (message "NO")
           (setq emacs-faust-ide-output-openp nil)))

  (with-current-buffer (get-buffer-create emacs-faust-ide-output-buffer)

    (if (not emacs-faust-ide-output-openp)
        (progn
          (pop-to-buffer emacs-faust-ide-output-buffer nil t)
          (emacs-faust-ide-output-mode)
          (enlarge-window -25)))

    (if build-all
        (progn
          (message "Building ALL")
          (setq display-mode "all"))
          ;; (setq files-to-build "*.dsp")
      (progn
        (message "Building just %s" dsp-buffer)
        ;; (setq display-mode "single")
        (setq display-mode "single")))

    (let* ((files-to-build (if build-all
                               "*.dsp"
                             dsp-buffer)))
      (message "Files: %s" files-to-build)
      (set-process-sentinel
       (start-process-shell-command "Build" nil (format "faust2svg %s" files-to-build)) 'run-sentinel)

      ;; (call-process "/bin/bash" nil emacs-faust-ide-output-buffer t "-c" (format "faust2svg %s" files-to-build))
      (insert (format "Process Diagram started : Building %s\n" files-to-build)))

    (setq other-window-scroll-buffer emacs-faust-ide-output-buffer)

    ;; (other-window -1)
    (pop-to-buffer dsp-buffer nil t)
    )
  (if output-visible
      (scroll-other-window))

  (setq temp-file-name "faust-graphs.html")
  (setq emacs-faust-ide-directory-files (directory-files (file-name-directory buffer-file-name) nil "^[a-z0-9A-Z]?+\\.dsp$"))
  (emacs-faust-ide-build-temp-file emacs-faust-ide-directory-files temp-file-name dsp-buffer-name display-mode)
  (emacs-faust-ide-show temp-file-name))



(defun emacs-faust-ide-build-temp-file (list temp-file-name diagram display-mode)
  "Build a minimal HTML (web) page to display Faust diagram(s)."
  (if (file-regular-p temp-file-name)
      (delete-file temp-file-name))

  (let*
      ((flex-value (if (equal display-mode "all") "3 1" "100%")))
    (write-region (format "<html>
</head>
<style>
html {
  background-color: #ccc;
  font-family: sans-serif;
}
div.wrap {
  display:flex;
  flex-flow: row wrap;
}
div.item {
  float: right;
  width: 30%%;
  border: thin silver solid;
  margin: 0.2em;
  padding: 0.1em;
  order:2;
/*  flex: 3 1; */
  flex: %s;
}
div.focus {
  border: thick red solid;
  order:1;
}
img.scaled {
  width: 100%%;
}
</style>
<title>Faust diagram</title>
</head>
<body>
<div class='wrap'><h4>Render %s</h4>\n" flex-value (current-time-string)) nil temp-file-name)
    (while list
      (if (file-regular-p (car list))
          (let* ((dsp-element (file-name-sans-extension (car list)))
                 (i 0)
                 (dsp-file-name (car list))
                 (class (if (equal diagram (car list))
                            "focus"
                          "normal"))
                 (order (if (equal diagram (car list))
                            0
                          (+ 1 i)))
                 (dsp-dir (file-name-directory buffer-file-name)))
            (write-region
             (format "
<div class='item %s'>
  <a href='%s%s-svg/process.svg'>
<img class=scaled src='%s%s-svg/process.svg'
    alt='%s'></a>
  <figcaption>%s</figcaption>
</div>
\n"
                     class
                     dsp-dir
                     dsp-element
                     dsp-dir
                     dsp-element
                     dsp-file-name
                     dsp-file-name) nil temp-file-name 'append 0 nil nil)))
      (setq list (cdr list)))
    (write-region "</div>
</body>
</html>\n" nil temp-file-name 'append 0 nil nil)))

(provide 'emacs-faust-ide)
