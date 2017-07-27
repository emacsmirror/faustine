;; faustine.el --- a Faust code editor for Emacs
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

(defvar display-mode)
(defvar temp-file-name)
(defvar output-visible)
(defvar dsp-buffer)
(defvar faustine-output-openp)
(defvar dsp-buffer-name)
(defvar files-to-build)
(defvar output-check)
(defvar command-output)
(defvar faustine-regexp-dsp)
(defvar faustine-regexp-lib)

(defvar ac-modes)
(defvar ac-user-dictionary)
(defvar ac-auto-show-menu)
(defvar ac-auto-start)
(defvar ac-sources)

(defgroup faustine nil
  "Faustine - A lightweight Emacs Faust IDE.
Customize `build-backend' for a lucky build."
  :group 'tools)

(defgroup keyboard-shortcuts nil
  "Faustine keyboard shortcuts"
  :group 'faustine)

(defcustom build "C-c C-b"
  "Build the current buffer/file executable using the `build-backend' script. "
  :type '(string)
  :group 'keyboard-shortcuts)

(defcustom build-all "C-c C-S-b"
  "Build all project files."
  :type '(string)
  :group 'keyboard-shortcuts)

(defcustom diagram "C-c C-d"
  "Generate the current buffer/file Faust diagram."
  :type '(string)
  :group 'keyboard-shortcuts)

(defcustom diagram-all "C-c C-S-d"
  "Generate all project files Faust diagrams."
  :type '(string)
  :group 'keyboard-shortcuts)

(defcustom online-doc "C-c C-h"
  "Websearch the selected string on the faust.grame.fr library web site."
  :type '(string)
  :group 'keyboard-shortcuts)

(defcustom toggle-output-buffer "C-c C-o"
  "Show/hide Faust output buffer"
  :type '(string)
  :group 'keyboard-shortcuts)

(defcustom mdoc "C-c C-m"
  "Generate Faust mdoc of the current faust buffer/file."
  :type '(string)
  :group 'keyboard-shortcuts)

(defcustom run "C-c C-r"
  "Run the current buffer/file executable."
  :type '(string)
  :group 'keyboard-shortcuts)

(defcustom source-code "C-c C-s"
  "Generate C++ source code of the current faust buffer/file."
  :type '(string)
  :group 'keyboard-shortcuts)

(defcustom output-buffer-name "*Faust*"
  "The name of the Faust output Buffer. Surround it with \"*\" to hide it in special buffers."
  :type '(string)
  :group 'faustine)

(defcustom diagram-page-name "faust-graphs.html"
  "The name of the Faust diagrams HTML page."
  :type '(string)
  :group 'faustine)

(defcustom faust-libs-dir "/usr/local/share/faust/"
  "The Faust library directory for direct linking."
  :type '(string)
  :group 'faustine)

(defcustom faust-extension "dsp"
  "The Faust files extension."
  :type '(string)
  :group 'faustine)

(defcustom build-backend 'faust2jaqt
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

;;;###autoload
(add-to-list 'auto-mode-alist (cons (concat "\\." faust-extension "$") 'faustine-mode))

(defvar faustine-module-path (file-name-directory load-file-name))

(defvar faustine-minor-mode-green-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `faustine-minor-mode-green'.")

(defvar faustine-minor-mode-red-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `faustine-minor-mode-red'.")

(define-button-type 'faustine-link-lib
  'follow-link t
  'action #'faustine-link-lib)

(define-button-type 'faustine-link-dsp
  'follow-link t
  'action #'faustine-link-dsp)

(setq
 faustine-regexp-lib "\\\"\\([^\\\"\\\\(]+\\.lib\\)\\\""
 faustine-regexp-dsp (concat "\"\\([^\"\\(]+\\.\\(" faust-extension "\\)\\)\""))

(easy-menu-define
  faustine-minor-mode-green-menu
  faustine-minor-mode-green-map
  "Green bug menu"
  '("Faust build: OK"
    ["Faust output buffer" faustine-toggle-output-buffer t]
    ["Generate C++ code" faustine-source-code t]
    ["Generate diagram" faustine-diagram t]
    ["Build executable" faustine-build t]
    ["Run executable" faustine-run t]
    ("Project"
    ["Generate all diagrams" faustine-diagram-all t]
    ["Build all executables" faustine-build-all t])))

(easy-menu-define
  my-mode-mapfaustine-minor-mode-red-menu
  faustine-minor-mode-red-map
  "Red bug menu"
  '("Faust build: Error"
    ["Faust output buffer" faustine-toggle-output-buffer t]
    ("Sub Menu"
     ["My subentry" my-obscure-function t])))

(easy-menu-define jrk-menu global-map "MyMenu"
  '("My Files"))

(defvar faustine-minor-mode-green-bug
  (list
   " "
   (propertize
    "Syntax: OK"
    'display
    `(image :type xpm
            :ascent center
            :file ,(expand-file-name "greenbug.xpm" faustine-module-path)))))

(defvar faustine-minor-mode-red-bug
  (list
   " "
   (propertize
    "Syntax: ERROR"
    :help-echo "Tst string"
    :follow-link 'test-mouse
    'display
    `(image :type xpm
            :ascent center
            :file ,(expand-file-name "redbug.xpm" faustine-module-path)))))

(put 'faustine-minor-mode-green-bug 'risky-local-variable t)
(put 'faustine-minor-mode-red-bug 'risky-local-variable t)

(add-hook 'faustine-mode-hook
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

(define-minor-mode faustine-minor-mode-green
  "Minor mode to display a green bug in the mode-line."
  :lighter faustine-minor-mode-green-bug
  :keymap faustine-minor-mode-green-map)

(define-minor-mode faustine-minor-mode-red
  "Minor mode to display a red bug in the mode-line."
  :lighter faustine-minor-mode-red-bug
  :keymap faustine-minor-mode-red-map)

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


;; (defcustom build "C-c C-b"
;;   :type '(string)
;;   :group 'keyboard-shortcuts)

;; (defcustom build-all "C-c C-S-b"
;;   :type '(string)
;;   :group 'keyboard-shortcuts)

;; (defcustom diagram "C-c C-S-d"
;;   :type '(string)
;;   :group 'keyboard-shortcuts)

;; (defcustom diagram-all "C-c C-S-d"
;;   :type '(string)
;;   :group 'keyboard-shortcuts)

;; (defcustom online-doc "C-c C-h"
;;   :type '(string)
;;   :group 'keyboard-shortcuts)

;; (defcustom toggle-output-buffer "C-c C-o"
;;   :type '(string)
;;   :group 'keyboard-shortcuts)

;; (defcustom mdoc "C-c C-m"
;;   :type '(string)
;;   :group 'keyboard-shortcuts)

;; (defcustom run "C-c C-r"
;;   :type '(string)
;;   :group 'keyboard-shortcuts)

;; (defcustom source-code "C-c C-s"
;;   :type '(string)
;;   :group 'keyboard-shortcuts)

(defvar faustine-mode-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd build) 'faustine-build)
     (define-key map (kbd build-all) 'faustine-build-all)
     (define-key map (kbd diagram) 'faustine-diagram)
     (define-key map (kbd diagram-all) 'faustine-diagram-all)
     (define-key map (kbd online-doc) 'faustine-online-doc)
     (define-key map (kbd toggle-output-buffer) 'faustine-toggle-output-buffer)
     (define-key map (kbd mdoc) 'faustine-mdoc)
     (define-key map (kbd run) 'faustine-run)
     (define-key map (kbd source-code) 'faustine-source-code)
     (define-key map [?\C-c ?\C-c] 'faustine-syntax-check)
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

(defvar faust-variables-regexp "[A-Za-z][A-Za-z]*")
(defvar faust-arguments-regexp "[0-9]")
(defvar faust-operator-regexp "\\([~!_@,<>:;]\\)")
(defvar faust-math-op-regexp "[=\+\{\}()/*-]")
(defvar faust-keywords-regexp (regexp-opt faust-keywords 'words))
(defvar faust-function-regexp (regexp-opt faust-functions 'words))
(defvar faust-ui-keywords-regexp (regexp-opt faust-ui-keywords 'words))

(defvar faustine-mode-font-lock-keywords
  `((,faust-function-regexp . font-lock-type-face)
    (,faust-ui-keywords-regexp . font-lock-builtin-face)
    (,faust-math-op-regexp . font-lock-function-name-face)
    (,faust-operator-regexp . font-lock-constant-face)
    (,faust-keywords-regexp . font-lock-keyword-face)))

(define-derived-mode faustine-output-mode fundamental-mode
  "Emacs Faust IDE output buffer mode."
  (font-lock-fontify-buffer))

;;;###autoload
(define-derived-mode faustine-mode fundamental-mode "Emacs Faust IDE Mode" "
Faustine is a lightweight IDE that leverages the mighty power of the faust executable.

Use \\[faustine-set-preferences] to set it up.
Available commands while editing Faust (*.dsp) files:

\\{faustine-mode-map}"

  (kill-all-local-variables)
  (add-hook 'after-save-hook 'faustine-syntax-check nil t)
  (add-hook 'find-file-hook 'faustine-syntax-check nil t)
  (add-hook 'find-file-hook 'faustine-buttonize-buffer-lib nil t)
  (add-hook 'find-file-hook 'faustine-buttonize-buffer-dsp nil t)
  (setq mode-name "Faust")
  (set-syntax-table faustine-mode-syntax-table)
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local font-lock-defaults
              '(faustine-mode-font-lock-keywords))
  (smie-setup nil #'ignore)

  (use-local-map faustine-mode-map)

  (font-lock-add-keywords 'faustine-output-mode
                          '(("finished" . font-lock-keyword-face)
                            ("started" . font-lock-keyword-face)
                            ("Build" . font-lock-string-face)
                            ("Diagram" . font-lock-string-face)
                            ("ERROR" . font-lock-warning-face)
                            ("exited abnormally with code" . font-lock-warning-face)))

  (font-lock-add-keywords 'c++-mode
                          '(("Process Build finished" . font-lock-keyword-face)
                            ("Process Build started" . font-lock-keyword-face)
                            ("Process Diagram started" . font-lock-keyword-face)
                            ("ERROR" . font-lock-warning-face)))

  ;; (auto-complete-mode 1)
  (add-to-list 'ac-modes 'faustine-mode)
  (setq ac-user-dictionary (append faust-keywords faust-functions faust-ui-keywords))
  (setq ac-auto-show-menu t)
  (setq ac-auto-start t)
  (setq major-mode 'faustine-mode)
  (message "########### MODE OK & build-backend : %s" build-backend)
  (run-hooks 'change-major-mode-after-body-hook 'after-change-major-mode-hook))

;; Functions

(defun faustine-set-preferences ()
  "Use `cutomize-group' to set up Emacs Faust IDE preferences "
  (interactive)
  (customize-group 'faustine))

(defun faustine-link-lib (button)
  "Open library file"
  (find-file (format "%s%s"
                     faust-libs-dir
                     (buffer-substring
                      (button-start button) (button-end button))))
  (faustine-mode)
  (faustine-buttonize-buffer-lib))

(defun faustine-link-dsp (button)
  "Open Faust file"
  (message "####### XXX: %s" (file-name-directory buffer-file-name))
  (find-file (format "%s%s"
                     (file-name-directory buffer-file-name)
                     (buffer-substring
                      (button-start button) (button-end button)))))

(defun faustine-buttonize-buffer-dsp ()
  "turn all file paths into buttons"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward faustine-regexp-dsp nil t)
      (make-button (match-beginning 1) (match-end 1) :type 'faustine-link-dsp))))

(defun faustine-buttonize-buffer-lib ()
  "turn all file paths into buttons"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward faustine-regexp-lib nil t)
      (make-button (match-beginning 1) (match-end 1) :type 'faustine-link-lib))))

(defun test-mouse ()
  (interactive)
  (message "plop"))

(defun faustine-online-doc (start end)
  "Websearch selected string on the faust.grame.fr library web site."
  (interactive "r")
  (let ((q (buffer-substring-no-properties start end)))
    (browse-url (concat "http://faust.grame.fr/library.html#"
                        (url-hexify-string q)))))

(defun faustine-build-all ()
  "Build all executables using `faustine-build'"
  (interactive)
  (faustine-build 1))

(defun faustine-diagram-all ()
  "Build all executables using `faustine-diagram'"
  (interactive)
  (faustine-diagram 1))

(defun faustine-source-code ()
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

(defun faustine-syntax-check ()
  "Check if Faust code buffer compiles."
  (interactive)
  (setq dsp-buffer (buffer-name))
  (setq-local output-check (shell-command-to-string (format "faust %s > /dev/null" dsp-buffer)))
  ;; (message "Output: (%s)" output-check)
  (if (string= "" output-check)
      (progn
        (faustine-minor-mode-red 0)
        (faustine-minor-mode-green t))
    (progn
      (faustine-minor-mode-green 0)
      (faustine-minor-mode-red t))))

(defun faustine-run ()
  "Run the executable generated by the current Faust code buffer."
  (interactive)
  (setq dsp-buffer (current-buffer))
  (setq dsp-buffer-name (buffer-name))
  (with-current-buffer (get-buffer-create output-buffer-name)
    (pop-to-buffer output-buffer-name nil t)
    ;; (previous-window)
    (goto-char (point-max))
    (start-process-shell-command "Run" (current-buffer)
                                 (format "./%s" (file-name-sans-extension
                                                 (file-name-nondirectory
                                                  dsp-buffer-name))))
    (other-window -1)
    (pop-to-buffer dsp-buffer nil t)))

(defun faustine-show (file)
  "Show FILE in a web page using default browser."
  (interactive)
  (browse-url-of-file file))

(defun faustine-mdoc ()
  "Generate Faust mdoc of the current faust file, display it in a buffer."
  (interactive)
  (setq dsp-buffer (current-buffer))
  (setq dsp-buffer-name (buffer-name))
  (with-current-buffer (get-buffer-create output-buffer-name)
    (pop-to-buffer output-buffer-name nil t)
    (faustine-output-mode)
    (goto-char (point-max))
    (call-process "/bin/bash" nil t nil "-c" (format "faust2mathdoc %s" dsp-buffer-name))
    (other-window -1)
    (pop-to-buffer dsp-buffer nil t))
  (setq temp-file-name (format "%s-mdoc/pdf/%s.pdf"
                               (file-name-sans-extension
                                 dsp-buffer-name)
                               (file-name-sans-extension
                                 dsp-buffer-name)))
  (faustine-show temp-file-name))

(defun log-to-buffer (process event)
  "Run the program, print the status to the output buffer, scroll buffer down."
  (let ((oldbuf (current-buffer)))
    (with-current-buffer (get-buffer-create output-buffer-name)
      (faustine-output-mode)
      (font-lock-fontify-buffer)
      (goto-char (point-max))
      (newline)
      (insert (format "%s | Process: %s\nEvent: %s"
                      (format-time-string "%H:%M:%S")
                      process
                      (replace-regexp-in-string "\n" " " event)))
      (if (get-buffer-window output-buffer-name `visible)
          (progn (setq other-window-scroll-buffer output-buffer-name)
                 (scroll-other-window))))))

(defun faustine-toggle-output-buffer ()
  "Show/hide Faust output buffer"
  (interactive)
  (if (get-buffer-window output-buffer-name `visible)
      (delete-window (get-buffer-window output-buffer-name `visible))
    (let ((oldbuf (current-buffer)))
      (with-current-buffer (get-buffer-create output-buffer-name)
        (display-buffer output-buffer-name)
        (if (> (+ 1 -16)
               (window-resizable
                (get-buffer-window output-buffer-name `visible) -16 nil))
            (window-resize (get-buffer-window output-buffer-name `visible) -16 nil))))))

(defun project-files (fname blist)
  "Recursively find all Faust links in FNAME, canonicalize and put them in BLIST, return it."
  (interactive)
  (add-to-list 'blist (expand-file-name fname))
  (with-temp-buffer
    (insert-file-contents-literally fname)
    (goto-char (point-min))
    (while (re-search-forward faustine-regexp-dsp nil t)
      (when (match-string 0)
        (let ((uri (expand-file-name (match-string 1))))
          (if (not (member uri blist))
              (setq blist (project-files uri blist))))))
    (identity blist)))

(defun faustine-build (&optional build-all)
  "Build the executable(s) using the `build-backend' executable. If BUILD-ALL is set, build all .dsp files in the current directory."
  (interactive)
  (setq dsp-buffer (current-buffer))
  (setq dsp-buffer-name (buffer-name))
  (with-current-buffer (get-buffer-create output-buffer-name)
    (pop-to-buffer output-buffer-name nil t)
    (faustine-output-mode)
    (goto-char (point-max))
    (insert "\nProcess Build started\n")
    (if build-all
        (progn (setq files-to-build (mapconcat 'identity (project-files dsp-buffer-name '()) " "))
               (message "Building ALL: %s" files-to-build))
      (progn (message "Building just %s" dsp-buffer)
             (setq files-to-build dsp-buffer)))
    (start-process-shell-command
     "Build"
     (current-buffer)
     (format "%s %s" build-backend files-to-build))
    (other-window -1)
    (message "files: %s" files-to-build)
    (pop-to-buffer dsp-buffer nil t)))

(defun faustine-diagram (&optional build-all)
  "Generate Faust diagram(s)."
  (interactive)
  (setq dsp-buffer (current-buffer))
  (setq dsp-buffer-name (buffer-name))
  (log-to-buffer "Diagram" "started")
  (let ((mylist nil)
        (files-to-build
         (if build-all
             (project-files dsp-buffer-name '())
           (list dsp-buffer-name)))
        (display-mode (if build-all "all" "single")))
    (setq command-output (shell-command-to-string (format "faust2svg %s" (mapconcat 'identity files-to-build " "))))
    (if (string= "" command-output)
        (progn
          (log-to-buffer "Diagram" "finished")
          (faustine-build-temp-file files-to-build diagram-page-name dsp-buffer-name display-mode)
          (faustine-show diagram-page-name))
      (log-to-buffer "Diagram" (format "Error: %s" command-output)))))

(defun faustine-build-temp-file (list temp-file-name diagram display-mode)
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
                 (class (if (equal diagram (file-name-nondirectory (car list)))
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
                     (file-name-nondirectory dsp-element)
                     (file-name-nondirectory dsp-file-name)
                     (file-name-nondirectory dsp-file-name)
                     ) nil temp-file-name 'append 0 nil nil)))
      (setq list (cdr list)))
    (write-region "</div>
</body>
</html>\n" nil temp-file-name 'append 0 nil nil)))

(provide 'faustine)
