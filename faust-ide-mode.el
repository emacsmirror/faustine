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

(defun faust-ide-mode-menu ()
  "Create the faust-ide-mode menu"
  (define-key-after
    global-map
    [menu-bar faust-ide-mode-menu]
    (cons "faust-ide-mode" (make-sparse-keymap "hoot hoot"))
    'tools )

  (define-key
    global-map
    [menu-bar faust-ide-mode-menu prefs]
    '("Prefs" . (lambda () (interactive) (customize-group 'faust-ide-mode))))

  (define-key
    global-map
    [menu-bar faust-ide-mode-menu help]
    '("Help" . (lambda () (interactive) (describe-mode (get-buffer "faust-ide-mode"))))))

(add-hook 'faust-ide-mode-mode-hook
          (lambda ()
            (toggle-truncate-lines 1)
            (linum-mode -1)
            (hl-line-mode t)))

(add-hook 'faust-ide-mode-message-mode-hook
          (lambda ()
            (goto-address-mode t)))


(defun faust-ide-mode-mode ()
  "
         .' '.
-        .   .            \\\\       faust-ide-mode
 `.        .         .  -{{{:}     A lightweight Mail User Agent for GNU Emacs.
   ' .  . ' ' .  . '      //

Type \\[customize-group] faust-ide-mode (or use the menu)  to set it up.

Available commands in summary (list) buffer:
 \\{faust-ide-mode-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (unless faust-ide-mode-mode-map
    (setq faust-ide-mode-mode-map (make-sparse-keymap))

    (define-key faust-ide-mode-mode-map "\r" 'faust-ide-mode-open)
    (define-key faust-ide-mode-mode-map "+" 'faust-ide-mode-create-folder)
    (define-key faust-ide-mode-mode-map "/" 'isearch-forward-regexp)
    (define-key faust-ide-mode-mode-map "B" 'bury-buffer)
    (define-key faust-ide-mode-mode-map "d" 'faust-ide-mode-delete)
    (define-key faust-ide-mode-mode-map "g" 'faust-ide-mode-redraw)
    ;; (define-key faust-ide-mode-mode-map "h" 'faust-ide-mode-toggle-headers)
    (define-key faust-ide-mode-mode-map "K" 'faust-ide-mode-kill-folder)
    ;; (define-key faust-ide-mode-mode-map "r" 'faust-ide-mode-reply-to)
    ;; (define-key faust-ide-mode-mode-map "n" 'next-line)
    (define-key faust-ide-mode-mode-map "m" 'faust-ide-mode-move)
    ;; (define-key faust-ide-mode-mode-map "p" 'previous-line)
    (define-key faust-ide-mode-mode-map "n" 'message-mail)
    (define-key faust-ide-mode-mode-map "S" 'faust-ide-mode-show-structure)
    (define-key faust-ide-mode-mode-map "u" 'faust-ide-mode-undelete)
    (define-key faust-ide-mode-mode-map "x" 'faust-ide-mode-expunge)
    (define-key faust-ide-mode-mode-map "X" 'faust-ide-mode-spam))
  (use-local-map faust-ide-mode-mode-map)
  ;;set the mode as a non-editor mode
  (put 'faust-ide-mode-mode 'mode-class 'special)
  ;;specify the mode name
  (setq mode-name "faust-ide-mode")
  (setq major-mode 'faust-ide-mode-mode)
  ;;setup the buffer to be modal
  (setq buffer-read-only 't)
  ;;specify that this buffer has been initialized with the major mode
  (make-local-variable 'faust-ide-mode-mode-initialized-p)
  (setq faust-ide-mode-mode-initialized-p 't)
  ;;ensure that paragraphs are considered to be whole mailing lists
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "^[A-Za-z0-9]")
  ;; Ensure the undo doesn't get recorded for this buffer
  (buffer-disable-undo)
  ;;setup the kill-buffer stuff
  (make-local-variable 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'faust-ide-mode-kill-buffer-hook)
  ;; Make the connection local
  (make-local-variable 'faust-ide-mode-connection)
  ;;make the username and password local
  (make-local-variable 'faust-ide-mode-username)
  (make-local-variable 'faust-ide-mode-password)
  ;;run the mode hooks
  (run-hooks 'faust-ide-mode-mode-hook))


(define-derived-mode faust-ide-mode-message-mode message-mode "Faust-Ide-Mode Message" "
Available commands in message composition buffer:

 \\{faust-ide-mode-message-mode-map}

Also available in message composition buffer:
\\{mml-mode-map}
"
  (unless faust-ide-mode-message-keymap-initializedp
    (define-key faust-ide-mode-message-mode-map "\r" 'faust-ide-mode-message-open-attachment)
    ;;(define-key faust-ide-mode-message-mode-map "s" 'faust-ide-mode-message-save-attachment)
    ;;(define-key faust-ide-mode-message-mode-map "d" 'faust-ide-mode-message-dump-attachment)
    (define-key faust-ide-mode-message-mode-map "a" 'message-wide-reply)
    (define-key faust-ide-mode-message-mode-map "h" 'faust-ide-mode-toggle-headers)
    (define-key faust-ide-mode-message-mode-map "H" 'faust-ide-mode-wash-html)
    (define-key faust-ide-mode-message-mode-map "s-i" 'message-insert-or-toggle-importance)

    (define-key faust-ide-mode-message-mode-map "q" 'faust-ide-mode-kill-buffer)

    (define-key faust-ide-mode-message-mode-map "r" 'message-reply)
    (setq faust-ide-mode-message-keymap-initializedp 't))
  ;;set the mode as a non-editor mode
  (put 'faust-ide-mode-message-mode 'mode-class 'special)
  ;;ensure that paragraphs are considered to be whole mailing lists
  (make-local-variable 'paragraph-start)
  (setq paragraph-start paragraph-separate)
  ;;setup the buffer to be read only
  ;; (make-local-variable 'buffer-read-only)
  (setq buffer-read-only 't)
  ;;run the mode hooks
  (run-hooks 'faust-ide-mode-message-mode-hook))

(provide 'faust-ide-mode)
