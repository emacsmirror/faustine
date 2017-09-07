(require 'ert)
(require 'faust-mode)
(require 'faustine)

(ert-deftest no-byte-compile-warnings ()
  "Byte-compile should not emit warnings"
  (byte-compile-file "faustine.el")
  (switch-to-buffer "*Compile-Log*")
  (let ((lines (buffer-substring (point-min) (point-max))))
    (should (not (string-match "Warning:" lines)))))
