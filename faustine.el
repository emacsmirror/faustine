;;; faustine.el --- Edit, visualize, build and run Faust code

;; Copyright (C) 2017 Yassin Philip

;; Author: Yassin Philip <xaccrocheur@gmail.com>
;; Maintainer: Yassin Philip <xaccrocheur@gmail.com>
;; Keywords: modes, faust
;; Version: 1.0.1
;; URL: https://bitbucket.org/yassinphilip/faustine

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Edit, visualize, build and run Faust code.
;; Inspired by Faustworks, now deprecated.

;;; Code:

(require 'smie)

(defconst faustine-faust-keywords-lib-analyzer
  '("amp_follower" "amp_follower_ud" "amp_follower_ar" "mth_octave_analyzer[N]" "mth_octave_spectral_level6e" "octave_filterbank" "octave_analyzer" "half_octave_filterbank" "half_octave_analyzer" "third_octave_filterbank" "third_octave_analyzer" "analyzer"))

(defconst faustine-faust-keywords-lib-basic
  '("samp2sec" "sec2samp" "db2linear" "linear2db" "lin2LogGain" "log2LinGain" "tau2pole" "pole2tau" "midikey2hz" "pianokey2hz" "hz2pianokey" "countdown" "countup" "sweep" "time" "tempo" "period" "pulse" "pulsen" "beat" "count" "take" "subseq" "if" "selector" "selectn" "select2stereo" "Other" "latch" "sAndH" "peakhold" "peakholder" "impulsify" "automat" "bypass1" "bypass2"))

(defconst faustine-faust-keywords-lib-compressor
  '("compressor_mono" "compressor_stereo" "limiter_*"))

(defconst faustine-faust-keywords-lib-delay
  '("delay" "fdelay" "sdelay" "fdelaylti" "fdelayltv" "fdelay[n]" "fdelay[n]a"))

(defconst faustine-faust-keywords-lib-demo
  '("Analyzers" "mth_octave_spectral_level_demo" "Filters" "parametric_eq_demo" "spectral_tilt_demo" "mth_octave_filterbank_demo" "filterbank_demo" "Effects" "cubicnl_demo" "gate_demo" "compressor_demo" "exciter" "moog_vcf_demo" "wah4_demo" "crybaby_demo" "vocoder_demo" "flanger_demo" "phaser2_demo" "freeverb_demo" "stereo_reverb_tester" "fdnrev0_demo" "zita_rev_fdn_demo" "zita_rev1" "Generators" "sawtooth_demo" "virtual_analog_oscillator_demo" "oscrs_demo"))

(defconst faustine-faust-keywords-lib-envelope
  '("smoothEnvelope" "ar" "asr" "adsr"))

(defconst faustine-faust-keywords-lib-filter
  '("zero" "pole" "integrator" "dcblockerat" "dcblocker" "ff_comb" "ff_fcomb" "ffcombfilter" "fb_comb" "fb_fcomb" "rev1" "fbcombfilter" "ffbcombfilter" "allpass_comb" "allpass_fcomb" "rev2" "allpass_fcomb5" "allpass_fcomb1a" "iir" "fir" "conv" "convN" "tf1" "tf2" "tf3" "notchw" "tf21" "tf22" "tf22t" "tf21t" "av2sv" "bvav2nuv" "iir_lat2" "allpassnt" "iir_kl" "allpassnklt" "iir_lat1" "allpassn1mt" "iir_nl" "allpassnnlt" "tf2np" "wgr" "nlf2" "apnl" "allpassn" "allpassnn" "allpasskl" "allpass1m" "tf2s" "tf2snp" "tf3slf" "tf1s" "tf2sb" "tf1sb" "resonlp" "resonhp" "resonbp" "lowpass" "highpass" "lowpass0_highpass1" "lowpass_plus|minus_highpass" "lowpass_plus|minus_highpass" "lowpass3e" "lowpass6e" "highpass3e" "highpass6e" "bandpass" "bandstop" "bandpass6e" "bandpass12e" "low_shelf" "lowshelf_other_freq" "high_shelf" "highshelf_other_freq" "peak_eq" "cubicnl" "gate_mono" "gate_stereo" "Filtering" "speakerbp" "piano_dispersion_filter" "stereo_width" "echo" "transpose" "mesh_square"))

(defconst faustine-faust-keywords-lib-hoa
  '("encoder" "decoder" "decoderStereo" "optimBasic" "optimMaxRe" "optimInPhase" "Usage" "wider" "map" "rotate"))

(defconst faustine-faust-keywords-lib-math
  '("SR" "BS" "PI" "FTZ" "neg" "sub(x,y)" "inv" "cbrt" "hypot" "ldexp" "scalb" "log1p" "logb" "ilogb" "log2" "expm1" "acosh" "asinh" "atanh" "sinh" "cosh" "tanh" "erf" "erfc" "gamma" "lgamma" "J0" "J1" "Jn" "Y0" "Y1" "Yn" "fabs" "fmax" "fmin" "np2" "frac" "isnan" "chebychev" "chebychevpoly" "diffn"))

(defconst faustine-faust-keywords-lib-misceffect
  '("Dynamic" "cubicnl" "gate_mono" "gate_stereo" "Filtering" "speakerbp" "piano_dispersion_filter" "stereo_width" "echo" "transpose" "Meshes" "mesh_square"))

(defconst faustine-faust-keywords-lib-miscoscillator
  '("sinwaveform" "coswaveform" "phasor" "oscsin" "osc" "oscos" "oscp" "osci" "lf_imptrain" "lf_pulsetrainpos" "lf_squarewavepos" "lf_squarewave" "lf_trianglepos" "lf_rawsaw" "lf_sawpos" "lf_sawpos_phase" "sawN(N,freq)" "sawNp" "saw2dpw(freq)" "saw2(freq)" "saw3(freq)" "saw4(freq)" "saw5(freq)" "saw6(freq)" "sawtooth(freq)" "saw2f2(freq)" "saw2f4(freq)" "pulsetrainN" "pulsetrain" "squareN" "square" "imptrain" "imptrainN" "triangle" "triangleN" "oscb" "oscr" "oscrs" "oscs" "oscs" "oscw" "oscwq" "oscwc" "oscws"))

(defconst faustine-faust-keywords-lib-noise
  '("noise" "multirandom" "multinoise" "noises" "pink_noise" "pink_noise_vm" "lfnoise" "lfnoise0" "lfnoiseN"))

(defconst faustine-faust-keywords-lib-phafla
  '("flanger_mono" "flanger_stereo" "phaser2_mono" "phaser2_stereo"))

(defconst faustine-faust-keywords-lib-pm
  '("chain" "input" "output" "terminations" "fullTerminations" "leftTermination" "rightTermination" "waveguide" "idealString"))

(defconst faustine-faust-keywords-lib-reverb
  '("jcrev" "satrev" "mono_freeverb" "stereo_freeverb" "fdnrev0" "zita_rev_fdn" "zita_rev1_stereo" "zita_rev1_ambi"))

(defconst faustine-faust-keywords-lib-route
  '("cross" "crossnn" "crossn1" "interleave" "butterfly" "hadamard" "recursivize"))

(defconst faustine-faust-keywords-lib-signal
  '("bus" "block" "interpolate" "smooth" "smoo" "polySmooth" "bsmooth" "lag_ud" "dot"))

(defconst faustine-faust-keywords-lib-spat
  '("panner" "spat" "stereoize"))

(defconst faustine-faust-keywords-lib-synth
  '("popFilterPerc" "dubDub" "sawTrombone" "combString" "additiveDrum" "additiveDrum"))

(defconst faustine-faust-keywords-lib-vaeffect
  '("moog_vcf" "moog_vcf_2b" "wah4" "autowah" "crybaby" "vocoder"))

(defvar faustine-faust-keywords-lib
  (append
   faustine-faust-keywords-lib-analyzer
   faustine-faust-keywords-lib-basic
   faustine-faust-keywords-lib-compressor
   faustine-faust-keywords-lib-delay
   faustine-faust-keywords-lib-demo
   faustine-faust-keywords-lib-envelope
   faustine-faust-keywords-lib-filter
   faustine-faust-keywords-lib-hoa
   faustine-faust-keywords-lib-math
   faustine-faust-keywords-lib-misceffect
   faustine-faust-keywords-lib-miscoscillator
   faustine-faust-keywords-lib-noise
   faustine-faust-keywords-lib-phafla
   faustine-faust-keywords-lib-pm
   faustine-faust-keywords-lib-reverb
   faustine-faust-keywords-lib-route
   faustine-faust-keywords-lib-signal
   faustine-faust-keywords-lib-spat
   faustine-faust-keywords-lib-synth
   faustine-faust-keywords-lib-vaeffect)
  "All the Faust library function keywords.")

(defvar ac-sources)

(defconst faustine-faust-keywords-statements
  '("process" "with" "case" "seq" "par" "sum" "prod" "include" "import" "component" "library" "environment" "declare" "define" "undef" "error" "pragma" "ident" "if" "def" "else" "elif" "endif" "line" "warning"))

(defconst faustine-faust-keywords-functions
  '("mem" "prefix" "int" "float" "rdtable" "rwtable" "select2" "select3" "ffunction" "fconstant" "fvariable" "attach" "acos" "asin" "atan" "atan2" "cos" "sin" "tan" "exp" "log" "log10" "pow" "sqrt" "abs" "min" "max" "fmod" "remainder" "floor" "ceil" "rint"))

(defconst faustine-faust-keywords-math
  '("mem" "prefix" "int" "float" "rdtable" "rwtable" "select2" "select3" "ffunction" "fconstant" "fvariable" "attach" "acos" "asin" "atan" "atan2" "cos" "sin" "tan" "exp" "log" "log10" "pow" "sqrt" "abs" "min" "max" "fmod" "remainder" "floor" "ceil" "rint"))

(defconst faustine-faust-keywords-ui
  '("button" "checkbox" "vslider" "hslider" "nentry" "vgroup" "hgroup" "tgroup" "vbargraph" "hbargraph"))

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

(defcustom faustine-kb-toggle-output-buffer "C-c C-o"
  "Show/hide Faust output buffer."
  :type '(string)
  :group 'keyboard-shortcuts)

(defcustom faustine-output-buffer-name "*Faust*"
  "The name of the Faust output buffer.
Surround it with \"*\" to hide it in special buffers."
  :type '(string)
  :group 'faustine)

(defcustom faustine-pop-output-buffer nil
  "Pop open the Faust output buffer at each command call."
  :type '(boolean)
  :group 'faustine)

(defcustom faustine-output-buffer-height 22
  "The height/width of the Faust output buffer."
  :type 'integer
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

(eval-and-compile
  (defcustom faustine-faust-extension "dsp"
    "The Faust files extension."
    :type '(string)
    :group 'faustine))

(defcustom faustine-build-backend 'faust2jaqt
  "The Faust code-to-executable build backend script."
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

(defvar faustine-path (file-name-directory (or load-file-name (buffer-file-name))))

(defvar faustine-green-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for the function `faustine-green-mode'.")

(defvar faustine-red-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for the function `faustine-red-mode'.")

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
     ["Generate all linked diagrams" (faustine-diagram t) t]
     ["Build all linked executables" (faustine-build t) t])
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

(defvar faust-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `faust-mode'.")

(defvar faust-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/  ". 124b" st)
    (modify-syntax-entry ?*  ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?\^m "> b" st)
    st)
  "Syntax table for `faust-mode'.")

(defvar faustine-regexp-keywords-function (regexp-opt faustine-faust-keywords-functions 'words))
(defvar faustine-regexp-keywords-statement (regexp-opt faustine-faust-keywords-statements 'words))
(defvar faustine-regexp-keywords-lib (regexp-opt faustine-faust-keywords-lib 'words))
(defvar faustine-regexp-keywords-ui (regexp-opt faustine-faust-keywords-ui 'words))

(defvar faustine-regexp-faust-file (rx
                                    "\"" (submatch
                                          (and word-start
                                               (one-or-more word)
                                               "."
                                               (eval faustine-faust-extension)))
                                    "\"")
  "The regexp to match `something.faust'.")

(defvar faustine-regexp-log
  (rx
   (submatch (and word-start
                  (one-or-more word) "." (eval faustine-faust-extension) ":" (one-or-more digit))))
  "The regexp to match `something.faust:num'.")

(defconst faustine-regexp-lib (rx
                               "\"" (submatch (and word-start (one-or-more word) ".lib")) "\"")
  "The regexp to match `something.lib'.")

(defconst faustine-regexp-exe (rx
                               (submatch (and (or "./" "/") (one-or-more (any word "/")))) ";")
  "The regexp to match `/some/thing'.")

(defconst faustine-regexp-faust-operator (rx
                                          (any ",:*-+><")))

(defconst faustine-regexp-faust-delimiters (rx
                                            (any "{}()[];")))

(defconst faustine-regexp-faust-numbers (rx
                                         (one-or-more digit)))

(defconst faustine-output-mode-keywords-proc
  (rx
   (and word-start (or "Build" "Check" "Click" "C++" "Diagram" "Mdoc" "Run") word-end)))

(defvar faustine-output-mode-keywords-faust-file
  (rx
   (submatch (and word-start
                  (one-or-more word) "." (eval faustine-faust-extension)
                  ;; word-end
                  )))
  "The regexp to match `something.faust:num'.")

(defconst faustine-output-mode-keywords-jack
  (rx
   (or (and line-start (or "ins" "outs"))
       (and line-start "physical" space (or "input" "output") space "system")
       (and line-start "The" space (or "sample rate" "buffer size") space "is now"))))

(defconst faustine-output-mode-keywords-time
  (rx
   (and line-start (one-or-more digit) ":" (one-or-more digit) ":" (one-or-more digit))))

(defconst faustine-output-mode-keywords-status
  (rx (and word-start (or "started" "finished") word-end)))

(defconst faustine-output-mode-keywords-bad
  (rx (and word-start (or "warning" "error") word-end)))

(defconst faust-mode-font-lock-keywords
  `((,faustine-regexp-keywords-function . font-lock-function-name-face)
    (,faustine-regexp-keywords-statement . font-lock-keyword-face)
    (,faustine-regexp-keywords-lib . font-lock-keyword-face)
    (,faustine-regexp-keywords-ui . font-lock-variable-name-face)
    (,faustine-regexp-faust-operator . font-lock-type-face)
    (,faustine-regexp-keywords-lib . font-lock-builtin-face)
    (,faustine-regexp-faust-delimiters . font-lock-reference-face)
    (,faustine-regexp-faust-numbers . font-lock-negation-char-face)))

(defconst faustine-output-mode-font-lock-keywords
  `((,faustine-output-mode-keywords-proc . 'font-lock-string-face)
    (,faustine-output-mode-keywords-jack . 'font-lock-variable-name-face)
    (,faustine-output-mode-keywords-bad . 'font-lock-warning-face)
    (,faustine-output-mode-keywords-faust-file . 'font-lock-function-name-face)
    (,faustine-output-mode-keywords-time . 'font-lock-type-face)
    (,faustine-output-mode-keywords-status . 'font-lock-keyword-face)))

(defvar faust-mode-ac-source
  '((candidates . faustine-faust-keywords-lib)))

;;;###autoload
(define-derived-mode faust-mode prog-mode "Faust"

  "Faustine is a lightweight IDE that leverages the mighty power of the faust executable.

Use `faustine-configure' (\\[faustine-configure]) to set it up.
Available commands while editing Faust (*.dsp) files:

\\{faust-mode-map}"

  (kill-all-local-variables)
  (setq-local comment-start "//")

  (setq mode-name "Faust"
        major-mode 'faust-mode
        comment-end ""
        font-lock-defaults '(faust-mode-font-lock-keywords))

  (if (boundp 'ac-sources)
      (progn
        (add-to-list 'ac-modes 'faust-mode)
        (add-to-list 'ac-sources 'faust-mode-ac-source))
    (message "You should really install auto-complete"))

  (smie-setup nil #'ignore)

  (add-hook 'find-file-hook 'faustine-syntax-check nil t)
  (add-hook 'after-save-hook 'faustine-syntax-check nil t)

  (set-syntax-table faust-mode-syntax-table)
  (use-local-map faust-mode-map)

  (define-key faust-mode-map (kbd faustine-kb-configure) 'faustine-configure)
  (define-key faust-mode-map (kbd faustine-kb-build) 'faustine-build)
  (define-key faust-mode-map (kbd faustine-kb-build-all) 'faustine-build-all)
  (define-key faust-mode-map (kbd faustine-kb-diagram) 'faustine-diagram)
  (define-key faust-mode-map (kbd faustine-kb-diagram-all) 'faustine-diagram-all)
  (define-key faust-mode-map (kbd faustine-kb-mdoc) 'faustine-mdoc)
  (define-key faust-mode-map (kbd faustine-kb-online-doc) 'faustine-online-doc)
  (define-key faust-mode-map (kbd faustine-kb-run) 'faustine-run)
  (define-key faust-mode-map (kbd faustine-kb-source-code) 'faustine-source-code)
  (define-key faust-mode-map (kbd faustine-kb-syntax-check) 'faustine-syntax-check)
  (define-key faust-mode-map (kbd faustine-kb-toggle-output-buffer) 'faustine-toggle-output-buffer)

  (run-hooks 'change-major-mode-after-body-hook 'after-change-major-mode-hook))

(define-derived-mode faustine-output-mode fundamental-mode
  "The Faust output buffer mode."
  (kill-all-local-variables)
  (setq font-lock-defaults '(faustine-output-mode-font-lock-keywords t))
  (font-lock-fontify-buffer))

(defun faustine-project-files (fname blist &optional calling-process)
  "Recursively find all Faust links in FNAME, put them in BLIST, return BLIST.
Log CALLING-PROCESS to output buffer."
  (add-to-list 'blist (expand-file-name fname))
  (with-temp-buffer
    (if (file-exists-p (expand-file-name fname))
        (insert-file-contents-literally fname))
    (goto-char (point-min))
    (while (re-search-forward faustine-regexp-faust-file nil t nil)
      (when (match-string 0)
        (let ((uri (expand-file-name (match-string 1)))
              (isok-p (file-exists-p (expand-file-name (match-string 1)))))
          (if (not isok-p)
              (faustine-sentinel (format "%s:%s" calling-process fname)
                                 (format "warning %s does not exist\n" uri)))
          (if (and isok-p (not (member uri blist)))
              (setq blist (faustine-project-files uri blist))))))
    (identity blist)))

(defun faustine-sentinel (process event)
  "Log PROCESS and EVENT to output buffer."
  (let ((calling-buffer (cadr (split-string (format "%s" process) ":")))
        (file-name (file-name-sans-extension
                    (cadr (split-string (format "%s" process) ":"))))
        (status-ok (string-prefix-p "finished" event))
        (process (format "%s" process)))
    (with-current-buffer (get-buffer-create faustine-output-buffer-name)
      (faustine-output-mode)
      (goto-char (point-max))

      (insert (format "%s | %s %s"
                      (format-time-string "%H:%M:%S")
                      process
                      event))

      (faustine-buttonize-buffer 'log)
      (faustine-buttonize-buffer 'exe)
      (when (get-buffer-window faustine-output-buffer-name `visible)
        (with-selected-window (get-buffer-window (current-buffer))
          (goto-char  (point-max)))))

    (when (string-prefix-p "Check" process)
      (if status-ok
          (progn (faustine-red-mode 0) (faustine-green-mode t))
        (progn (faustine-green-mode 0) (faustine-red-mode t))))

    (when status-ok
      (when (string-prefix-p "Mdoc" process)
        (browse-url-of-file (format "%s-mdoc/pdf/%s.pdf" file-name file-name)))
      (when (string-prefix-p "C++" process)
        (find-file-other-window (format "%s.cpp" file-name)))
      (when (string-prefix-p "Diagram" process)
        (browse-url-of-file faustine-diagram-page-name))))
  (faustine-buttonize-buffer 'dsp)
  (faustine-buttonize-buffer 'lib)
  (bury-buffer faustine-output-buffer-name)
  (if faustine-pop-output-buffer
      (faustine-open-output-buffer)))

(define-button-type 'faustine-button-lib
  'help-echo "Click to open"
  'follow-link t
  'action #'faustine-button-lib-action)

(define-button-type 'faustine-button-dsp
  'help-echo "Click to open"
  'follow-link t
  'face 'button
  'action #'faustine-button-dsp-action)

(define-button-type 'faustine-button-log
  'help-echo "Click to open"
  'follow-link t
  'action #'faustine-button-log-action)

(define-button-type 'faustine-button-exe
  'help-echo "Click to run"
  'follow-link t
  'action #'faustine-button-exe-action)

(defun faustine-button-lib-action (button)
  "Search Faust library file and insert BUTTON."
  (find-file (format "%s%s"
                     faustine-faust-libs-dir
                     (buffer-substring
                      (button-start button) (button-end button))))
  (faust-mode)
  (faustine-buttonize-buffer 'lib))

(defun faustine-button-dsp-action (button)
  "Search Faust file and insert BUTTON."
  (find-file (format "%s%s"
                     (file-name-directory buffer-file-name)
                     (buffer-substring
                      (button-start button) (button-end button)))))

(defun faustine-button-log-action (button)
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

(defun faustine-button-exe-action (button)
  "Run the executable described by BUTTON."
  (faustine-run (buffer-substring-no-properties
                 (button-start button) (button-end button))))

(defun faustine-buttonize-buffer (type)
  "Turn all found strings into buttons of type TYPE."
  (save-excursion
    (goto-char (point-min))
    (let ((regexp (cond ((eq type 'dsp) faustine-regexp-faust-file)
                        ((eq type 'log) faustine-regexp-log)
                        ((eq type 'exe) faustine-regexp-exe)
                        ((eq type 'lib) faustine-regexp-lib))))
      (while (re-search-forward regexp nil t nil)
        (if
            (not (eq 'comment (syntax-ppss-context (syntax-ppss))))
            (progn
              (remove-overlays (match-beginning 1) (match-end 1) nil nil)
              (make-button (match-beginning 1) (match-end 1)
                           :type (intern-soft (concat "faustine-button-" (symbol-name type)))))
          (remove-overlays (match-beginning 1) (match-end 1) nil nil))))))

(defun faustine-diagram-all ()
  "Build all linked diagrams."
  (interactive)
  (faustine-diagram t))

(defun faustine-build-all ()
  "Build all linked executables."
  (interactive)
  (faustine-build t))

(defun faustine-configure ()
  "Use `cutomize-group' to set up Faustine preferences."
  (interactive)
  (customize-group 'faustine))

(defun faustine-online-doc (start end)
  "Websearch selected (or under point) string on faust.grame.fr.
Build a button from START to END."
  (interactive "r")
  (let ((selection (if (use-region-p)
                       (buffer-substring-no-properties start end)
                     (current-word))))
    (browse-url (concat "http://faust.grame.fr/library.html#"
                        (url-hexify-string selection)))))

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
      (if (> -10
             (window-resizable (get-buffer-window faustine-output-buffer-name `visible)
                               (- faustine-output-buffer-height) nil))
          (window-resize (get-buffer-window faustine-output-buffer-name `visible)
                         (- faustine-output-buffer-height) nil)))))

(defun faustine-syntax-check ()
  "Check if Faust code buffer compiles.
Run at load and save time."
  (interactive)
  (let ((process (start-process-shell-command
                  (format "Check:%s" (buffer-name))
                  faustine-output-buffer-name
                  (format "faust %s > /dev/null" (buffer-name)))))
    (set-process-sentinel process 'faustine-sentinel)))

(defun faustine-mdoc (&optional build-all)
  "Generate mdoc of the current file, display it in a buffer.
If BUILD-ALL is set, build all linked files."
  (interactive)
  (let ((process (start-process-shell-command
                  (format "Mdoc:%s" (buffer-name))
                  faustine-output-buffer-name
                  (format "faust2mathdoc %s" (current-buffer)))))
    (faustine-sentinel (format "Mdoc:%s" (buffer-name)) "started\n")
    (set-process-sentinel process 'faustine-sentinel)))

(defun faustine-build (&optional build-all)
  "Build the current buffer/file executable(s).
If BUILD-ALL is set, build all Faust files referenced by this one."
  (interactive)
  (faustine-sentinel (format "Build:%s" (buffer-name)) "started\n")
  (let* ((files-to-build (if build-all
                             (mapconcat 'identity (faustine-project-files (buffer-name) '() "Build") " ")
                           (current-buffer)))
         (process (start-process-shell-command
                   (format "Build:%s" (buffer-name))
                   faustine-output-buffer-name
                   (format "%s %s" faustine-build-backend files-to-build))))
    (set-process-sentinel process 'faustine-sentinel)))

(defun faustine-run (&optional button)
  "Run the executable generated by the current Faust code buffer or passed by BUTTON."
  (interactive)
  (let* ((command (if button
                      button
                    (format "./%s" (file-name-sans-extension
                                    (file-name-nondirectory
                                     (buffer-name))))))
         (buffer (if button "Click" (buffer-name)))
         (process (start-process-shell-command
                   (format "Run:%s" buffer)
                   faustine-output-buffer-name
                   command)))
    (set-process-sentinel process 'faustine-sentinel)))

(defun faustine-source-code ()
  "Generate Faust C++ code of the current faust file, display it in `faustine-c++-buffer-name'."
  (interactive)
  (let ((process (start-process-shell-command
                  (format "C++:%s" (buffer-name))
                  nil
                  (format "faust -uim %s -o %s.cpp"
                          (buffer-name)
                          (file-name-sans-extension (buffer-name))))))
    (set-process-sentinel process 'faustine-sentinel)))

(defun faustine-diagram (&optional build-all)
  "Generate Faust diagram(s).
If BUILD-ALL is set, build all `faustine-faust-extension` files referenced by this one."
  (interactive)
  (message "plop")
  (let ((files-to-build
         (if build-all (faustine-project-files (buffer-name) '() "Diagram") (list (buffer-name))))
        (display-mode
         (if build-all "all" "single")))
    (let ((process (start-process-shell-command
                    (format "Diagram:%s" (buffer-name))
                    nil
                    (format "faust2svg %s" (mapconcat 'identity files-to-build " ")))))
      (faustine-build-html-file files-to-build (buffer-name) display-mode)
      (set-process-sentinel process 'faustine-sentinel))))

(defun faustine-build-html-file (list diagram display-mode)
  "Build a minimal HTML (web) page to display Faust diagram(s).
LIST is the list of files to display, DIAGRAM is the current file, and DISPLAY-MODE is the mode."
  (when (file-regular-p faustine-diagram-page-name)
    (delete-file faustine-diagram-page-name))
  (let ((flex-value (if (equal display-mode "all") "" "100%")))
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
                     (file-name-nondirectory (directory-file-name svg-dir)))
             nil faustine-diagram-page-name 'append 0 nil nil)))
      (setq list (cdr list)))
    (write-region "</div>
</body>
</html>\n" nil faustine-diagram-page-name 'append 0 nil nil)))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.dsp\\'" . faust-mode))

(provide 'faustine)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; faustine.el ends here
