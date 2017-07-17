;;; faust-ide-mode.el --- Minor mode for editing Faust code

;; URL: https://github.com/xaccrocheur/faust-ide-mode/
;; Maintainer: xaccrocheur@gmail.com
;; Keywords: languages, faust

;; This file is NOT part of GNU Emacs.

;; faust-ide-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; faust-ide-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Minor mode for editing Faust code (uses c-mode as a basis / fallback).
;; Faust — cross-platform programming language for graphics/sound applications and experiments.
;; http://www.warmplace.ru/soft/faust
;; Originally created by Alexander Zolotov (NightRadio) and Mik Razuvaev (Goglus) for non-programmers, demosceners and designers.

;;; Code:

;; Faust syntax
(setq faust-keywords '("if" "else" "else" "while" "break" "continue" "go" "goto" "halt" "include" "fn" "ret"))
(setq faust-functions '("new" "remove" "remove_with_alpha" "resize" "convert_type" "clean" "clone" "copy" "get_size" "get_xsize" "get_ysize" "get_esize" "get_type" "get_flags" "set_flags" "reset_flags" "get_prop" "set_prop" "remove_props" "show_memory_debug_messages" "zlib_pack" "zlib_unpack" "num_to_str" "str_to_num" "strcat" "strcmp" "strlen" "strstr" "sprintf" "printf" "fprintf" "logf" "get_log" "load" "fload" "save" "fsave" "get_real_path" "new_flist" "remove_flist" "get_flist_name" "get_flist_type" "flist_next" "get_file_size" "remove_file" "rename_file" "copy_file" "create_directory" "set_disk0" "get_disk0" "fopen" "fopen_mem" "fclose" "fputc" "fputs" "fwrite" "fgetc" "fgets" "fread" "feof" "fflush" "fseek" "ftell" "setxattr" "frame" "vsync" "set_screen" "get_screen" "set_zbuf" "get_zbuf" "clear_zbuf" "get_color" "get_red" "get_green" "get_blue" "get_blend" "transp" "get_transp" "clear" "dot" "dot3d" "get_dot" "get_dot3d" "line" "line3d" "box" "fbox" "pixi" "triangles3d" "sort_triangles3d" "set_key_color" "get_key_color" "set_alpha" "get_alpha" "print" "get_text_xsize" "get_text_ysize" "set_font" "get_font" "effector" "color_gradient" "split_rgb" "split_ycbcr" "set_gl_callback" "remove_gl_data" "gl_draw_arrays" "gl_blend_func" "pack_frame" "unpack_frame" "create_anim" "remove_anim" "clone_frame" "remove_frame" "play" "stop" "t_reset" "t_rotate" "t_translate" "t_scale" "t_push_matrix" "t_pop_matrix" "t_get_matrix" "t_set_matrix" "t_mul_matrix" "t_point" "set_audio_callback" "enable_audio_input" "get_note_freq" "midi_open_client" "midi_close_client" "midi_get_device" "midi_open_port" "midi_reopen_port" "midi_close_port" "midi_get_event" "midi_get_event_time" "midi_next_event" "midi_send_event" "start_timer" "get_timer" "get_year" "get_month" "get_day" "get_hours" "get_minutes" "get_seconds" "get_ticks" "get_tps" "sleep" "get_event" "set_quit_action" "thread_create" "thread_destroy" "mutex_create" "mutex_destroy" "mutex_lock" "mutex_trylock" "mutex_unlock" "op_cn" "op_cc" "op_ccn" "generator" "wavetable_generator" "sampler" "envelope2p" "gradient" "fft" "new_filter" "remove_filter" "reset_filter" "init_filter" "apply_filter" "replace_values" "file_dialog" "prefs_dialog" "open_url" "dlopen" "dlclose" "dlsym" "dlcall" "system" "argc" "argv" "exit"))
(setq faust-types '("INT" "INT8" "INT16" "INT32" "INT64" "FLOAT" "FLOAT32" "FLOAT64" "DYNAMIC" "PIXEL"))
(setq faust-containers '("INT" "INT8" "INT16" "INT32" "INT64" "FLOAT" "FLOAT32" "FLOAT64" "DYNAMIC" "PIXEL" "CFLAG_INTERP" "GL_MIN_LINEAR" "GL_MAG_LINEAR" "GL_NICEST" "GL_NO_XREPEAT" "GL_NO_YREPEAT" "RESIZE_INTERP1" "RESIZE_INTERP2" "RESIZE_UNSIGNED_INTERP2" "RESIZE_COLOR_INTERP1" "RESIZE_COLOR_INTERP2"))
(setq faust-sizes '("INT_SIZE" "FLOAT_SIZE" "INT_MAX" "COLORBITS" ))
(setq faust-zlib '("Z_NO_COMPRESSION" "Z_BEST_SPEED" "Z_BEST_COMPRESSION" "Z_DEFAULT_COMPRESSION" ))
(setq faust-file-formats '("FORMAT_RAW" "FORMAT_JPEG" "FORMAT_PNG" "FORMAT_GIF" "FORMAT_WAVE" "FORMAT_AIFF" "FORMAT_PIXICONTAINER" ))
(setq faust-load-save-options '("LOAD_FIRST_FRAME" ))
(setq faust-gif-saving '("GIF_GRAYSCALE" "GIF_DITHER" ))
(setq faust-jpeg-saving '("JPEG_H1V1" "JPEG_H2V1" "JPEG_H2V2" "JPEG_TWOPASS" ))
(setq faust-alignment '("TOP" "BOTTOM" "LEFT" "RIGHT" ))
(setq faust-effects '("EFF_NOISE" "EFF_SPREAD_LEFT" "EFF_SPREAD_RIGHT" "EFF_SPREAD_UP" "EFF_SPREAD_DOWN" "EFF_HBLUR" "EFF_VBLUR" "EFF_COLOR" ))
(setq faust-opengl '("GL_POINTS" "GL_LINE_STRIP" "GL_LINE_LOOP" "GL_LINES" "GL_TRIANGLE_STRIP" "GL_TRIANGLE_FAN" "GL_TRIANGLES" "GL_ZERO" "GL_ONE" "GL_SRC_COLOR" "GL_ONE_MINUS_SRC_COLOR" "GL_DST_COLOR" "GL_ONE_MINUS_DST_COLOR" "GL_SRC_ALPHA" "GL_ONE_MINUS_SRC_ALPHA" "GL_DST_ALPHA" "GL_ONE_MINUS_DST_ALPHA" "GL_SRC_ALPHA_SATURATE" ))
(setq faust-audio '("AUDIO_FLAG_INTERP2" ))
(setq faust-midi '("MIDI_PORT_READ" "MIDI_PORT_WRITE" ))
(setq faust-events '("EVT" "EVT" "EVT_TYPE" "EVT_FLAGS" "EVT_TIME" "EVT_X" "EVT_Y" "EVT_KEY" "EVT_SCANCODE" "EVT_PRESSURE" "EVT_UNICODE" "EVT_MOUSEBUTTONDOWN" "EVT_MOUSEBUTTONUP" "EVT_MOUSEMOVE" "EVT_TOUCHBEGIN" "EVT_TOUCHEND" "EVT_TOUCHMOVE" "EVT_BUTTONDOWN" "EVT_BUTTONUP" "EVT_SCREENRESIZE" "EVT_QUIT" "EVT_FLAG_SHIFT" "EVT_FLAG_CTRL" "EVT_FLAG_ALT" "EVT_FLAG_MODE" "EVT_FLAG_MODS" "EVT_FLAG_DOUBLECLICK" ))
(setq faust-events-key-codes '("KEY_MOUSE_LEFT" "KEY_MOUSE_MIDDLE" "KEY_MOUSE_RIGHT" "KEY_MOUSE_SCROLLUP" "KEY_MOUSE_SCROLLDOWN" "KEY_BACKSPACE" "KEY_TAB" "KEY_ENTER" "KEY_ESCAPE" "KEY_SPACE" "KEY_F1" "KEY_F2" "KEY_F3" "KEY_F4" "KEY_F5" "KEY_F6" "KEY_F7" "KEY_F8" "KEY_F9" "KEY_F10" "KEY_F11" "KEY_F12" "KEY_UP" "KEY_DOWN" "KEY_LEFT" "KEY_RIGHT" "KEY_INSERT" "KEY_DELETE" "KEY_HOME" "KEY_END" "KEY_PAGEUP" "KEY_PAGEDOWN" "KEY_CAPS" "KEY_SHIFT" "KEY_CTRL" "KEY_ALT" "KEY_MENU" "KEY_UNKNOWN" ))
(setq faust-events-set-quit '("QA_NONE" "QA_CLOSE_VM" ))
(setq faust-mathematical-constants '("M_E" "M_LOG2E" "M_LOG10E" "M_LN2" "M_LN10" "M_PI" "M_2_SQRTPI" "M_SQRT2" "M_SQRT1_2" ))
(setq faust-data-processing-operations '("OP_MIN" "OP_MAX" "OP_MAXABS" "OP_LIMIT_TOP" "OP_LIMIT_BOTTOM" "OP_ABS" "OP_SUB2" "OP_COLOR_SUB2" "OP_DIV2" "OP_ADD" "OP_SADD" "OP_COLOR_ADD" "OP_SUB" "OP_SSUB" "OP_COLOR_SUB" "OP_MUL" "OP_SMUL" "OP_MUL_RSHIFT15" "OP_COLOR_MUL" "OP_DIV" "OP_COLOR_DIV" "OP_AND" "OP_OR" "OP_XOR" "OP_LSHIFT" "OP_RSHIFT" "OP_EQUAL" "OP_LESS" "OP_GREATER" "OP_COPY" "OP_COPY_LESS" "OP_COPY_GREATER" "OP_BMUL" "OP_EXCHANGE" "OP_COMPARE" "OP_MUL_DIV" "OP_MUL_RSHIFT" "OP_SIN" "OP_SIN8" "OP_RAND" ))
(setq faust-sampler '("SMP_INFO_SIZE" "SMP_DEST" "SMP_DEST_OFF" "SMP_DEST_LEN" "SMP_SRC" "SMP_SRC_OFF_H" "SMP_SRC_OFF_L" "SMP_SRC_SIZE" "SMP_LOOP" "SMP_LOOP_LEN" "SMP_VOL1" "SMP_VOL2" "SMP_DELTA" "SMP_FLAGS" "SMP_FLAG_INTERP2" "SMP_FLAG_INTERP4" "SMP_FLAG_PINGPONG" "SMP_FLAG_REVERSE" ))
(setq faust-native-code-constants '("CCONV_DEFAULT" "CCONV_CDECL" "CCONV_STDCALL" "CCONV_UNIX_AMD64" "CCONV_WIN64" ))
(setq faust-constants-for-posix-compatibility '("FOPEN_MAX" "SEEK_CUR" "SEEK_END" "SEEK_SET" "EOF" "STDIN" "STDOUT" "STDERR" ))
(setq faust-various '("FAUST_VERSION" "OS_NAME" "ARCH_NAME" "CURRENT_PATH" "USER_PATH" "TEMP_PATH" "OPENGL" ))
(setq faust-built-in-global-variables '("window_xsize" "WINDOW_YSIZE" "FPS" "PPI" "UI_SCALE"))
(setq faust-constants '("ACTIVE" "AGENT" "ALL_SIDES" "ATTACH_BACK"))
(setq faust-events '("at_rot_target" "at_target" "attach"))

;; Color Faces
(defvar faust-ide-mode-colors-orange 'faust-ide-mode-colors-orange)
(defvar faust-ide-mode-colors-black 'faust-ide-mode-colors-black)
(defvar faust-ide-mode-colors-white 'faust-ide-mode-colors-white)
(defvar faust-ide-mode-colors-yellow 'faust-ide-mode-colors-yellow)
(defvar faust-ide-mode-colors-red 'faust-ide-mode-colors-red)
(defvar faust-ide-mode-colors-green 'faust-ide-mode-colors-green)
(defvar faust-ide-mode-colors-blue 'faust-ide-mode-colors-blue)

(defface faust-ide-mode-colors-orange `((t (:weight bold :background "DimGrey" :foreground "orange"))) "Orange.")
(defface faust-ide-mode-colors-black `((t (:weight bold :background "DimGrey" :foreground "black"))) "Black.")
(defface faust-ide-mode-colors-white `((t (:weight bold :background "DimGrey" :foreground "white"))) "White.")
(defface faust-ide-mode-colors-yellow `((t (:weight bold :background "DimGrey" :foreground "yellow"))) "Yellow.")
(defface faust-ide-mode-colors-red `((t (:weight bold :background "DimGrey" :foreground "red"))) "Red.")
(defface faust-ide-mode-colors-green `((t (:weight bold :background "DimGrey" :foreground "green"))) "Green.")
(defface faust-ide-mode-colors-blue `((t (:weight bold :background "DimGrey" :foreground "blue"))) "Blue.")

;; Available font-lock faces:
;; font-lock-comment-face
;; font-lock-comment-delimiter-face
;; font-lock-string-face
;; font-lock-doc-face
;; font-lock-keyword-face
;; font-lock-builtin-face
;; font-lock-function-name-face
;; font-lock-variable-name-face
;; font-lock-type-face
;; font-lock-constant-face
;; font-lock-warning-face
;; font-lock-negation-char-face
;; font-lock-preprocessor-face

(defvar faust-ide-mode-font-lock-defaults
  `((

;; Variables
     ("\\${?[#?]?\\([[:alpha:]_][[:alnum:]_]*\\|0\\)" . font-lock-variable-name-face)
     ("\\<\\([[:alnum:]_]+\\)\\(\\[.+\\]\\)?[ \t]*[-+*/%^]?=" . font-lock-variable-name-face)

;; Operators
     ("%\\|/\\| div \\|*\\|+\\|-\\|>>\\|<<\\| == \\|!=\\|<\\|>\\| <= \\| >= \\||\\|^\\| & " . font-lock-warning-face)

;; Colors
     ("ORANGE" . faust-ide-mode-colors-orange)
     ("BLACK" . faust-ide-mode-colors-black)
     ("WHITE" . faust-ide-mode-colors-white)
     ("YELLOW" . faust-ide-mode-colors-yellow)
     ("RED" . faust-ide-mode-colors-red)
     ("GREEN" . faust-ide-mode-colors-green)
     ("BLUE" . faust-ide-mode-colors-blue)

;; Faust!
     ( ,(regexp-opt faust-keywords 'words) . font-lock-builtin-face)
     ;; ( ,(regexp-opt faust-operators 'words) . font-lock-warning-face)
     ( ,(regexp-opt faust-types 'words) . font-lock-type-face)
     ( ,(regexp-opt faust-sizes 'words) . font-lock-keyword-face)
     ( ,(regexp-opt faust-containers 'words) . font-lock-constant-face)
     ( ,(regexp-opt faust-file-formats 'words) . font-lock-builtin-face)
     ( ,(regexp-opt faust-load-save-options 'words) . font-lock-builtin-face)
     ( ,(regexp-opt faust-gif-saving 'words) . font-lock-builtin-face)
     ( ,(regexp-opt faust-jpeg-saving 'words) . font-lock-builtin-face)
     ( ,(regexp-opt faust-alignment 'words) . font-lock-constant-face)
     ( ,(regexp-opt faust-effects 'words) . font-lock-constant-face)
     ( ,(regexp-opt faust-opengl 'words) . font-lock-constant-face)
     ( ,(regexp-opt faust-audio 'words) . font-lock-constant-face)
     ( ,(regexp-opt faust-midi 'words) . font-lock-constant-face)
     ( ,(regexp-opt faust-events 'words) . font-lock-builtin-face)
     ( ,(regexp-opt faust-events-key-codes 'words) . font-lock-builtin-face)
     ( ,(regexp-opt faust-events-set-quit 'words) . font-lock-builtin-face)
     ( ,(regexp-opt faust-mathematical-constants 'words) . font-lock-constant-face)
     ( ,(regexp-opt faust-data-processing-operations 'words) . font-lock-preprocessor-face)
     ( ,(regexp-opt faust-sampler 'words) . font-lock-constant-face)
     ( ,(regexp-opt faust-native-code-constants 'words) . font-lock-constant-face)
     ( ,(regexp-opt faust-constants-for-posix-compatibility 'words) . font-lock-constant-face)
     ( ,(regexp-opt faust-various 'words) . font-lock-builtin-face)
     ( ,(regexp-opt faust-built-in-global-variables 'words) . font-lock-variable-name-face)
     ( ,(regexp-opt faust-constants 'words) . font-lock-constant-face)
     ( ,(regexp-opt faust-functions 'words) . font-lock-function-name-face))))

(define-derived-mode faust-ide-mode c-mode
  "faust-ide-mode"
  "A GNU Emacs minor mode for editing Faust (http://faust.grame.fr) code."
  (setq font-lock-defaults faust-ide-mode-font-lock-defaults))

(provide 'faust-ide-mode)
