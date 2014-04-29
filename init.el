;; ===============
;; Emacs init file
;; ===============
;;
;; This file was created on 2011-03-14 by Rufflewind and has been placed in
;; the public domain.  Feel free to use any part of this code for whatever
;; purposes.  The latest version can be found at:
;;
;;     http://github.com/Rufflewind/emacs.d
;;
;;
;; Byte-compilation
;; ----------------
;;
;; The files under `~/.emacs.d/` may be byte-compiled to `.elc` files to speed
;; up the loading process.  To do this, type `C-x C-e` at the end of the
;; following line of code:
;;
;;     (byte-recompile-directory "~/.emacs.d/" 0)
;;
;;
;; Starting an Emacs client
;; ========================
;;
;; The following Windows command attempts to connect to an existing Emacs
;; server if available, or start a new server if none are available.  Insert
;; absolute paths if necessary:
;;
;;   Target:      emacsclientw -d DISPLAY:0.0 -c -n -a runemacs
;;   Start in:    %HOME%
;;
;; The equivalent on Linux:
;;
;;     $ emacsclient -c -n -a emacs
;;
;;
;; ---------------------------------------------------------------------------
;;
;; Basics
;; ======

(when (and (display-graphic-p)
           (require 'server nil 'noerror)
           (fboundp 'server-running-p))
  (when (eq (server-running-p) :other) (server-force-delete))
  (unless (server-running-p) (server-start)))
(add-to-list 'load-path "~/.emacs.d/elisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/elisp")
(defalias 'yes-or-no-p 'y-or-n-p)
(global-set-key [?\C-d] 'delete-forward-char)
(global-set-key [?\C-c ?\C-o] 'occur)
(global-set-key [?\C-l] 'goto-line)
(global-set-key [?\C-x ?\\] 'delete-trailing-whitespace)
(global-unset-key [C-x f])
(setq
 completion-cycle-threshold 2
 disabled-command-function nil
 display-time-24hr-format t
 display-time-day-and-date t
 enable-recursive-minibuffers t
 highlight-nonselected-windows t
 inhibit-startup-message t
 initial-major-mode 'text-mode
 initial-scratch-message nil
 max-lisp-eval-depth 6000
 mode-require-final-newline nil
 mouse-wheel-progressive-speed nil
 ring-bell-function 'ignore
 save-abbrevs nil
 scalable-fonts-allowed t
 standard-indent 4
 tab-width 4
 truncate-partial-width-windows nil
 vc-handled-backends nil)
(setq-default
 buffer-file-coding-system 'utf-8-unix
 comment-column 40
 completion-ignored-extensions '(".o" ".elc" ".bin")
 fill-column 78
 indent-tabs-mode nil
 major-mode 'text-mode
 revert-without-query '("")
 truncate-lines nil
 word-wrap t)
(add-hook
 'before-save-hook
 'time-stamp)                           ; Switch: 'time-stamp-toggle-active'
(add-hook
 'shell-mode-hook
 'ansi-color-for-comint-mode-on)
(column-number-mode t)
(show-paren-mode t)
(electric-indent-mode 0)
(set-face-attribute 'mode-line nil :box nil)

;; Dump all the auto-save files into a temporary directory
(let ((backup-dir (concat (file-name-as-directory temporary-file-directory)
                          "emacs/saves/"))
      (max-age (* 60 60 24 7 60)))      ; Purge backups older than this
  (make-directory backup-dir t)
  (setq
   auto-save-file-name-transforms `((".*" ,backup-dir t))
   backup-by-copying t
   backup-directory-alist `((".*" . ,backup-dir))
   delete-old-versions t
   kept-new-versions 7
   kept-old-versions 3
   version-control t)
  (dolist (file (directory-files backup-dir t))
    (when (and
           (backup-file-name-p file)
           (> (- (float-time (current-time))
                 (float-time (nth 5 (file-attributes file)))) max-age))
      (delete-file file))))

;; Automatically byte-compile the file after saving an Emacs Lisp file,
;; provided that the corresponding `*.elc` already exists.
(add-hook
 'after-save-hook
 (lambda ()
   (if (eq major-mode 'emacs-lisp-mode)
       (save-excursion
         (when (file-exists-p
                (concat (file-name-sans-extension buffer-file-name) ".elc"))
           (byte-compile-file buffer-file-name))))))

;; Frame
(when (display-graphic-p)
  (defun set-width (w)
    (interactive "nWidth: ")
    (set-frame-width (selected-frame) w))
  (defun set-height (h)
    (interactive "nHeight: ")
    (set-frame-height (selected-frame) h))
  (add-to-list 'default-frame-alist '(width . 81))
  (add-to-list 'default-frame-alist '(height . 30)))

;; Hide clutter
(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(when (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

;; Encoding
(setq locale-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; Window dedication
(defun toggle-current-window-dedication ()
  (interactive)
  (let* ((window (selected-window)) (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "") (buffer-name))))
(global-set-key [pause] 'toggle-current-window-dedication)

;; Make buffer names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-trailing-separator-p t)

;; Save place in files
(require 'saveplace)
(setq save-place-file "~/.emacs.d/places")
(setq-default save-place t)

;; Buffer management
(global-set-key [C-tab] 'next-buffer)
(global-set-key [C-backtab] 'previous-buffer)
(global-set-key [C-S-tab] [C-backtab])
(global-set-key [C-S-iso-lefttab] [C-backtab])
(global-set-key [f5] 'revert-buffer)
(icomplete-mode 1)
(add-hook 'ibuffer-mode-hooks
          '(lambda () (setq ibuffer-display-summary nil)))

;; Go to a certain column number (padding with space as needed)
(defun goto-col (col-number)
  "Go to a column number within the current line."
  (interactive "nColumn number: ")
  (beginning-of-line)
  (let ((number col-number))
    (while (> number 0)
      (if (eolp)
          (insert " ")
        (forward-char))
      (setq number (1- number)))))
(global-set-key (kbd "C-=") 'goto-col)

;; Kill the buffer and remove the associated file.
(defun delete-buffer-and-file ()
  "Kill the buffer and remove the associated file."
  (interactive)
  (when (yes-or-no-p "Kill buffer and remove associated file (if any)? ")
    (let ((filename (buffer-file-name)))
      (condition-case err
          (when (and filename (file-exists-p filename))
            (delete-file filename)
            (message "Successfully removed '%s'." filename))
        (message err)))
    (not-modified)
    (kill-buffer (current-buffer))))

;; Find non-ASCII characters.
(defun find-non-ascii-char ()
  "Find non-ASCII characters."
  (interactive)
  (occur "[[:nonascii:]]+"))

;; Clipboard fixes
(delete-selection-mode t)
(setq kill-do-not-save-duplicates t
      mouse-drag-copy-region nil
      save-interprogram-paste-before-kill t
      x-select-enable-clipboard t)

;; Window movement
(global-set-key [C-S-right] 'windmove-right)
(global-set-key [C-S-left] 'windmove-left)
(global-set-key [C-S-up] 'windmove-up)
(global-set-key [C-S-down] 'windmove-down)
(defun delete-and-kill-other-window ()
  "Close the other pane and kill the buffer in it also."
  (interactive)
  (other-window 1)
  (kill-buffer)
  (delete-window))
(defun delete-and-kill-this-window ()
  "Close this pane and kill the buffer in it also."
  (interactive)
  (kill-buffer)
  (delete-window))
(global-set-key (kbd "C-x M-1") 'delete-and-kill-other-window)
(global-set-key (kbd "C-x M-0") 'delete-and-kill-this-window)

;; Scrolling
(defun scroll-down-1 ()
  (interactive)
  (scroll-down 1))
(defun scroll-up-1 ()
  (interactive)
  (scroll-up 1))
(global-set-key [M-up] 'scroll-down-1)
(global-set-key [M-down] 'scroll-up-1)

;; Printing
(declare-function htmlfontify-buffer "htmlfontify")
(defun preview-buffer () (interactive)
  (let ((temp-file (make-temp-file "preview-buffer" nil ".htm")))
    (htmlfontify-buffer)
    (write-file temp-file)
    (browse-url-of-file temp-file)
    (kill-buffer)))

;; Smoother scrolling
(setq
 mouse-wheel-scroll-amount '(5 ((shift) . 1) ((control)))
 scroll-step 1)

;; Print to PDF file (only works on Linux)
(defun print-to-pdf ()
  (interactive)
  (ps-spool-buffer-with-faces)
  (switch-to-buffer "*PostScript*")
  (write-file "/tmp/tmp.ps")
  (kill-buffer "tmp.ps")
  (shell-command (concat "ps2pdf14 /tmp/tmp.ps " (buffer-name) ".pdf"))
  (shell-command "rm /tmp/tmp.ps")
  (message (concat "Saved to:  " (buffer-name) ".pdf")))

;; ---------------------------------------------------------------------------
;;
;; Modes
;; =====
;;
;; Avoid loading the mode libraries directly via `load`, `load-file`, or
;; `require`.  Instead, use `autoload` to load the file lazily (when a
;; function is called) and perform the initialization procedure within
;; `eval-after-load`.  The syntax for `autoload` is:
;;
;;     (autoload '<function-name> "<file-name>" nil t)
;;
;; The `t` indicates that the function can be called interactively, and the
;; filename can be specified without an extension.

;; C/C++
(add-to-list 'auto-mode-alist           ; `c-mode` is buggy so avoid it
             '("\\.\\(c\\|h\\|inl\\)$" . c++-mode))
(setq-default
 c-basic-offset 4
 c-require-final-newline '())
(defvar c-offsets-alist)
(setq c-offsets-alist
      '((arglist-intro . +)
        (arglist-close . 0)
        (inextern-lang . 0)
        (innamespace . 0)))
(add-hook
 'c++-mode-hook
 '(lambda ()
    (c-toggle-electric-state -1)
    ;; We could place some regexes into `c-mode-common-hook', but note that
    ;; their evaluation order matters.
    (font-lock-add-keywords
     nil
     `(;; complete some fundamental keywords
       (,(concat
          "\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long"
          "\\|float\\|double\\)\\>")
        . font-lock-keyword-face)
       ;; namespace names and tags - these are rendered as constants by cc-mode
       ("\\<\\(\\w+::\\)" . font-lock-function-name-face)
       ;;  new C++11 keywords
       (,(concat
          "\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr"
          "\\|static_assert\\|thread_local\\|override\\|final\\)\\>")
        . font-lock-keyword-face)
       ("\\<\\(char16_t\\|char32_t\\)\\>" . font-lock-keyword-face)
       ;; PREPROCESSOR_CONSTANT, PREPROCESSORCONSTANT
       ("\\<[A-Z]*_[A-Z_]+\\>" . font-lock-constant-face)
       ("\\<[A-Z]\\{3,\\}\\>"  . font-lock-constant-face)
       ;; hexadecimal numbers
       ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
       ;; integer/float/scientific numbers
       ("\\<[\\-+]*[0-9]*\\.?[0-9]+\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?\\>"
        . font-lock-constant-face)
       ;; c++11 string literals
       ;;       L"wide string"
       ;;       L"wide string with UNICODE codepoint: \u2018"
       ;;       u8"UTF-8 string", u"UTF-16 string", U"UTF-32 string"
       ("\\<\\([LuU8]+\\)\".*?\"" 1 font-lock-keyword-face)
       ;;       R"(user-defined literal)"
       ;;       R"( a "quot'd" string )"
       ;;       R"delimiter(The String Data" )delimiter"
       ;;       R"delimiter((a-z))delimiter" is equivalent to "(a-z)"
       ("\\(\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(\\)"
        1 font-lock-keyword-face t) ; start delimiter
       (,(concat
          "\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}"
          "(\\(.*?\\))[^\\s-\\\\()]\\{0,16\\}\"")
        1 font-lock-string-face t)  ; actual string
       (,(concat "\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}"
                 "(.*?\\()[^\\s-\\\\()]\\{0,16\\}\"\\)")
        1 font-lock-keyword-face t) ; end delimiter
       ;; user-defined types (rather project-specific)
       ("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(type\\|ptr\\)\\>" . font-lock-type-face)
       ("\\<\\(xstring\\|xchar\\)\\>" . font-lock-type-face)))) t)

;; Git commit messages
(add-to-list 'auto-mode-alist
             '("COMMIT_EDITMSG" . (lambda () (setq fill-column 72))))

;; Gnuplot
(autoload 'gnuplot-mode "gnuplot" nil t)
(add-to-list 'auto-mode-alist '("\\.\\(plt\\|gp\\|gnuplot\\)$" . gnuplot-mode))

;; Haskell
(ignore-errors (require 'haskell-mode-autoloads)) ; noerror doesn't work
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; JavaScript
(add-to-list 'auto-mode-alist '("\\.\\(g\\|j\\)s$" . js-mode))

;; LaTeX
(eval-after-load 'TeX-mode
  '(progn
     (defvar LaTeX-math-menu-unicode)
     (and (load "auctex.el" t)
          (load "preview-latex.el" t))
     (setq LaTeX-math-menu-unicode t)
     (setq-default TeX-master t)))
(add-hook
 'TeX-mode-hook
 '(lambda ()
    (setq preview-image-type 'pnm)
    (TeX-PDF-mode t)
    (local-unset-key [C-j])
    (local-set-key [f6] 'TeX-command-master)))

;; LESS CSS
(add-to-list 'auto-mode-alist '("\\.\\(le\\|c\\|sc\\)ss$" . css-mode))

;; Lua
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(autoload 'lua-mode "lua-mode" nil t)
(add-hook 'lua-mode-hook 'hs-minor-mode)

;; Markdown
(autoload 'markdown-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.m\\(d\\|arkdown\\)$" . markdown-mode))

;; Python
(defadvice python-calculate-indentation (around outdent-closing-brackets)
  "Fix python indentation for lines that end with an opening bracket."
  (save-excursion
    (beginning-of-line)
    (let ((syntax (syntax-ppss)))
      (if (and (not (eq 'string (syntax-ppss-context syntax)))
               (python-continuation-line-p)
               (cadr syntax)
               (skip-syntax-forward "-")
               (looking-at "\\s)"))
          (progn
            (forward-char 1)
            (ignore-errors (backward-sexp))
            (setq ad-return-value (current-indentation)))
        ad-do-it))))
(ad-activate 'python-calculate-indentation)

;; Rust
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))

;; Scheme
(add-to-list 'auto-mode-alist '("\\.ctl$" . scheme-mode))

;; YAML
(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;; ---------------------------------------------------------------------------
;;
;; Visuals
;; =======

;; Rainbow (minor) mode
(autoload 'rainbow-mode "rainbow-mode" nil t)
(add-to-list 'safe-local-variable-values '(rainbow-mode))

;; Theme settings
(setq ansi-color-names-vector
      [unspecified "#3f3f3f" "#cc9393" "#7f9f7f"
                   "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"]
      custom-safe-themes t)
(defadvice load-theme
  (before theme-dont-propagate activate)
  (mapcar #'disable-theme custom-enabled-themes))

;; Sets the family and height of the specified face.  If the face exists,
;; `non-nil` is returned.
(defun set-face (face family height)
  (when (member family (font-family-list))
    (set-face-attribute face nil :family family)
    (when (boundp 'height) (set-face-attribute face nil :height height)) t))

;; Fonts need to be defined outside `when` to stop Emacs from complaining
(defun face-consolas ()
  (interactive)
  (set-face 'default "Consolas" 120))
(defun face-droid ()
  (interactive)
  (set-face 'default "Droid Sans Mono" 120))
(defun face-dejavu ()
  (interactive)
  (or (set-face 'default "DejaVu Sans Mono" 110)
      (set-face 'default "Bitstream Vera Sans Mono" 120)))
(defun face-oxygen ()
  (interactive)
  (set-face 'default "Oxygen Mono" 140))
(defun face-inconsolata ()
  (interactive)
  (set-face 'default "Inconsolata" 140))
(defun face-ubuntu ()
  (interactive)
  (set-face 'default "Ubuntu Mono" 170))

(when (display-graphic-p)               ; Non-terminals only

  ;; Disable this since it can freeze up the GUI
  (defun iconify-frame ())

  ;; Just never really works well in terminals
  (global-hl-line-mode)

  ;; Face settings
  (set-fontset-font "fontset-default" 'unicode "STIXGeneral")
  (or (face-oxygen)
      (face-inconsolata)
      (face-ubuntu)
      (face-droid)
      (face-dejavu)
      (face-consolas))

  ;; Opacity
  (defun modify-alpha (change)
    "Modify the opacity of the frame by `change`%."
    (set-frame-parameter
     nil 'alpha
     (min 100 (max frame-alpha-lower-limit
                   (+ change (or (frame-parameter nil 'alpha) 100))))))
  (global-set-key (kbd "C-M-_") '(lambda () (interactive) (modify-alpha 5)))
  (global-set-key (kbd "C-M-+") '(lambda () (interactive) (modify-alpha -5)))
  (modify-all-frames-parameters '((alpha . 95)))

  ;; Don't load this in terminals because it's incredibly slow.
  (ignore-errors
    (load-theme 'tango-dark-custom)
    ()))
