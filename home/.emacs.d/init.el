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
;;     emacsclient -c -n -a emacs
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
(when (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/elisp"))
(defalias 'yes-or-no-p 'y-or-n-p)
(when (fboundp 'delete-forward-char)
  (global-set-key [?\C-d] 'delete-forward-char))
(global-set-key [?\C-c ?\C-o] 'occur)
(global-set-key [?\C-l] 'goto-line)
(global-set-key [?\C-x ?\\] 'delete-trailing-whitespace)
(global-set-key [?\C-x ?f] 'find-file)
(global-set-key [?\C-x ?s] 'save-buffer)
(global-set-key [f3] 'isearch-repeat-forward)
(setq
 auto-save-default nil
 completion-cycle-threshold 2
 disabled-command-function nil
 display-time-24hr-format t
 display-time-day-and-date t
 enable-recursive-minibuffers t
 highlight-nonselected-windows t
 inhibit-startup-message t
 initial-major-mode 'text-mode
 initial-scratch-message nil
 make-backup-files nil
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
 completion-ignored-extensions
 '(".a" ".aux" ".bin" ".chi" ".dll" ".dylib" ".elc" ".exe" ".gch" ".hi" ".ipch"
   ".lib" ".mod" ".o" ".obj" ".pyc" ".sdf" ".so" ".stackdump" ".thm" ".toc")
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
(when (fboundp 'electric-indent-mode)
  (electric-indent-mode 0))
(set-face-attribute 'mode-line nil :box nil)
(eval-after-load "warnings"
  '(add-to-list 'warning-suppress-types '(undo discard-info)))
(condition-case err
    (load "local" t)                     ; machine-specific settings
  ((debug error)
   (display-warning '(local.el) (error-message-string err) :error)))

;; Dump all the auto-save files into a temporary directory
(when make-backup-files
  (let ((backup-dir "~/.emacs.d/backups/")
        (max-age (* 60 60 24 365)))     ; Purge backups older than this
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
        (delete-file file)))))

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

;; Buffer management
(global-set-key [C-tab] 'next-buffer)
(global-set-key [C-backtab] 'previous-buffer)
(global-set-key [C-S-tab] [C-backtab])
(global-set-key [C-S-iso-lefttab] [C-backtab])
(global-set-key [f4] 'save-buffer)
(global-set-key [f5] 'revert-buffer)
(icomplete-mode 1)
(add-hook 'ibuffer-mode-hooks
          '(lambda () (setq ibuffer-display-summary nil)))
;; try to avoid buffers whose name satisfies this pattern;
;; note: the extra space is necessary because they occur in the names of
;; hidden buffers that can't ever be reached using (next-buffer)
(defvar ignored-buffer-name-pattern "\\` *\\*.*\\* *\\'")
(defun have-only-ignored-buffers (buffers)
  (or (null buffers)
      (and (string-match ignored-buffer-name-pattern
                         (buffer-name (car buffers)))
           (have-only-ignored-buffers (cdr buffers)))))
(defun custom-next-buffer ()
  (interactive)
  (next-buffer)
  (unless (have-only-ignored-buffers (buffer-list))
    (while (string-match ignored-buffer-name-pattern (buffer-name))
      (next-buffer))))
(global-set-key [remap next-buffer] 'custom-next-buffer)
(defun custom-previous-buffer ()
  (interactive)
  (previous-buffer)
  (unless (have-only-ignored-buffers (buffer-list))
    (while (string-match ignored-buffer-name-pattern (buffer-name))
      (previous-buffer))))
(global-set-key [remap previous-buffer] 'custom-previous-buffer)

;; Miscellaneous control functions
(defun goto-col (col-number)
  "Go to the given column number, padding with space if needed."
  (interactive "nColumn number: ")
  (beginning-of-line)
  (let ((number col-number))
    (while (> number 0)
      (if (eolp)
          (insert " ")
        (forward-char))
      (setq number (1- number)))))
(global-set-key (kbd "C-=") 'goto-col)
(defun delete-horizontal-space-backward ()
  "Delete everything from the point to the previous non-whitespace char."
  (interactive)
  (delete-horizontal-space t))
(global-set-key (kbd "M-S-<backspace>") 'delete-horizontal-space-backward)
(defun delete-horizontal-space-forward ()
  "Delete everything from the point to the next non-whitespace char."
  (interactive)
  (let ((orig-pos (point)))
    (delete-region
     (progn
       (skip-chars-forward " \t")
       (constrain-to-field nil orig-pos t))
     orig-pos)))
(global-set-key [?\M-\]] 'delete-horizontal-space-forward)
(defun backward-whitespace ()
  "Move point to the beginning of the current sequence of whitespace chars."
  (interactive)
  (forward-whitespace -1))
(global-set-key [?\M-p] 'backward-whitespace)
(global-set-key [?\M-n] 'forward-whitespace)
(defun indent-region-offset (offset)
  (if mark-active
      (let ((deactivate-mark nil))
        (save-excursion
          (indent-rigidly (region-beginning) (region-end) offset)))))
(defun indent-region-left ()
  "Shift the indentation of the region by one character to the left."
  (interactive)
  (indent-region-offset -1))
(global-set-key (kbd "M-,") 'indent-region-left)
(defun indent-region-right ()
  "Shift the indentation of the region by one character to the right."
  (interactive)
  (indent-region-offset 1))
(global-set-key (kbd "M-.") 'indent-region-right)

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
    (set-buffer-modified-p nil)
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
(defun copy-buffer ()
  (interactive)
  (save-excursion
    (kill-ring-save (point-min) (point-max))
    (message "Buffer copied.")))
(global-set-key (kbd "C-x w") 'copy-buffer)

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

;; Library for guessing indentation style
(autoload 'guess-style-set-variable "guess-style" nil t)
(autoload 'guess-style-guess-variable "guess-style")
(autoload 'guess-style-guess-all "guess-style" nil t)

(defun load-melpa ()
  (interactive)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/"))
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
  (package-initialize)
  (package-refresh-contents))

(defun install-all-packages ()
  (interactive)
  (load-melpa)
  (ignore-errors (package-install 'markdown-mode))
  (ignore-errors (package-install 'rainbow-mode))
  (ignore-errors (package-install 'yaml-mode)))

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

;; Agda
(ignore-errors
  (require 'agda-input)
  (setq agda-input-user-translations
        '(("Real" "ℝ")
          ("Reals" "ℝ")
          ("Cplx" "ℂ")
          ("Complex" "ℂ")
          ("Int" "ℤ")
          ("Integer" "ℤ")
          ("Integers" "ℤ")
          ("Nat" "ℕ")
          ("Natural" "ℕ")
          ("Naturals" "ℕ")
          ("PI" "ℼ")
          ("pi" "ℼ")
          ("I" "ⅈ")
          ("i" "ⅈ")
          ("D" "ⅆ")
          ("d" "ⅆ")
          ("E" "ⅇ")
          ("e" "ⅇ")
          ;; the ones by agda are the wrong brackets
          ("langle" "⟨")
          ("rangle" "⟩")
          ))
  ;; cause changes to take effect
  (agda-input-setup))

;; C/C++
(add-to-list 'auto-mode-alist           ; `c-mode` is buggy so avoid it
             '("\\.\\(c\\|h\\|inl\\)\\'" . c++-mode))
(setq-default
 c-basic-offset 4
 c-require-final-newline '())
(defvar c-offsets-alist)
(setq c-offsets-alist
      '((arglist-intro . +)
        (arglist-close . 0)
        (inextern-lang . 0)
        (innamespace . 0)))
(add-hook 'c++-mode-hook
          '(lambda () (c-toggle-electric-state -1)) t)

;; CSS
(add-to-list 'auto-mode-alist '("\\.\\(le\\|sc\\)ss\\'" . css-mode))

;; Git commit and tag messages
(add-to-list 'auto-mode-alist
             '("COMMIT_EDITMSG" . (lambda () (setq fill-column 72))))
(add-to-list 'auto-mode-alist
             '("TAG_EDITMSG"    . (lambda () (setq fill-column 72))))

;; Gnuplot
(autoload 'gnuplot-mode "gnuplot" nil t)
(add-to-list 'auto-mode-alist
             '("\\.\\(plt\\|gp\\|gnuplot\\)\\'" . gnuplot-mode))

;; Haskell
(autoload 'haskell-mode "haskell-mode-autoloads" nil t)
(add-to-list 'auto-mode-alist '("\\.\\(c?hs\\|hsc\\)\\'" . haskell-mode))
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; JavaScript
(add-to-list 'auto-mode-alist '("\\.\\(g\\|j\\)s\\'" . js-mode))

;; LaTeX
(eval-after-load "tex-mode"
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
    (local-set-key
     [f6]
     '(lambda ()
        (interactive)
        (save-buffer)
        (TeX-command "LaTeX" 'TeX-master-file)))))
(add-hook
 'bibtex-mode-hook
 '(lambda () (local-set-key [f6] 'bibtex-reformat)))

;; Lua
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
(autoload 'lua-mode "lua-mode" nil t)
(add-hook 'lua-mode-hook 'hs-minor-mode)
(setq lua-indent-level 4)

;; Markdown
(autoload 'markdown-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.m\\(d\\|arkdown\\)\\'" . markdown-mode))

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
(autoload 'python-mode "python" nil t)
(add-hook 'python-mode-hook 'guess-style-guess-all)
(add-to-list 'auto-mode-alist '("\\.pyw?\\'" . python-mode))
(add-to-list 'magic-mode-alist '("^#!/.*[jp]ython[.0-9]*$" . python-mode))

;; R
(autoload 'r-mode "ess-site" nil t)
(add-to-list 'auto-mode-alist '("\\.r\\'" . r-mode))
(eval-after-load "ess-site" '(ess-toggle-underscore nil))

;; Rust
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; Scheme
(add-to-list 'auto-mode-alist '("\\.ctl\\'" . scheme-mode))

;; Shell
(add-to-list 'auto-mode-alist
             '("\\([/\]PKGBUILD\\|\\.install\\)\\'" . shell-script-mode))

;; YAML
(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))

;; Z3
(add-to-list 'auto-mode-alist '("\\.z3\\'" . lisp-mode))

;; ---------------------------------------------------------------------------
;;
;; Visuals
;; =======

;; Rainbow (minor) mode
(autoload 'rainbow-mode "rainbow-mode" nil t)
(add-to-list 'safe-local-variable-values '(rainbow-mode))

;; Theme settings
(defadvice load-theme
  (before theme-dont-propagate activate)
  (mapcar #'disable-theme custom-enabled-themes))

;; Sets the family and height of the specified face.  If the face exists,
;; `non-nil` is returned.
(defun set-face (face family height)
  (when (member family (font-family-list))
    (set-face-attribute face nil :family family)
    (when (boundp 'height) (set-face-attribute face nil :height height)) t))

;; Opacity
(defun alpha-modify (change)
  "Modify the opacity of the frame by `change`%."
  (set-frame-parameter
   nil 'alpha
   (min 100 (max frame-alpha-lower-limit
                 (+ change (or (frame-parameter nil 'alpha) 100))))))
(defun alpha-increase ()
  "Increase the opacity of the frame by 5%."
  (interactive)
  (alpha-modify 5))
(defun alpha-decrease ()
  "Decrease the opacity of the frame by 5%."
  (interactive)
  (alpha-modify -5))

;; Face height adjustments
(defun face-height-multiply (face factor)
  (set-face-attribute
   face nil :height (round (* factor (face-attribute face :height)))))
(defun face-height-increase ()
  "Increase the face height by 11%."
  (interactive)
  (face-height-multiply 'default (/ 1 0.9)))
(defun face-height-decrease ()
  "Decrease the face height by 10%."
  (interactive)
  (face-height-multiply 'default 0.9))

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
(defun face-envy ()
  (interactive)
  (set-face 'default "Envy Code R" 130))
(defun face-fira ()
  (interactive)
  (set-face 'default "Fira Mono" 150))
(defun face-monoid ()
  (interactive)
  (set-face 'default "Monoid" 140))
(defun face-oxygen ()
  (interactive)
  (set-face 'default "Oxygen Mono" 130))
(defun face-inconsolata ()
  (interactive)
  (set-face 'default "Inconsolata" 140))
(defun face-ubuntu ()
  (interactive)
  (set-face 'default "Ubuntu Mono" 140))

(setq custom-safe-themes t)
(defvar my-theme 'solarized-custom)

;; these settings only apply when more than 256 colors are supported; the
;; problem is that in 256-colors, the default emacs theme will mix the console
;; colors and absolute colors, which can result in unreadable text when the
;; console colors use a dark background; Emacs always assumes that
;; xterm-256color and rxvt-unicode-256color have a light background; currently
;; the workaround is to load a specific theme that doesn't use any console
;; colors; unfortunately, this adds quite a bit of loading time
;;
;; also because emacs has really bad support for lexical closures, a dynamic
;; function is defined instead to avoid unnecessary complications
(defun rf-initialize-colors (gui)
  (when gui

    ;; disabled to prevent locking up the GUI
    (defun iconify-frame ())

    ;; font face
    (set-fontset-font "fontset-default" 'unicode "STIXGeneral")
    (or (face-monoid)
        (face-droid)
        (face-envy)
        (face-fira)
        (face-oxygen)
        (face-inconsolata)
        (face-ubuntu)
        (face-dejavu)
        (face-consolas))

    ;; transparency
    (global-set-key (kbd "C-M-_") 'alpha-increase)
    (global-set-key (kbd "C-M-+") 'alpha-decrease)
    (global-set-key (kbd "C--") 'face-height-decrease)
    (global-set-key (kbd "C-=") 'face-height-increase)
    (set-frame-parameter nil 'alpha 95))

  (global-hl-line-mode)
  (setq ansi-color-names-vector
        [unspecified "#3f3f3f" "#cc9393" "#7f9f7f"
                     "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])
  (ignore-errors (load-theme my-theme)))

(if (display-graphic-p)
    (rf-initialize-colors t)
  ;; we need to wait until (tty-color-alist) is fully populated
  (add-hook
   'term-setup-hook
   (lambda ()
     (when (>= (length (tty-color-alist)) 256)
       (setq frame-background-mode 'dark)))))
