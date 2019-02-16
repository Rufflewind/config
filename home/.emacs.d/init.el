;; ===============
;; Emacs init file
;; ===============
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
(add-to-list 'load-path "~/.emacs.d/elisp/haskell-mode")
(add-to-list 'load-path "/usr/share/hindent/elisp")
(when (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/elisp")
  (add-to-list 'custom-theme-load-path
               "~/.emacs.d/elisp/emacs-color-themes/themes"))
(defalias 'yes-or-no-p 'y-or-n-p)
(when (fboundp 'delete-forward-char)
  (global-set-key (kbd "C-d") 'delete-forward-char))
(global-set-key (kbd "C-c C-o") 'occur)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-x \\") 'delete-trailing-whitespace)
(global-set-key (kbd "C-x f") 'find-file)
(global-set-key (kbd "C-x s") 'save-buffer)
(global-set-key (kbd "C-M--") 'undo-only)
(global-set-key (kbd "<f2>") 'recompile)
(global-set-key (kbd "S-<f2>") 'compile)
(global-set-key (kbd "<f3>") 'isearch-forward)
(global-set-key (kbd "M-<f3>") 'isearch-forward-regexp)
(global-unset-key (kbd "C-h n"))
(global-unset-key (kbd "C-x C-k RET"))
(setq
 auto-save-default nil
 completion-cycle-threshold 2
 disabled-command-function nil
 display-time-24hr-format t
 display-time-day-and-date t
 enable-recursive-minibuffers t
 frame-title-format "%b"
 highlight-nonselected-windows t
 inhibit-startup-message t
 initial-major-mode 'text-mode
 initial-scratch-message nil
 mark-even-if-inactive nil
 make-backup-files nil
 max-lisp-eval-depth 6000
 mode-require-final-newline nil
 mouse-wheel-progressive-speed nil
 org-startup-truncated nil
 ring-bell-function 'ignore
 safe-local-variable-values '(((buffer-file-coding-system . utf-8-unix)
                               (rainbow-mode)))
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
(column-number-mode)
(show-paren-mode)
(when (fboundp 'electric-indent-mode)
  (electric-indent-mode 0))
(set-face-attribute 'mode-line nil :box nil)
(eval-after-load "warnings"
  '(add-to-list 'warning-suppress-types '(undo discard-info)))
(condition-case err
    (load "~/.emacs.d/local.el" t)      ; machine-specific settings
  ((debug error)
   (display-warning '(local.el) (error-message-string err) :error)))

;; Dump all the auto-save files into a temporary directory
(when make-backup-files
  (let ((backup-dir "~/.emacs.d/backups/")
        (max-age (* 60 60 24 365)))     ; Purge backups older than this
    (make-directory backup-dir t)
    (setq
     auto-save-default nil
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
(global-set-key (kbd "<pause>") 'toggle-current-window-dedication)

;; Make buffer names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-trailing-separator-p t)

;; Save place in files
(when make-backup-files
  (require 'saveplace)
  (setq save-place-file "~/.emacs.d/places"))

;; Buffer management
(global-set-key (kbd "C-<tab>") 'next-buffer)
(global-set-key (kbd "C-<backtab>") 'previous-buffer)
(global-set-key (kbd "C-S-<tab>") (kbd "C-<backtab>"))
(global-set-key (kbd "C-S-<iso-lefttab>") (kbd "C-<backtab>"))
(global-set-key (kbd "<f4>") 'save-buffer)
(global-set-key (kbd "<f5>") 'revert-buffer)
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
(global-set-key (kbd "M-]") 'delete-horizontal-space-forward)
(defun backward-whitespace ()
  "Move point to the beginning of the current sequence of whitespace chars."
  (interactive)
  (forward-whitespace -1))
(global-set-key (kbd "M-p") 'backward-whitespace)
(global-set-key (kbd "M-n") 'forward-whitespace)
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
(eval-after-load "js" '(define-key js-mode-map (kbd "M-.") nil))
(global-set-key (kbd "<f8>") 'sgml-tag)

;; Kill the buffer and remove the associated file.
(defun delete-buffer-and-file ()
  "Kill the buffer and remove the associated file if it exists."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when (yes-or-no-p
           (if filename
               (format "Kill buffer and remove '%s' if it exists? " filename)
             "Kill buffer? "))
      (when filename
        ;; make sure the filename hasn't changed during the prompt
        (let ((filename-new (buffer-file-name)))
          (when (not (string= filename filename-new))
            (error "Aborted (filename has changed).")))
        (condition-case err
            (when (file-exists-p filename)
              (delete-file filename)
              (message "Removed '%s'." filename))
          (message err)))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))))

;; Find non-ASCII characters.
(defun find-non-ascii-char ()
  "Find non-ASCII characters."
  (interactive)
  (occur "[[:nonascii:]]+"))

(defun chmodx ()
  "Change the current buffer file executable."
  (interactive)
  (with-demoted-errors "Unable to make file executable: %s"
    (let* ((current-mode (file-modes (buffer-file-name)))
           (add-mode (logand ?\111 (default-file-modes))))
      (or (/= (logand ?\111 current-mode) 0)
          (zerop add-mode)
          (set-file-modes (buffer-file-name)
                          (logior current-mode add-mode))))))

;; Clipboard fixes
(delete-selection-mode)
(setq kill-do-not-save-duplicates t
      mouse-drag-copy-region nil
      save-interprogram-paste-before-kill t
      x-select-enable-clipboard t)

;; Window movement
(global-set-key (kbd "C-S-<right>") 'windmove-right)
(global-set-key (kbd "C-S-<left>") 'windmove-left)
(global-set-key (kbd "C-S-<up>") 'windmove-up)
(global-set-key (kbd "C-S-<down>") 'windmove-down)
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
(global-set-key (kbd "M-<up>") 'scroll-down-1)
(global-set-key (kbd "M-<down>") 'scroll-up-1)

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

;; Tramp: use 'sshx' to force /bin/sh (it doesn't work with zsh prompts)
(setq tramp-default-method "sshx")

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

;; C/C++
(add-to-list 'auto-mode-alist           ; `c-mode` is buggy so avoid it
             '("\\.\\(c\\|h\\|inl\\)\\'" . c++-mode))
(setq-default
 c-default-style "linux"
 c-basic-offset 4
 c-block-comment-prefix ""
 c-require-final-newline '())
(defvar c-offsets-alist)
(setq c-offsets-alist
      '((arglist-intro . +)
        (arglist-close . 0)
        (inextern-lang . 0)
        (inline-open . 0)
        (innamespace . 0)
        (member-init-intro . +)))

;; Fix lambda indentation
;; http://stackoverflow.com/a/38262812
(defun my-c++-looking-at-lambda-as-param ()
  "Return t if text after point matches '[...](' or '[...]{'"
  (looking-at ".*[,(][ \t]*\\[[^]]*\\][ \t]*[({][^}]*?[ \t]*[({][^}]*?$"))
(defun my-c++-looking-at-lambda-in-uniform-init ()
  "Return t if text after point matches '{[...](' or '{[...]{'"
  (looking-at ".*{[ \t]*\\[[^]]*\\][ \t]*[({][^}]*?[ \t]*[({][^}]*?$"))
(defun my-c++-indentation-examine (langelem looking-at-p)
  (and (equal major-mode 'c++-mode)
       (ignore-errors
         (save-excursion
           (goto-char (c-langelem-pos langelem))
           (funcall looking-at-p)))))
(defadvice c-lineup-arglist (around my activate)
  "Improve indentation of continued C++11 lambda function opened as argument."
  (setq ad-return-value
        (if (my-c++-indentation-examine
             langelem
             #'my-c++-looking-at-lambda-as-param)
            0
          ad-do-it)))
(defun my-c++-lambda-indentation ()
  (c-set-offset
   'block-close
   (lambda (langelem)
     (if (my-c++-indentation-examine
          langelem
          #'my-c++-looking-at-lambda-in-uniform-init)
         '-
       0)))
  (c-set-offset
   'statement-block-intro
   (lambda (langelem)
     (if (my-c++-indentation-examine
          langelem
          #'my-c++-looking-at-lambda-in-uniform-init)
         0
       '+))))

(add-hook
 'c++-mode-hook
 '(lambda ()
    (c-toggle-electric-state -1)
    (my-c++-lambda-indentation)

    ;; We could place some regexes into `c-mode-common-hook', but note that
    ;; their evaluation order matters.
    (font-lock-add-keywords
     nil
     `((,(concat
          "\\<\\(_Alignas\\|alignas\\|_Alignof\\|alignof\\|constexpr\\|"
          "decltype\\|final\\|_Generic\\|mutable\\|noexcept\\|_Noreturn\\|"
          "_Pragma\\|restrict\\|_Static_assert\\|static_assert\\|"
          "_Thread_local\\|thread_local\\|override\\)\\>")
        . font-lock-keyword-face)
       (,(concat
          "\\<\\(_Atomic\\|_Bool\\|char16_t\\|char32_t\\|_Complex\\|complex\\|"
          "_Imaginary\\|imaginary\\)\\>")
        . font-lock-type-face)
       ("\\<nullptr\\>" . font-lock-constant-face)
       ))) t)

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
(autoload 'hindent-mode "hindent" nil t)
(add-to-list 'auto-mode-alist '("\\.\\(c?hs\\|hsc\\)\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("xmobarrc\\'" . haskell-mode))
(add-to-list 'magic-mode-alist '("^#!/.*runhaskell.*$" . haskell-mode))
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'hindent-mode)
(setq hindent-reformat-buffer-on-save t)

;; JavaScript
(add-to-list 'auto-mode-alist '("\\.\\(gs\\|jsx?\\)\\'" . web-mode))
(add-hook
 'web-mode-hook
 '(lambda ()
    (setq web-mode-code-indent-offset 4
          web-mode-markup-indent-offset 2)))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

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
    (setq font-latex-fontify-sectioning 'color)
    (setq preview-image-type 'pnm)
    (TeX-PDF-mode t)
    (set-fill-column 99999)
    (local-unset-key (kbd "C-j"))
    (local-set-key
     [f2]
     '(lambda ()
        (interactive)
        (save-buffer)
        (TeX-command "LaTeX" 'TeX-master-file)))))
(setq bibtex-entry-format t ;; be more aggressive about cleaning up
      bibtex-align-at-equal-sign t)
(add-hook
 'bibtex-mode-hook
 '(lambda ()
    (local-set-key
     (kbd "<f6>")
     '(lambda ()
        (interactive)
        (bibtex-sort-buffer)
        (bibtex-reformat)))
    (local-set-key (kbd "S-<f6>") 'bibtex-reformat)))

;; Lua
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
(autoload 'lua-mode "lua-mode" nil t)
(add-hook 'lua-mode-hook 'hs-minor-mode)
(setq lua-indent-level 4)

;; Markdown
(autoload 'markdown-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.m\\(d\\|arkdown\\)\\'" . markdown-mode))
(setq markdown-enable-math t)
(setq markdown-translate-filename-function '(lambda (file) ())) ;; disable links

;; PureScript
(add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)

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
(add-to-list 'auto-mode-alist '("\\.pyw?\\'" . python-mode))
(add-to-list 'magic-mode-alist '("^#!/.*[jp]ython[.0-9]*$" . python-mode))

;; R
(autoload 'r-mode "ess-site" nil t)
(add-to-list 'auto-mode-alist '("\\.r\\'" . r-mode))
(eval-after-load "ess-site" '(ess-toggle-underscore nil))

;; Rust
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-to-list 'magic-mode-alist '("^#!/.*cargo-script$" . rust-mode))

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
    (when height (set-face-attribute face nil :height height))
    t))

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
(defun face-iosevka ()
  (interactive)
  (set-face 'default "Iosevka Term" 160))
(defun face-monoid ()
  (interactive)
  (set-face 'default "Monoid" 140))
(defun face-mononoki ()
  (interactive)
  (set-face 'default "mononoki" 140))
(defun face-oxygen ()
  (interactive)
  (set-face 'default "Oxygen Mono" 130))
(defun face-inconsolata ()
  (interactive)
  (set-face 'default "Inconsolata" 140))
(defun face-ubuntu ()
  (interactive)
  (set-face 'default "Ubuntu Mono" 140))
(defun face-share-tech ()
  (interactive)
  (set-face 'default "Share Tech Mono" 160))

(setq custom-safe-themes t)
(defvar my-theme)

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
    (or (face-mononoki)
        (face-iosevka)
        (face-share-tech)
        (face-oxygen)
        (face-monoid)
        (face-droid)
        (face-envy)
        (face-fira)
        (face-inconsolata)
        (face-ubuntu)
        (face-dejavu)
        (face-consolas))

    (set-face-attribute 'fixed-pitch nil :family 'unspecified)

    ;; font size / face height
    (global-set-key (kbd "C--") 'face-height-decrease)
    (global-set-key (kbd "C-=") 'face-height-increase))

  (global-hl-line-mode)
  (setq ansi-color-names-vector
        [unspecified "#3f3f3f" "#cc9393" "#7f9f7f"
                     "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])
  (ignore-errors (load-theme my-theme))
)

(if (display-graphic-p)
    (progn
      (rf-initialize-colors t)
      (defun rf-exit-emacs ()
        (interactive)
        (when (yes-or-no-p "Really exit emacs?")
          (save-buffers-kill-terminal)))
      (global-set-key (kbd "C-x C-c") 'rf-exit-emacs))
  ;; we need to wait until (tty-color-alist) is fully populated
  (add-hook
   'term-setup-hook
   (lambda ()
     (when (>= (length (tty-color-alist)) 256)
       (setq frame-background-mode 'dark)))))
