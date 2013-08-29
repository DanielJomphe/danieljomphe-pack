(live-load-config-file "bindings.el")

;;; -------------------------------------------------------------------------
;;; PACKAGES
;;; -------------------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(defvar my-packages
  '(
  ;;elein
  ;;magit-simple-keys
    hl-sexp
  org
;;markdown-mode
;;guru-mode
;;projectile
    )
  "A list of packages to ensure are installed at launch.")

(defun my-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (my-packages-installed-p)
  (message "%s" "Refreshing the package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;;; -------------------------------------------------------------------------
;;; COSMETIC
;;; -------------------------------------------------------------------------

;;; TODO properly conditionalize most of the following
;;; TODO make those functions callable at any time, and improve upon that

(when (eq system-type 'darwin)
  ;; Work around a bug on OS X where system-name is FQDN
  (setq system-name (car (split-string system-name "\\."))))

(defun dj-os-osx ()
  (set-face-attribute 'default nil :family "menlo")
  (set-face-attribute 'default nil :height 150)
;;(ns-toggle-fullscreen)
  )
(defun dj-os-windows ()
  (set-face-attribute 'default nil :family "consolas")
  (add-to-list 'initial-frame-alist `(fullscreen . fullheight))
  (add-to-list 'default-frame-alist `(fullscreen . fullheight))
  (add-to-list 'default-frame-alist '(left . 0))
  (add-to-list 'default-frame-alist '(top . 0))
  (add-to-list 'default-frame-alist '(height . 47))
  (add-to-list 'default-frame-alist '(width . 155)))
(defun dj-os-linux ()
  (set-face-attribute 'default nil :family "whatever")
  (add-to-list 'initial-frame-alist `(fullscreen . fullheight))
  (add-to-list 'default-frame-alist `(fullscreen . fullheight))
  (add-to-list 'default-frame-alist '(left . 0))
  (add-to-list 'default-frame-alist '(top . 0))
  (add-to-list 'default-frame-alist '(height . 47))
  (add-to-list 'default-frame-alist '(width . 155)))
(dj-os-osx)


(defun dj-screen-small ()
  (split-window-horizontally))
(defun dj-screen-big ()
  (delete-other-windows)
  (switch-to-buffer "*scratch*")
  (split-window-horizontally -80)
  (split-window-horizontally  80)
  (split-window-vertically))
(dj-screen-small)
;;; (dj-screen-big)

(defun dj-cursor ()
;;(set-cursor-color "#ff0000")
  (setq-default cursor-type 'bar))
(dj-cursor)

(setq initial-scratch-message nil)
;;; (setq initial-buffer-choice "~/.emacs.d/config/core.el")

(setq mouse-wheel-scroll-amount '(1 ((shift) . 3) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;;; -------------------------------------------------------------------------
;;; MODALITY
;;; -------------------------------------------------------------------------

;;(global-whitespace-mode)                ; Run this only with ESK!
;;(global-hl-sexp-mode)
;;(projectile-global-mode)
;;(guru-global-mode +1)
;;(global-rainbow-delimiters-mode t)

;;; (define-globalized-minor-mode whatever-mode
;;;  whatever-mode
;;;    (lambda ()
;;;       (whatever-mode t)))
;;; (global-whatever-mode)

(add-to-list 'auto-mode-alist '("\.dtm$"  . clojure-mode))
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))

;;; deactivate line highlighting from both ESK and Live Coding
(global-hl-line-mode 0)
;;; prog-mode-hook is defined in Emacs Starter Kit
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

;;;(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(add-hook 'slime-repl-mode-hook
          (lambda ()
            (font-lock-mode nil)
            (clojure-mode-font-lock-setup)
            (font-lock-mode t)))

;; (autoload 'markdown-mode "markdown-mode"
;;    "Major mode for editing Markdown files" t)
;; (setq auto-mode-alist
;;    (cons '("\\.md" . markdown-mode) auto-mode-alist))

;;; -------------------------------------------------------------------------
;;; DURENDAL
;;; -------------------------------------------------------------------------

;;; Clojure Utilities
;;; Durendal 0.2 didn't work; let's copy & adapt what works.
;;; In fact, not much here seems to work. :/

;;; Auto-compile on save (deactivated for now)
(defvar durendal-auto-compile? nil      ; `t` if I want to put it back on
  "Automatically compile on save when applicable.")

(defun durendal-in-current-project? (file)
  (let ((root (expand-file-name
               (read (cadr (slime-eval
                            '(swank:eval-and-grab-output
                              "(System/getProperty \"user.dir\")")))))))
    (string= (substring file 0 (length root)) root)))

(defun durendal-auto-compile ()
  (when (and slime-mode durendal-auto-compile?
             (slime-connected-p) (slime-current-package)
             (durendal-in-current-project? buffer-file-name))
    (slime-compile-and-load-file)))

(defun durendal-enable-auto-compile ()
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook 'durendal-auto-compile))

;;; Slime REPL enhancenements (I don't remember what they do)
(defadvice slime-repl-emit (after durendal-slime-repl-emit-ad)
  (with-current-buffer (slime-output-buffer)
    (add-text-properties slime-output-start slime-output-end
                         '(font-lock-face slime-repl-output-face
                                          rear-nonsticky (font-lock-face)))))

(defadvice slime-repl-insert-prompt (after durendal-slime-repl-prompt-ad)
  (with-current-buffer (slime-output-buffer)
    (let ((inhibit-read-only t))
      (add-text-properties slime-repl-prompt-start-mark (point-max)
                           '(font-lock-face slime-repl-prompt-face
                                            rear-nonsticky
                                            (slime-repl-prompt
                                             read-only
                                             font-lock-face
                                             intangible))))))

(defun durendal-enable-slime-repl-enhancements ()
  (ad-activate #'slime-repl-emit)
  (ad-activate #'slime-repl-insert-prompt))

(defun durendal-disable-slime-repl-enhancements ()
  (ad-deactivate #'slime-repl-emit)
  (ad-deactivate #'slime-repl-insert-prompt))

(defun durendal-enable ()
  "Enable hooks for all durendal functionality."
  (add-hook 'slime-connected-hook
            (lambda ()
              (if (equal (slime-lisp-implementation-name) "clojure")
                  (progn
                    (add-hook 'clojure-mode-hook
                              'durendal-enable-auto-compile)
                    (durendal-enable-slime-repl-enhancements))
                  (progn
                    (remove-hook 'clojure-mode-hook
                                 'durendal-enable-auto-compile)
                    (durendal-disable-slime-repl-enhancements))))))

(durendal-enable)

;;; -------------------------------------------------------------------------
;;; BINDINGS
;;; -------------------------------------------------------------------------

(global-set-key "\C-h" 'backward-delete-char-untabify)
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(global-set-key (kbd "C-c C-j") 'clojure-jack-in)

;;; -------------------------------------------------------------------------
;;; WHATEVER
;;; -------------------------------------------------------------------------

;;; TODO make sure this still works; last time, it didn't.
;;; TODO make sure this is needed. There's a rgrep mode, see
;;;      http://david.rothlis.net/emacs/basic_c.html
;;; TODO bring ack into the fold, too! Try one of the packages.
;;; thanks johnw: https://gist.github.com/1198329
(defun find-grep-in-project (command-args)
  (interactive
   (progn
     (list (read-shell-command "Run find (like this): "
                               '("git ls-files -z | xargs -0 egrep -nH -e " . 41)
                               'grep-find-history))))
  (when command-args
    (let ((null-device nil)) ; see grep
      (grep command-args))))

(server-start)
