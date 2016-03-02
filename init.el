(defvar *emacs-load-start* (float-time))

;; temporarily set variables to speedup startup
(let ((gc-cons-threshold 100000000)
      (file-name-handler-alist nil))

;; package stuff
;; ---
;; don't activate packages at startup (use-package can do this for us)
(setq package-enable-at-startup nil)
;; load packages
(package-initialize)
;; allow using MELPA as a source of packages
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; refresh contents
(when (not package-archive-contents)
  (package-refresh-contents))

;; use-package
;; ---
;; install
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; general customization
;; ---
;; remove initial annoying message
(setq inhibit-startup-message t)
;; remove initial scratch buffer message
(setq initial-scratch-message nil)
;; allow pressing y or n instead of typing "yes" or "no"
(defalias 'yes-or-no-p 'y-or-n-p)
;; enable windmove for moving between buffers
(windmove-default-keybindings)
;; enable recent file mode (to easily reopen recent files)
(recentf-mode)
(setq recentf-max-saved-items 500)

;; appearance
;; ---
;; use leuven theme
(load-theme 'leuven t)
;; disable blinking cursor
(blink-cursor-mode -1)
;; remove the tool bar
(tool-bar-mode -1)

;; magit
;; ---
(use-package magit
  :ensure t
  :commands (magit-status))

;; org-mode
;; ---
;; don't set S-<cursor> keys (because windmove uses them)
(setq org-replace-disputed-keys t)
;; use arrow instead of default ellipsis
(setq org-ellipsis " \u25bc")
;; enable word-wrap and have commands work on lines visually
(add-hook 'org-mode-hook 'visual-line-mode)

;; undo-tree
;; ---
(use-package undo-tree
  :ensure t
  :bind (("C-z" . undo-tree-undo)
	 ("C-S-z" . undo-tree-redo)
	 ("s-z" . undo-tree-undo)
	 ("s-S-z" . undo-tree-redo)))

;; helm
;; ---
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
	 ("M-y" . helm-show-kill-ring)
	 ("C-x C-f" . helm-find-files)
	 ("C-x <tab>" . helm-semantic-or-imenu)
	 ("C-x b" . helm-mini)
	 :map helm-map
	 ("DEL" . my/helm-backspace))
  :config
  (progn
    (require 'helm-config)

    (setq
     ;; allow helm to leverage recentf-mode
     helm-ff-file-name-history-use-recentf t
     ;; do not display invisible candidates
     helm-quick-update t
     ;; open helm buffer inside current window, not occupy whole other window
     ;; this makes helm behave like a popup
     helm-split-window-in-side-p t
     ;; allow fuzzy matching buffer names
     helm-buffers-fuzzy-matching t
     ;; move to end or beginning of source when reaching top or bottom of source.
     helm-move-to-line-cycle-in-source t)

    (helm-mode 1)

    (defun my/helm-backspace ()
      "On error when deleting (eg. read-only), quit without selecting."
      (interactive)
      (condition-case nil
	  (backward-delete-char 1)
	(error
	 (helm-keyboard-quit))))
    ))

)
(message ".emacs.d loaded in %fs" (- (float-time) *emacs-load-start*))
