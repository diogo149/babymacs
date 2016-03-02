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
;; function to update packages
(defun my/update-emacs-packages ()
  (interactive)
  (save-excursion
    (package-list-packages)
    (package-menu-mark-upgrades)
    (package-menu-execute)))

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
;; set default tab width
(setq tab-width 4)
;; when files change, have buffers automatically revert to stay in sync with
;; the filesystem (doesn't work on editted files - careful!)
(global-auto-revert-mode t)
;; keybinding to delete whitespace
(bind-key "S-<backspace>" #'delete-horizontal-space)
;; start in fundamental-mode
(setq initial-major-mode 'fundamental-mode)
;; make overwriting an external copy with an emacs copy add the external
;; copy to the kill ring
(setq save-interprogram-paste-before-kill t)
;; make scrolling smoother
(progn
  ;; only start scrolling when cursor is very close to edge of screen
  (setq scroll-margin 1)
  ;; don't recenter the cursor
  (setq scroll-conservatively 1000)
  (setq-default scroll-up-aggressively 0.01
                scroll-down-aggressively 0.01)
  )
;; show column numbers along with line numbers
(column-number-mode)
;; change smoothness of mouse scrolling
(progn
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
  (setq mouse-wheel-progressive-speed t))
;; when calling kill-ring-save (M-w) or kill-region (C-w) without a selection,
;; assume it means the current line
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end))
		 (list (line-beginning-position)
		       (line-beginning-position 2)))))
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
	   (line-beginning-position 2)))))
;; stop writing temporary files
(progn
  ;; disable autosaving (e.g. no #filename#)
  (setq auto-save-default nil)
  ;; disable creating lockfiles (eg. .#foo.bar)
  (setq create-lockfiles nil)
  ;; disable writing to backup files
  (setq make-backup-files nil)
  )
;; make C-v / M-v / C-S-v / M-S-v use a half page instead of full
(progn
  (defun half-window-height ()
    (/ (window-height) 2))
  (defun move-down-half-window ()
    (interactive)
    (next-logical-line (half-window-height)))
  (defun move-up-half-window ()
    (interactive)
    (previous-logical-line (half-window-height)))
  (defun scroll-down-half-window (&optional arg)
    (interactive)
    (dotimes (x (or arg (half-window-height)))
      (scroll-down-line)))
  (defun scroll-up-half-window (&optional arg)
    (interactive)
    (dotimes (x (or arg (half-window-height)))
      (scroll-up-line)))
  (bind-key "C-v" #'move-down-half-window)
  (bind-key "M-v" #'move-up-half-window)
  (bind-key "C-S-v" #'scroll-up-half-window)
  (bind-key "M-V" #'scroll-down-half-window)
  )
;; keep history on windows: use C-c <left>/<right> to undo/redo window changes
(winner-mode 1)
;; have C-x k kill current buffer
(defun my/kill-this-buffer ()
  "Kill current buffer without prompt"
  (interactive)
  (kill-buffer (current-buffer)))
(bind-key "C-x k" #'my/kill-this-buffer)

;; appearance
;; ---
;; use leuven theme
(load-theme 'leuven t)
;; disable blinking cursor
(blink-cursor-mode -1)
;; remove the tool bar
(tool-bar-mode -1)
;; highlight the current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#FFD")
;; use C-+ or C-- to change text size
(bind-key "C-+" #'text-scale-increase)
(bind-key "C--" #'text-scale-decrease)
;; highlight matching parentheses when the point is on them
(show-paren-mode 1)

;; buffer-move
;; ---
;; for swapping the buffers between windows
(use-package buffer-move
  :ensure t
  ;; generic C-S-<cursor> keybindings
  :bind (("<C-S-up>" . buf-move-up)
	 ("<C-S-down>" . buf-move-down)
	 ("<C-S-left>" . buf-move-left)
	 ("<C-S-right>" . buf-move-right)))

;; move-text
;; ---
;; allows moving lines / regions up and down in a file
(use-package move-text
  :ensure t
  :bind (("<M-up>" . move-text-up)
	 ("<M-down>" . move-text-down)))

;; discover-my-major
;; ---
;; conveniently display major mode keybindings
(use-package discover-my-major
  :ensure t
  :bind ("C-h C-m" . discover-my-major))

;; expand-region
;; ---
;; easily select regions that make sense
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; flycheck
;; ---
;; package for linting code
(use-package flycheck
  :ensure t
  :commands flycheck-mode
  :config
  (progn
    ;; AIRPLANE unicode
    (diminish 'flycheck-mode (string 32 #x2708))
    ))

;; magit
;; ---
(use-package magit
  :ensure t
  :commands (magit-status)
  :config
  (progn
    ;; allow finishing commit with C-x C-s or s-s
    (bind-key "C-x C-s" #'with-editor-finish git-commit-mode-map)
    (bind-key "s-s" #'with-editor-finish git-commit-mode-map)

    ;; improve magit-diff speed
    (setq magit-highlight-whitespace nil)
    (setq magit-highlight-trailing-whitespace nil)
    (setq magit-diff-refine-hunk nil)
    (setq magit-highlight-indentation nil)
    ))

;; org-mode
;; ---
(use-package org
  :ensure t
  :mode ("\\.org$" . org-mode)
  :init
  (progn
    ;; don't set S-<cursor> keys (because windmove uses them)
    (setq org-replace-disputed-keys t)
    )
  :config
  (progn
    ;; indent headings
    (setq org-startup-indented t)
    ;; use arrow instead of default ellipsis
    (setq org-ellipsis " \u25bc")
    ;; enable word-wrap and have commands work on lines visually
    (add-hook 'org-mode-hook 'visual-line-mode)
    ;; use global (buffer-move) keys instead of org defaults
    (unbind-key "<C-S-up>" org-mode-map)
    (unbind-key "<C-S-down>" org-mode-map)
    ))

;; undo-tree
;; ---
(use-package undo-tree
  :ensure t
  :bind (("C-z" . undo-tree-undo)
	 ("C-S-z" . undo-tree-redo)
	 ("s-z" . undo-tree-undo)))

;; evil-nerd-commenter
;; ---
(use-package evil-nerd-commenter
  :bind ("C-/" . evilnc-comment-or-uncomment-lines)
  :ensure t)

;; helm
;; ---
(use-package helm
  :ensure t
  :demand t
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

;; python
;; ---
(use-package python
  :mode ("\\.py$" . python-mode)
  :config
  (progn
    ;; auto-link with flake8
    (add-hook 'python-mode-hook #'flycheck-mode)
    ;; enable semantic mode (for helm-semantic-or-imenu)
    (add-hook 'python-mode-hook #'semantic-mode)

    ;; emacs reimplementation of virtualenvwrapper
    (use-package virtualenvwrapper
      :ensure t
      :config
      (progn
	;; make shells start in virtualenv
	(venv-initialize-interactive-shells)
	(venv-initialize-eshell)
	))

    ;; emacs python development environment
    ;; https://github.com/jorgenschaefer/elpy
    (use-package elpy
      :ensure t
      :diminish elpy-mode
      :config
      (progn
	;; use jedi as backed (seems to have better completion)
	(setq elpy-rpc-backend "jedi")
	;; modify elpy modules to disable some
	(setq elpy-modules
	      '(elpy-module-sane-defaults
		elpy-module-company
		elpy-module-eldoc
		;; elpy-module-flymake
		;; elpy-module-highlight-indentation
		;; elpy-module-pyvenv
		elpy-module-yasnippet))
	;; enable elpy
	(elpy-enable)
	;; pop tag to be consistent with lisps and beside M-.
	(bind-key "M-," #'pop-tag-mark elpy-mode-map)

	(defun my/elpy-workon ()
	  "Workon a virtualenv, then restart the elpy backend"
	  (interactive)
	  (venv-workon)
	  (elpy-rpc-restart))
	))

    ;; allow automatically formatting buffers
    (use-package py-autopep8
      :ensure t
      :config
      (progn
	(setq py-autopep8-options '("--max-line-length=80"))

	;; minor mode to disable autopep8-ing
	(defun my/py-autopep8-buffer ()
	  "like py-autopep8-buffer, but only runs in python mode"
	  (when (derived-mode-p #'python-mode)
	    (py-autopep8-buffer)))

	(define-minor-mode my/py-autopep8-mode
	  "Minor mode to enable autopep8 on save"
	  :lighter " P8"
	  :init-value nil
	  (cond
	   (my/py-autopep8-mode
	    (make-local-variable 'before-save-hook)
	    (add-hook 'before-save-hook #'my/py-autopep8-buffer))
	   (t
	    (kill-local-variable 'before-save-hook))))

	(defun my/py-autopep8-on ()
	  "Enable my/py-autopep8-mode minor mode."
	  (my/py-autopep8-mode 1))

	(add-hook 'python-mode-hook #'my/py-autopep8-on)
	))
    ))

)
(message ".emacs.d loaded in %fs" (- (float-time) *emacs-load-start*))
