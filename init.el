;; NOTE: init.el is generated from this emacs.org
;;
;; The initial version of this configuration is inspired by System Crafters Emacs from Scratch series

(cond ((eq system-type 'gnu/linux)
       ;; directory where I store all my notes and GTD files
       (setq ijskegel-notes-directory "/media/sf_notes")
       ;; font names
       (setq ijskegel-default-font "JetBrainsMono NF")
       (setq ijskegel-fixed-pitch-font "JetBrainsMono NF")
       (setq ijskegel-variable-pitch-font "JetBrainsMono NF")
       ;; Easily change the font size and transparance, e.g. for use on monitors with different resolutions
       (defvar ijskegel/default-font-size 90)
       (defvar ijskegel/default-variable-font-size 100)
       (defvar ijskegel/frame-transparency '(95 . 95))
       ))
(cond ((eq system-type 'windows-nt)
       ;; directory where I store all my notes and GTD files
       (setq ijskegel-notes-directory "C:/Users/lochep/Documents/notes")
       ;; font names
       (setq ijskegel-default-font "JetBrainsMono NF")
       (setq ijskegel-fixed-pitch-font "JetBrainsMono NF")
       (setq ijskegel-variable-pitch-font "Calibri")
       ;; Easily change the font size and transparance, e.g. for use on monitors with different resolutions
       (defvar ijskegel/default-font-size 75)
       (defvar ijskegel/default-variable-font-size 100)
       (defvar ijskegel/frame-transparency '(100 . 100))
       ))

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
;(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(blink-cursor-mode 0)       ; Do not blink the cursor
(menu-bar-mode -1)          ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

;; automatically select help window, close with 'q'
(setq help-window-select t)

(column-number-mode)
(global-hl-line-mode t)

;; scroll behaviour more like in vim
(setq scroll-conservatively most-positive-fixnum)
(setq scroll-step 1)
(setq scroll-margin 5)

;; set the minimal width for the line numbers gutter to 4 characters
(setq-default display-line-numbers-width 4)
;; show line numbers
(global-display-line-numbers-mode 1)
;; (setq display-line-numbers-type 'relative) enable for relative line numbers

;; automatically reverts the buffer when its visited file changes on disk
(global-auto-revert-mode t)

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha ijskegel/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,ijskegel/frame-transparency))
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(height . 55))
(add-to-list 'default-frame-alist '(width . 125))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                lisp-interaction-mode-hook
                dired-mode-hook
                ibuffer-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; open buffer-menu in current window
;; (global-set-key (kbd "C-x C-b") 'buffer-menu)

;; center frame on current monitor
;; credit: https://christiantietze.de/posts/2022/04/emacs-center-window-current-monitor-simplified/
(defun ijskegel/frame-recenter (&optional frame)
  "Center FRAME on the screen.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (modify-frame-parameters
     frame '((user-position . t) (top . 0.5) (left . 0.5)))))

(global-set-key (kbd "M-c") #'ijskegel/frame-recenter)

;; Set the default face
(set-face-attribute 'default nil :family ijskegel-default-font :height ijskegel/default-font-size :weight 'regular)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font ijskegel-fixed-pitch-font :height ijskegel/default-font-size :weight 'regular)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font ijskegel-variable-pitch-font :height ijskegel/default-variable-font-size)

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-opera :no-confirm)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package savehist
  :init
  (setq history-length 25)
  (savehist-mode))

(use-package vertico
  :init
  (vertico-mode)
  ;; enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  ;; use evil-like bindings for next and previous
  :bind (:map vertico-map
	      ("C-j" . vertico-next)
	      ("C-k" . vertico-previous)))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles . (partial-completion))))))

(use-package marginalia
  ;; bind `marginalia-cycle' only in the minibuffer
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))



;; (setup (:pkg embark)
;; 	 (:also-load embark-consult)
;; 	 (:global "C-S-a" embark-act)
;; 	 (:with-map minibuffer-local-map
;; 		    (:bind "C-d" embark-act))

;; 	 ;; Show Embark actions via which-key
;; 	 (setq embark-action-indicator
;; 	       (lambda (map)
;; 		 (which-key--show-keymap "Embark" map nil nil 'no-paging)
;; 		 #'which-key--hide-popup-ignore-command)
;; 	       embark-become-indicator embark-action-indicator))

(use-package popper
  :ensure t
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*Buffer List"
          "\\*xref\\*"
          "\\*eldoc\\*"
          help-mode
          compilation-mode))
  (setq popper-window-height 30)        ; dependent on screen resolution, make machine specific!
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

(set-register ?e '(file . "~/.emacs.d/emacs.org"))
(global-set-key (kbd "<f6>") (lambda() (interactive)(find-file "~/.emacs.d/emacs.org")))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; optional, is set to t by default
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-kill-on-visual-paste nil)
  (setq evil-symbol-word-search t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)

  (evil-set-undo-system 'undo-redo))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(defun ijskegel/switch-to-last-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently opened buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key (kbd "M-o") #'ijskegel/switch-to-last-buffer)

(defun ijskegel/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . ijskegel/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"))

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun ijskegel/org-mode-visual-fill ()
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . ijskegel/org-mode-visual-fill))

(setq ijskegel-gtd-directory (expand-file-name "gtd" ijskegel-notes-directory))

(set-register ?g (cons 'file ijskegel-gtd-directory))

(setq ijskegel-inbox-file (expand-file-name "inbox.org" ijskegel-gtd-directory))
(setq ijskegel-gtd-file (expand-file-name "gtd.org" ijskegel-gtd-directory))
(setq ijskegel-personal-file (expand-file-name "personal.org" ijskegel-gtd-directory))
(setq ijskegel-tickler-file (expand-file-name "tickler.org" ijskegel-gtd-directory))
(setq ijskegel-someday-file (expand-file-name "someday.org" ijskegel-gtd-directory))

(setq org-agenda-files (list ijskegel-inbox-file
                             ijskegel-gtd-file
                             ijskegel-personal-file
                             ijskegel-tickler-file))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline ijskegel-inbox-file "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline ijskegel-tickler-file "Tickler")
                               "* %i%? \n %U")))

(setq org-refile-targets '((ijskegel-gtd-file :maxlevel . 2)
                           (ijskegel-personal-file :level . 1)
                           (ijskegel-someday-file :level . 1)
                           (ijskegel-tickler-file :maxlevel . 2)))

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

(setq org-agenda-custom-commands 
      '(("w" "Work" tags-todo "@work"
         ((org-agenda-overriding-header "Work")
          (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
        ("h" "Home" tags-todo "@home"
         ((org-agenda-overriding-header "Home")
          (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))))

(defun my-org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("cpp" . "src cpp")))

;; Automatically tangle our emacs.org config file when we save it
(defun ijskegel/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
		      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'ijskegel/org-babel-tangle-config)))

;; switch between header and source file (if present)
(global-set-key (kbd "<f4>") 'ff-find-other-file)

;; show column indicator at 80 chars
(setq-default display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package tree-sitter)
(use-package tree-sitter-langs)
;; enable it globally and enable highlighting always if major mode is known in tree-sitter
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(use-package scad-mode)

(use-package dumb-jump
  :init
  ;; the next line requires at least Xref 1.1.0 (bundled with emacs 28.1 or newer)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package yasnippet)
(use-package yasnippet-snippets) ;; TODO replace with only the snippets actually used
(yas-global-mode 1)

(use-package docstr)
(global-docstr-mode 1)
(setq docstr-c-style 'qt)
(setq docstr-key-support t)
(add-hook 'docstr-before-insert-hook (lambda (search-string) (insert "@brief ")))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single
  :commands (dired dired-jump))

(use-package beframe
 :config
 (define-key global-map (kbd "C-x C-b") #'beframe-buffer-menu)
 (beframe-mode 1))

(use-package denote)
(setq denote-directory (expand-file-name "notes" ijskegel-notes-directory))
(setq denote-known-keywords '("emacs" "benchmark" "asml" "tc"))
;; default is org, others are markdown+(TOML, YAML) and plain text
(setq denote-file-type nil)

(set-register ?n (cons 'file denote-directory))

;; Enable fontification in Dired for the notes directory and its references subdirectory
(setq denote-dired-directories
      (list denote-directory
      (thread-last denote-directory (expand-file-name "references"))))

(add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

(load-file "~/.emacs.d/google-c-style.el")
(add-hook 'c-mode-common-hook 'google-set-c-style)

(use-package company
  :config
  (global-company-mode))

(use-package eglot)

(require 'eglot)
(define-key eglot-mode-map (kbd "C-c <tab>") #'company-complete) ;; initiate the completion
(define-key eglot-mode-map (kbd "C-c e j ") #'flymake-goto-next-error)
(define-key eglot-mode-map (kbd "C-c e k ") #'flymake-goto-prev-error)
(define-key eglot-mode-map (kbd "C-c e r ") #'eglot-rename)

;; (server-start)
;; (add-hook 'server-switch-hook (lambda () (select-frame-set-input-focus (selected-frame))))
