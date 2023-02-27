;; NOTE: init.el is generated from this emacs.org
;;
;; This configuration is heavily inspired by System Crafters Emacs from Scratch series

;; Easily change the font size and transparance, e.g. for use on monitors with different resolutions
(defvar ijskegel/default-font-size 90)
(defvar ijskegel/default-variable-font-size 100)
(defvar ijskegel/frame-transparency '(100 . 100))

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

;; automatically reverts the buffer when its visited file changes on disk
(global-auto-revert-mode t)

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha ijskegel/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,ijskegel/frame-transparency))
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		treemacs-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Set the default face
(set-face-attribute 'default nil :family "JetBrainsMono NF" :height ijskegel/default-font-size :weight 'regular)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono NF" :height ijskegel/default-font-size :weight 'regular)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height ijskegel/default-variable-font-size)

(use-package ef-themes
  :init (load-theme 'ef-day :no-confirm))

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

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :after evil
  :config
  (general-create-definer ijskegel/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (ijskegel/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "fde" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/emacs.org")))))

(use-package evil
  :init
  (setq evil-want-integration t)
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

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
;; Work with Git forges, such as Github and Gitlab, from the comfort of Magit and the rest of Emacs
;;(use-package forge
;;  :after magit)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

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

(use-package denote)
(setq denote-directory (expand-file-name "/media/sf_Notes/notes"))
(setq denote-known-keywords '("emacs" "benchmark" "asml" "tc"))
;; default is org, others are markdown+(TOML, YAML) and plain text
(setq denote-file-type nil)
(add-hook 'denote-dired-mode-hook #'denote-dired-mode)

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

(server-start)
(add-hook 'server-switch-hook (lambda () (select-frame-set-input-focus (selected-frame))))
