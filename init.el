;; -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; for org afterwards							                            ;;
;; (assq-delete-all 'org package--builtins)				                        ;;
;; (assq-delete-all 'org package--builtin-versions)			                    ;;
;; ;; use-package							                                	;;
;; (require 'package)							                                ;;
;; (setq package-check-signature nil)					                        ;;
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t) ;;
;; (package-initialize)							                                ;;
;; (require 'use-package-ensure)					                            ;;
;; (setq use-package-always-ensure t)					                        ;;
;; (unless (package-installed-p 'use-package)				                    ;;
;; (package-refresh-contents)						                            ;;
;; (package-install 'use-package))					                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
;; emacs
(use-package emacs
  :config
  ;; code system
  (prefer-coding-system 'utf-8)
  (setq system-time-locale "C")
  ;; startup screen
  (setq inhibit-startup-screen t)
  (setq initial-scratch-message nil)
  (setq inhibit-splash-screen t)
  (setq inhibit-startup-echo-area-message "YOUR-USER-NAME") ;;C-h v user-login-name
  (setq frame-title-format "%b")
  (setq cursor-type 'bar)
  ;; mode
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (cua-mode 1)
  (hl-line-mode 1)
  (visual-line-mode 1)
  ;; better
  (setq ring-bell-function 'ignore)
  (fset 'yes-or-no-p 'y-or-n-p)
  (global-auto-revert-mode 1)
  ;; stop auto-save
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  ;; set face, set font set
  (set-face-attribute 'default nil
		      :family "BlexMono Nerd Font Mono"
		      :height 125
		      :weight 'regular)
  (set-face-attribute 'variable-pitch nil
		      :family "BlexMono Nerd Font Mono"
		      :height 125
		      :weight 'regular)
  (set-fontset-font t 'han
		    (font-spec :family "Noto Sans Mono CJK SC"
			       :weight 'regular))
  (set-fontset-font t 'cjk-misc
		    (font-spec :family "Noto Sans Mono CJK SC"
			       :weight 'regular))
  (set-fontset-font t '(#x1f300 . #x1fad0)
		    (font-spec :family "Segoe UI Emoji"
			       :weight 'regular)
		    nil 'prepend)
  (set-fontset-font t 'unicode
		    (font-spec :family "Noto Sans Mono CJK SC"
			       :weight 'regular)
		    nil 'prepend)
  (setq initial-frame-alist
	'((width . 55)
	  (height . 25)))
  (setq default-frame-alist
	'((width . 55)
	  (height . 25))))
(setenv "PATH" (concat "C:\\msys64\\mingw64\\bin;" (getenv "PATH")))
;; which key
(use-package which-key
  :config
  (which-key-mode 1))
(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-item 200))
;; window
(winner-mode t)
(use-package bookmark
  :config
  (setq bookmark-save-flag 1)
  (setq bookmark-default-file "~/.emacs.d/bookmarks.eld")
  (bookmark-load bookmark-default-file t))
;;;; packages
;;;;;; essential
(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic)))
(use-package marginalia
  :straight t
  :config  (marginalia-mode t))
(use-package vertico
  :straight t
  :bind (:map vertico-map
	     ("RET" . vertico-directory-enter)
	     ("DEL" . vertico-directory-delete-word)
	     ("M-DEL" . vertico-directory-delete-char))
  :init
  (vertico-mode)
  :config
  (setq vertico-count 10
	vertico-cycle t)
  (vertico-multiform-mode 1))
(use-package savehist
  :init
  (savehist-mode)
  :config
  (setq savehist-autosave-interval 300)
  (setq savehist-file "~/.emacs.d/history")
  (setq savehist-additional-variables
	'(search-ring
	  regexp-search-ring
	  register-alist
	  file-name-history
	  shell-command-history)))
(use-package saveplace
  :ensure nil
  :command t
  :init
  (setq save-place-file "~/.emacs.d/saveplace")
  :config
  (setq save-place-forget-unreadable-files t)
  (save-place-mode +1))
(use-package corfu
  :straight t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-min-width 40)
  (corfu-max-width 100)
  (corfu-count 10)
  (corfu-preselect 'prompt)
  (corfu-preview-current 'insert) 
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous)
              ("RET" . nil)
              ([return] . nil))
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))
(use-package embark-consult
  :straight t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))
(use-package embark
  :straight t)
(global-set-key (kbd "C-.") 'embark-act)
;; (global-set-key (kbd "M-.") 'embark-dwim)
(use-package consult
  :straight t
  :bind (("C-x r" . consult-recent-file)))
(use-package super-save
  :straight t
  :config
  (setq super-save-auto-save-when-idle t
        super-save-idle-duration 5 ;; after 5 seconds of not typing autosave
        super-save-triggers ;; Functions after which buffers are saved (switching window, for example)
        '(evil-window-next evil-window-prev balance-windows other-window next-buffer previous-buffer)
        super-save-max-buffer-size 10000000)
  (super-save-mode +1))
(defun jib-clear-echo-area-timer ()
  (run-at-time "2 sec" nil (lambda () (message " "))))
(advice-add 'super-save-command :after 'jib-clear-echo-area-timer)
(use-package vundo
  :straight t
  :bind (("C-x u" . vundo))
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))
(use-package smartparens
  :straight t
  :config
  ;; Load default smartparens rules for various languages
  (require 'smartparens-config)
  (setq sp-max-prefix-length 25)
  (setq sp-max-pair-length 4)
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  (with-eval-after-load 'evil
    (setq sp-show-pair-from-inside t)
    (setq sp-cancel-autoskip-on-backward-movement nil)
    (setq sp-pair-overlay-keymap (make-sparse-keymap)))
  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))
  ;; In lisps ( should open a new form if before another parenthesis
  (sp-local-pair sp-lisp-modes "(" ")" :unless '(:rem sp-point-before-same-p))
  ;; Don't do square-bracket space-expansion where it doesn't make sense to
  (sp-local-pair '(emacs-lisp-mode org-mode markdown-mode gfm-mode)
                 "[" nil :post-handlers '(:rem ("| " "SPC")))
  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
             ;; Don't autopair opening braces if before a word character or
             ;; other opening brace. The rationale: it interferes with manual
             ;; balancing of braces, and is odd form to have s-exps with no
             ;; whitespace in between, e.g. ()()(). Insert whitespace if
             ;; genuinely want to start a new form in the middle of a word.
             :unless '(sp-point-before-word-p sp-point-before-same-p)))
  (smartparens-global-mode t))
;;;;;; org-mode
(use-package org
  :straight t
  :hook (org-mode . org-indent-mode)
  :config
  (setq org-directory "~/org")
  (setq org-agenda-files (list "~/org/agenda" "~/org/roam/daily"))
  (setq org-ellipsis "…")
  (setq org-fontify-whole-heading-line t)
  (setq org-pretty-entities t))
(use-package org-roam
  :straight t
  :after org
;;:custom
;;(org-roam-directory "c:/Users/your-name/org/roam")
;;;; this line help org-roam to act with "[[" to pop up compeletion list to select an existed node,
;;;; or "[[roam:" to launch org-roam-capture to get a new node that will be inserted the point.
;;(org-roam-complete-everywhere t)
  :config
  (org-roam-db-autosync-mode))
(use-package org-roam-ui
  :straight t
  :after org-roam)
(use-package org-ql
  :straight t
  :after org)
(use-package org-roam-ql
  :straight t
  :after org-roam org-ql
  )
(use-package org-super-agenda
  :straight t
  :after org
  :hook (org-agenda-mode . org-super-agenda-mode))

;;;;;; evil-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package evil				                 	  ;;
;;  :straight t						                      ;;
;;  :init						                          ;;
;;  (setq evil-want-keybinding nil)			              ;;
;;  (setq evil-want-C-u-scroll t)			              ;;
;;  :config						                          ;;
;;  (evil-mode))				                      	  ;;
;; (define-key evil-normal-state-map (kbd "：") 'evil-ex) ;;
;; (use-package evil-org			                	  ;;
;;  :straight t					                    	  ;;
;;  :after evil org				                    	  ;;
;;  :hook						                          ;;
;;  (org-mode . evil-org-mode))			             	  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; beautify
;;;; theme
(use-package color-theme-sanityinc-tomorrow
  :straight t
  :config
  (load-theme 'sanityinc-tomorrow-day t))
;;;; for org-mode
(use-package org-modern
  :straight t
  :after org
  :hook
  (org-mode . org-modern-mode)
  :custom
  (org-modern-hide-stars nil)
  (org-modern-star nil)
  (org-modern-list
   '((?+ . "•")
     (?- . "•")
     (?* . "•")))
  (org-modern-timestamp nil)
  (org-modern-progress 5)
  (org-modern-table nil)
  (org-modern-block-name nil)
  (org-modern-block-fringe nil)
  (org-modern-priority nil)
  (org-modern-keyword nil))
(use-package org-appear
  :straight t
  :hook
  (org-mode . org-appear-mode)
  :init
  (setq org-hide-emphasis-markers t
	org-appear-autoemphasis t
	org-appear-autolinks nil
	org-appear-autosubmarkers t))
;;;;modeline
(use-package hide-mode-line
  :straight t
  :commands (hide-modeline-mode))
(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-buffer-encoding 'nondefault))
(column-number-mode)
;; my quick
;;;; org-agenda
(defun my/org-agenda-today ()
  (interactive)
  (org-agenda nil "a")
  (org-agenda-log-mode))
;; keybinding
(use-package general
  :straight t)
(use-package magit
  :straight t
  :init
  (defun my/magit-version-override (&rest _) "4.4.2")
  (advice-add 'magit-version :override #'my/magit-version-override))
(use-package neotree
  :straight t
  :defer t
  :bind (("C-x D" . neotree-toggle)))
(use-package tldr
  :straight t
  :config
  (setq tldr-enabled-categories '("common"
				  "linux"
				  "osx"
				  "sunos")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package dashboard			                	   ;;
;;   :straight t				                    	   ;;
;;   :config					                    	   ;;
;;   (setq dashboard-banner-logo-title "")		           ;;
;;   (setq dashboard-init-info "")			               ;;
;;   (setq dashboard-startup-banner "")	             	   ;;
;;   (setq dashboard-center-content t)	         	       ;;
;;   (setq dashboard-vertically-center-content t)	       ;;
;;   (setq dashboard-items '((recents . 5)))		       ;;
;;   (setq dashboard-item-shortcuts '((recents . "r")))    ;;
;;   (dashboard-setup-startup-hook))			           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package writeroom-mode
  :straight t)

;; Windows user with straight.el
;; 1. Using MinGW64
;; 2. Update MinGW64: pacman -Syu
;; 3. Then: pacman -S make mingw-w64-x86_64-gcc git pkg-config mingw-w64-x86_64-libmupdf
;; 4. After "make all" build inside MinGW64 or Emacs,
;;    copy your built "reader-core.dll"
;;    from .../straight/repos/emacs-reader
;;    to   .../straight/builds/reader
(use-package reader
  :straight '(reader :type git :host codeberg :repo "divyaranjan/emacs-reader"
  		     :files ("*.el" "render-core.so")
  		     :pre-build ("make" "all")))

;; major modes
(use-package csound-mode
  :straight t
  :mode ("\\.csd\\'" "\\.orc\\'" "\\.sco\\'")
  :bind (:map csound-mode-map
	      ("C-c C-p" . csound-play)
	      ("C-c C-k" . csound-stop))
  :hook (csound-mode . (lambda ()
			 (setq-local comment-start ";")
			 (setq-local comment-end "")))
  :custom
  (csound-flags "-odac"))
