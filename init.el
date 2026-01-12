;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; It's a config for MSYS2 Emacs.
;; People usually use MinGW64 or UCRT64.
;; Menu:
;; - straight.el, use-package
;; - config of emacs itself
;; - theme, modeline
;; - builtin tools
;; - completion suite
;; - embark, consult
;; - some editor improvasations              
;; - org-mode family                          
;; - extended tools for programming management
;; - some useful tools
;; - emms
;; - major modes for other langs
;; - some defun my/... sentences, for keybindings and etc.
;; - keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; straight.el, use-package
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
;; (setq straight-vc-git-default-clone-depth 'full)
;;;; straight.el, use-package ends here.
;;;; Emacs config, use-package style
(use-package emacs
  :config
  ;; coding system, locale
  (prefer-coding-system 'utf-8)
  (setq system-time-locale "C")
  ;; custom.el
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (load custom-file :no-error)
  ;; no builtin auto-save
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  ;; no ring bell
  (setq ring-bell-function 'ignore)
  ;; inhibit default screen
  (setq inhibit-startup-screen t)
  (setq initial-scratch-message nil)
  (setq inhibit-splash-screen t)
  (setq inhibit-startup-echo-area-message "__") ;; Here, write the value of: C-h v user-login-name
  ;; some better faces
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq frame-title-format "%b")
  (setq cursor-type 'box)
  ;; some simple builtin modes
  (global-auto-revert-mode 1)
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (horizontal-scroll-bar-mode 0)
  (visual-line-mode 1)
  (column-number-mode 1)
  (hl-line-mode 1)
  (display-line-numbers-mode 0)
  (cua-mode 1)
  (winner-mode t)
  ;; font
  (set-face-attribute 'default nil
		      :family "BlexMono Nerd Font Mono"
		      :height 135
		      :weight 'regular)
  ;; Even it's "variable-pitch", I would still use Mono font here.
  (set-face-attribute 'variable-pitch nil
		      :family "BlexMono Nerd Font Mono"
		      :height 135
		      :weight 'regular)
  ;; They automatically fallback to "default" style of height and weight.
  (set-fontset-font t 'han (font-spec :family "Noto Sans Mono CJK SC"))
  (set-fontset-font t 'cjk-misc (font-spec :family "Noto Sans Mono CJK SC"))
  ;; Expand the left from "#x1fad0" to "#x1faff".
  (set-fontset-font t '(#x1f300 . #x1faff) (font-spec :family "Segoe UI Emoji"))
  ;; It uses "nil 'append" to add the font to the end of font alist, to catch rest unicode characters. 
  (set-fontset-font t 'unicode (font-spec :family "Noto Sans Mono CJK SC") nil 'append)
  ;; frame size
  (setq initial-frame-alist
	'((width . 55)
	  (height . 25)))
  (setq default-frame-alist
	'((width . 55)
	  (height . 25))))
;; (setenv "PATH" (concat "c:/msys64/mingw64/bin;" (getenv "PATH")))
;;;; Emacs config ends here, now actual packages.
;;;; theme, ui, modeline
(use-package color-theme-sanityinc-tomorrow
  :straight t
  :config
  (load-theme 'sanityinc-tomorrow-day t))
(use-package diminish
  :straight t
  :config
  (diminish 'visual-line-mode "warp")
  (diminish 'eldoc-mode "edoc")
  (diminish 'org-indent-mode "ind"))
(use-package hide-mode-line
  :straight t
  :commands (hide-mode-line-mode)) ;;lazy-load
(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-buffer-encoding 'never))
;;;; builtin
(use-package which-key
  :straight t
  :diminish which-key-mode
  :init
  (which-key-mode 1))
(use-package recentf
  :init
  (recentf-mode 1)
  :config
  (setq recentf-max-menu-item 200)
  (setq recentf-exclude
	'("\\.git$"
	  "\\.eld$"
	  "~/.emacs.d/emms/"
	  "~/.emacs.d/history"
	  "~/.emacs.d/saveplace")))
(use-package bookmark
  :config
  (setq bookmark-save-flag 1)
  (setq bookmark-default-file "~/.emacs.d/bookmarks.eld")
  (bookmark-load bookmark-default-file t)
  (bookmark-maybe-load-default-file))
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
  :init
  (setq save-place-file "~/.emacs.d/saveplace")
  :config  
  (setq save-place-forget-unreadable-files t)
  (save-place-mode +1))
;;;; completion suite
(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic)))
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
(use-package marginalia
  :straight t
  :after vertico
  :init
  (marginalia-mode 1))
(use-package corfu-terminal
  :straight t
  :defer t)
(use-package corfu
  :straight t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-auto-delay 0.5)
  (corfu-quit-no-match t)
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
(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
;;;; embark, consult
(use-package embark-consult
  :straight t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))
(use-package embark
  :straight t
  :defer t
  :bind (:map global-map
	      ("C-." . embark-act)
	      ("M-." .embark-dwim)))
(use-package consult
  :straight t
  :bind (:map global-map
	      ("C-x r" . consult-recent-file)
	      ("C-x l" . consult-line)))
;;;; Editor:
;; improved auto-save, undo and editing experience
(use-package super-save
  :straight t
  :diminish (super-save-mode . "save")
  :init
  (super-save-mode +1)
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-idle-duration 5)
  (super-save-triggers '(evil-window-next
			 evil-window-prev
			 balance-windows
			 other-window
			 next-buffer
			 previous-buffer))
  (super-save-max-buffer-size 10000000)
  (super-save-silent t))
(use-package vundo
  :straight t
  :bind (:map global-map
	      ("C-x u" . vundo))
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))
(use-package smartparens
  :straight t
  :diminish (smartparens-mode . "paren")
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
;;;; org-mode family:
;; org-roam, org-modern, org-appear, org-ql, org-super-agenda
(setq straight-built-in-pseudo-packages '(org))
(use-package org
  :straight t
  :hook (org-mode . org-indent-mode)
  :config
  (setq org-directory "~/org")
  (setq org-agenda-files (list "~/org/agenda" "~/org/roam/daily"))
  (setq org-ellipsis "…")
  (setq org-fontify-whole-heading-line t)
  (setq org-pretty-entities t)
  (setq org-html-validation-link nil)
  (setq org-attach-id-dir "~/org/.attach/"))
(use-package org-roam
  :straight t
  :after org
  :custom
  (org-roam-directory "~/org/roam")
  (org-roam-complete-everywhere t)
  :config
  (org-roam-db-autosync-mode))
(use-package org-roam-ui
  :straight t
  :defer t
  :commands (org-roam-ui-open)
  :after org-roam)
(use-package org-ql
  :straight t
  :defer t
  :after org)
(use-package org-roam-ql
  :straight t
  :defer t
  :after org-roam org-ql)
(use-package org-super-agenda
  :straight t
  :after org
  :defer t
  :hook (org-agenda-mode . org-super-agenda-mode))
(use-package org-modern
  :straight t
  :defer t
  :after org
  :hook (org-mode . org-modern-mode)
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
  :after org
  :defer t
  :hook (org-mode . org-appear-mode)
  :init
  (setq org-hide-emphasis-markers t)
  (setq org-appear-autoemphasis t)
  (setq org-appear-autolinks nil)
  (setq org-appear-autosubmarkers t))
;;;; extended tools for programming management
(use-package helpful
  :straight t
  :bind (:map global-map
	      ("C-h f" . helpful-callable)
	      ("C-h v" . helpful-variable)
	      ("C-h k" . helpful-key)
	      ("C-h x" . helpful-command)
	      ("C-h C-a" . helpful-at-point)
	      ("C-h F" . helpful-function)))
(use-package treesit
  :straight nil
  :custom
  (treesit-font-lock-level 4))
(use-package treesit-auto			     
  :straight t				     	     
  :after treesit
  :defer t
  :custom					     
  (treesit-auto-install 'prompt)		     
  :config					     
  ;;(setq treesit-auto-langs '(python yaml lisp))    
  (treesit-auto-add-to-auto-mode-alist 'all)	     
  (global-treesit-auto-mode))
(use-package flycheck
  :straight t
  :defer t)
(use-package magit
  :straight t
  :defer t
  :init
  (defun my/magit-version-override (&rest _) "4.4.2")
  (advice-add 'magit-version :override #'my/magit-version-override))
(use-package annotate
  :straight t
  :diminish (annotate-mode . "anno")
  :defer t)
(use-package neotree
  :straight t
  :defer t
  :bind (:map global-map
	      ("C-x D" . neotree-toggle))
  :config
  (setq neo-theme 'nerd-icons))	     	
;;;; useful
(use-package pangu-spacing
  :straight t
  :diminish (pangu-spacing-mode . "pangu")
  :after org org-roam 
  :hook (org-mode . pangu-spacing-mode))
(use-package tldr
  :straight t
  :commands (tldr)
  :config
  (setq tldr-enabled-categories '("common" "linux" "osx" "sunos")))
(use-package writeroom-mode
  :straight t
  :commands (writeroom-mode))
(use-package sdcv
  :straight t
  :defer t)
;; Guide to install emacs-reader, for Windows user with MSYS2 and straight.el
;; 1. Install MSYS2
;; 2. Open MSYS2 shell, usually MinGW64 or UCRT64,
;;    update packages first, run: pacman -Syu
;;    then run: pacman -S make mingw-w64-x86_64-gcc git pkg-config mingw-w64-x86_64-libmupdf
;;    or try them: mingw-w64-ucrt64-x86_64-[?] pkgconf
;; 3. Run: "cd ~/.emacs.d/straight/emacs-reader/repos/ && make all" to build.
;;    Tips: If you didn't install the right distribution of libmupdf,
;;          In the shell, "make all" command will tell you which version should be installed.
;; 4. Copy your built "reader-core.dll" by hand,
;;    from .../straight/repos/emacs-reader
;;    to   .../straight/builds/reader
;;    Run: "cp ~/emacs.d/straight/repos/emacs-reader/reader-core.dll \
;;             ~/emacs.d/straight/builds/reader"
(use-package reader
  :defer t
  :straight '(reader :type git :host codeberg :repo "divyaranjan/emacs-reader"
  		     :files ("*.el" "render-core.so")
  		     :pre-build ("make" "all")))
;;;; emms
(use-package mpvi
  :straight t
  :defer t)
(use-package emms
  :straight t
  :commands (emms emms-browser)
  :config
  (require 'emms-setup)
  (emms-all)
  (setq emms-player-list '(emms-player-mpv))
  (setq emms-source-file-default-directory "c:/music/") 
  (setq emms-source-file-directory-tree-function 'emms-source-file-directory-tree-internal)
  (setq emms-info-functions '(emms-info-native))
  ;;(setq emms-browser-covers '("Cover" "cover" "Folder" "folder")
  (require 'mpvi-emms)
  (mpvi-emms-integrated-mode 1))

;;;; major modes for other langs
(use-package ledger-mode
  :straight t
  :defer t)

;;;; defun my/...
;; AGENDA agenda-today
(defun my/org-agenda-today ()
  "My shortcut to org-agenda today with log mode on."
  (interactive)
  (org-agenda nil "a")
  (org-agenda-log-mode))
;; NOTES money
(defun my/ledger ()
  "My shortcut to my Ledger file."
    (interactive)                            
    (find-file "~/org/ledger/journal.ledger"))
;; QUICKS time-range
(defun my/org-insert-time-range ()
  "Choose a past time then insert the time range to now time."
  (interactive)
  (let* ((current-time (current-time))
	 (now-string (format-time-string "%Y-%m-%d %a %H:%M" current-time))
	 (chosen-time (org-read-date nil nil nil "Begin date:" nil nil t))
	 (chosen-parsed (parse-time-string chosen-time))
	 (chosen-formated (format-time-string "%Y-%m-%d %a %H:%M"
					      (encode-time chosen-parsed))))
    (insert (format "[%s]--[%s]" chosen-formated now-string))))
;;;; NON-INTERACTIVE defun my/...
(defun my/clear-echo-area-timer ()
  "Clear emacs startup hook after ? second, by outputting a blank message."
  (run-at-time "0 sec" nil (lambda ()
			     (message " "))))
(add-hook 'emacs-startup-hook #'my/clear-echo-area-timer)
;;;; keybindings
;; My Notes:
;;  1. "<esc>" is alternative "Meta" key, not safe to be touched or set as leader.
;;  2. Mode map should be in use-package sentence,"C-c C-?".
;;  3. Heavy tool or major mode should be with global map "C-c M-?" (project, magit).
;;  4. Helpful should replace "C-h ?".
;;  5. Word oriented operations should be builtin "M-?".
;;  6. File operation should be "C-x ?" "C-x C-?".
;;     a. "C-x ?"
;;        "C-x a" abbrev-mode, "C-x d" dired-mode
;;        "C-x b" switch-buffer, "C-x k" kill-buffer
;;     b. "C-x C-?"
;;        "C-x C-f" find-file, "C-x C-r" find-file-read-only <= "C-x C-q" permit-editing
;;        "C-x C-s" save-file, "C-x C-w" write-file
;;  7. a. "C-x o" other-window. "C-x +" balance-windows.
;;     b. "C-x ?(digit)":
;;        "C-x 0" quit now, "C-x 1" now.
;;        "C-x 2" open to bottom, C-x 3 open to right.
;;        "C-x 4" other-window + actions.
;;        "C-x 5" frame related actions.
;;        "C-x 6" 2C-two-columns related actions.
;;  8. "C-x <left>" "C-x <right>" is default of builtin commands "previous-buffer" and "next-buffer".
;;  9. "C-c <left>" "C-c <right>" is default of builtin winner-mode.
;; 10. "C-c ? ? ? ..." is completely safe.
;;     => leader.
(use-package cua-base
  :bind (:map global-map
	      ("C-RET" . cua-set-rectangle-mark)))
(use-package general
  :straight t
  :after which-key
  :defer t
  :config
  (general-create-definer leader
    :keymaps 'override
    :prefix "C-c")
  (leader
    ;; notes / roam node
    "n" '(:ignore t :which-key "📔NOTES")
    "nn" 'org-roam-node-find
    "nt" 'org-roam-dailies-goto-today
    "nc" 'org-roam-capture
    "nC" 'org-roam-dailies-capture-today
    "nm" 'my/ledger
    ;; consult
    "c" '(:ignore t :which-key "🔍CONSULT")
    "cr" 'consult-recent-file
    "cc" 'consult-line
    "cb" 'consult-buffer
    "cg" 'consult-ripgrep
    ;; agenda
    "a"  '(:ignore t :which-key "📅AGENDA")
    "aa" 'my/org-agenda-today
    ;; bookmark
    "b"  '(:ignore t :which-key "🔖BOOKMARK")
    "bb" 'bookmark-set
    "bj" 'bookmark-jump
    ;; emms
    "e" '(:ignore t :which-key "🎵EMMS")
    "ee" 'emms-browser
    "ep" 'emms-pause
    "eP" 'emms-previous
    "eN" 'emms-next
    ;; file
    "f"  '(:ignore t :which-key "📁FILE")
    "ff" 'find-file
    "fr" 'consult-recent-file
    "fm" 'make-directory
    ;; project
    "p"  '(:ignore t :which-key "🗄PROJECT")
    ;; "pc" 'my/project-create-project
    "pp" 'project-dired
    ;; quick
    "q" '(:ignore t :which-key "🧙QUICKS")
    "qt" 'my/org-insert-time-range
    "qc" 'insert-char
    ))
