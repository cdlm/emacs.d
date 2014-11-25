(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'cask "/opt/homebrew/opt/cask/cask.el")
(cask-initialize)
(require 'use-package)

(use-package pallet
  :config (pallet-mode))

(use-package bind-key)

(use-package color-theme-sanityinc-solarized
  :if (display-graphic-p)
  :config (progn
            (use-package paren-face
              ;; dimmer parentheses
              :config (global-paren-face-mode))
            (load-theme 'sanityinc-solarized-dark)
            (set-face-background 'cursor "#df4") ; I like it to flash
            (color-theme-sanityinc-solarized--with-colors
             'dark
             (set-face-foreground 'parenthesis faintest))))

(use-package powerline
  ;; colorful modeline
  :config (progn
            (use-package anzu ;; number of search matches
              :diminish anzu-mode
              :config (global-anzu-mode))
            (powerline-default-theme)))

(use-package smooth-scrolling
  ;; scrolling without jumps
  :idle)

(use-package evil
  :commands evil-mode
  :config (progn
            (use-package evil-leader)
            (use-package evil-matchit)
            (use-package evil-paredit)
            (use-package evil-surround)))

(use-package drag-stuff
  :diminish drag-stuff-mode
  :config (progn
            (setq drag-stuff-modifier '(super control))
            (add-to-list 'drag-stuff-except-modes 'org-mode)
            (add-to-list 'drag-stuff-except-modes 'rebase-mode)
            (drag-stuff-global-mode)))

(use-package expand-region
  ;; select around cursor, C-- C-= to contract
  :bind ("C-=" . er/expand-region))

(use-package yasnippet
  :diminish yas-minor-mode
  :config (progn
            (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
            (yas-global-mode)))

(use-package auto-complete
  :config (use-package auto-complete-exuberant-ctags
            :config (ac-exuberant-ctags-setup)))

(use-package smart-tab
  :diminish smart-tab-mode
  :idle (progn
          (global-smart-tab-mode)
          (add-to-list 'smart-tab-disabled-major-modes 'help-mode)
          (add-to-list 'smart-tab-disabled-major-modes 'Custom-mode)))

(use-package dired-x
  :commands dired-omit-mode
  :init (add-hook 'dired-mode-hook 'dired-omit-mode)
  :config (setq dired-omit-extensions
                (append dired-omit-extensions
                        '("aux" "blg" "fdb_latexmk" "fls" "lof" "log" "lol" "lot" "toc"))))

(use-package magit
  :bind ("C-x g" . magit-status)
  :diminish magit-auto-revert-mode
  :idle (progn
          (use-package magit-svn
            :diminish magit-svn-mode)
          (add-hook 'magit-mode-hook 'turn-on-magit-svn)))

(use-package git-gutter
  ;; show diff hunks in gutter + stage/unstage from buffer
  :diminish git-gutter-mode
  :idle (global-git-gutter-mode)
  :config (progn
            (bind-keys
             ("C-x C-g C-n" . git-gutter:next-hunk)
             ("C-x C-g C-p" . git-gutter:previous-hunk)
             ("C-x C-g C-s" . git-gutter:stage-hunk)
             ("C-x C-g C-r" . git-gutter:revert-hunk))))

(use-package flycheck
  :diminish flycheck-mode
  :idle (progn
          (use-package flycheck-ledger)
          (use-package flycheck-cask
            :config (add-hook 'flycheck-mode-hook 'flycheck-cask-setup))
          (use-package flycheck-haskell
            :config (add-hook 'flycheck-mode-hook 'flycheck-haskell-setup))
          (use-package flycheck-rust
            :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
          (global-flycheck-mode)))

(use-package ido
  :config (progn
            (use-package flx-ido)
            (use-package ido-vertical-mode :init (ido-vertical-mode))
            (use-package ido-ubiquitous :init (ido-ubiquitous-mode)))
  :init (ido-mode 'both))

(use-package smex
  :config (or (boundp 'smex-cache)
              (smex-initialize))
  ;; M-x interface with colors and completion
  :bind ("M-x" . smex))

(use-package recentf
  ;; recent files
  :config (use-package recentf-ext))


(use-package hl-line
  ;; visible current line
  :config (global-hl-line-mode))

(use-package ethan-wspace ;; intelligent showing / autocleaning of whitespace
  :disabled t
  :diminish ethan-wspace-mode)

(use-package paren
  :config (show-paren-mode))

(use-package highlight-parentheses
  :commands highlight-parentheses-mode
  :diminish highlight-parentheses-mode)

;; (use-package smartparens :diminish smartparens-mode)
(use-package autopair
  :diminish autopair-mode
  :config (autopair-mode))

(use-package browse-url
  :bind ("s-<mouse-1>" . browse-url-at-mouse))

(use-package mmm-auto
  :config (progn
            (setq mmm-global-mode 'maybe)
            (mmm-add-classes
             '((eruby :submode ruby-mode
                      ;; :face mmm-declaration-submode-face
                      :front "<%[=]?" :back "%>")))
            (mmm-add-mode-ext-class 'sgml-mode "\\.rhtml$" 'eruby)))

(use-package apache-mode
  :mode (("\\.htaccess\\'" . apache-mode)
         ("httpd\\.conf\\'" . apache-mode)
         ("vhost\\.conf\\'" . apache-mode)
         ("srm\\.conf\\'" . apache-mode)
         ("access\\.conf\\'" . apache-mode)
         ("sites-\\(available\\|enabled\\)/" . apache-mode)))

(use-package tex-site ;; auctex
  :mode ("\\.\\(tex\\|sty\\|cls\\)\\'" . latex-mode)
  :config (progn
            (use-package auto-complete-auctex)
            (use-package auctex-latexmk
              :config (auctex-latexmk-setup))))

(use-package pillar
  :mode ("\\.\\(pier\\|pillar\\)\\'" . pillar-mode))

(use-package markdown-mode
  :mode "\\.\\(md\\|markdown\\)\\'"
  :config (progn
            (use-package pandoc-mode
              :config (add-hook 'markdown-mode-hook 'turn-on-pandoc))))

(use-package ruby-mode
  :mode (("\\.\\(gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|thor\\)\\'" . ruby-mode)
         ("\\Gemfile\\(\\.lock\\)?\\|\\(Cap\\|Guard\\|[rR]ake\\|Vagrant\\)file\\'" . ruby-mode))
  :config (progn
            (use-package robe :diminish robe-mode)
            (use-package yard-mode :diminish yard-mode)
            (use-package ruby-block :diminish ruby-block-mode)
            (use-package ruby-interpolation :diminish ruby-interpolation-mode)
            (setq robe-highlight-capf-candidates nil)
            (setq enh-ruby-check-syntax nil)
            (add-hook 'ruby-mode-hook
                      (lambda ()
                        (abbrev-mode -1) ; buggy with enh-ruby-mode, used to indent after `end`
                        (electric-spacing-mode)
                        (ruby-block-mode)
                        (ruby-interpolation-mode)
                        (yard-mode)
                        (robe-mode)))))

(use-package rbenv
  :config (global-rbenv-mode))

(use-package feature-mode
  ;; cucumber feature files
  :mode "\\.feature\\'")

(use-package lisp-mode
  :config (dolist (hook '(lisp-mode-hook
                          lisp-interaction-mode-hook
                          emacs-lisp-mode-hook))
            (add-hook hook 'highlight-parentheses-mode)))

(use-package haskell-mode
  :commands haskell-mode
  :mode "\\.l?hs\\'"
  :config (use-package hs-lint))

(use-package geiser)                    ; list/scheme/racket interaction

(use-package perl-mode
  :mode "\\.?latexmkrc\\'")

;;;
;;; interesting packages to try someday
;;;

(use-package smart-newline
  ;; newlines and indent at block ends
  :config (smart-newline-mode))

(use-package smart-indent-rigidly
  ;; better indent for markdown?
  :disabled t)

(use-package electric
  :config (electric-indent-mode))

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package yaml-mode
  :mode "\\.yml\\'"
  :diminish ansible-doc
  :config (progn
            (use-package ansible)
            (use-package ansible-doc)
            (add-hook 'yaml-mode-hook
                      (lambda ()
                        (bind-key "C-m" 'newline-and-indent yaml-mode-map)
                        (ansible)
                        (ansible-doc-mode)))))

(use-package haskell-mode)

(use-package ledger-mode
  :mode "\\.ledger\\'")

(use-package uncrustify :disabled t)

(use-package autoinsert
  :config (progn
            (add-hook 'find-file-hook 'auto-insert)
            (define-auto-insert
              '("Gemfile\\'" . "Rubygem / Bundler dependencies")
              "Gemfile")))

;;;
;;; utility definitions and adhoc configuration
;;;

(defun add-to-executable-path (path)
  (let ((expanded-path (expand-file-name path)))
        (add-to-list 'exec-path expanded-path)
        (setenv "PATH" (concat expanded-path ":" (getenv "PATH")))))


(when (display-graphic-p)
  ;; GUI settings
  (add-hook 'after-init-hook 'server-start)
  ;; OS X apps launch from / and don't inherit the user's shell environment
  (cd "~")
  (mapc 'add-to-executable-path
        (list
         "/opt/texlive/2014/bin/x86_64-darwin"
         "/opt/homebrew/bin")))


(when (not (display-graphic-p))
  ;; terminal settings
  (menu-bar-mode -1))

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

(delete-selection-mode)
(setq-default indent-tabs-mode nil)

;;;
;;; own editing functions
;;;

(unbind-key "C-x .") ;; who cares about set-fill-prefix

(defun region-or-line-beginning ()
  (if (use-region-p) (region-beginning) (line-beginning-position)))

(defun region-or-line-end ()
  (if (use-region-p) (region-end) (line-end-position)))

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if
there's no active region.

http://stackoverflow.com/a/9697222/63112"
  (interactive)
  (comment-or-uncomment-region (region-or-line-beginning)
                               (region-or-line-end)))

(bind-key "s-/" 'comment-or-uncomment-region-or-line)

(defun shift-region-or-line (distance)
  "Shift either the active region or the current line by distance.
Shift right if distance is positive, left if negative."
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-or-line-beginning)
                      (region-or-line-end)
                      distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun shift-right ()
  (interactive)
  (shift-region-or-line 1))

(defun shift-left ()
  (interactive)
  (shift-region-or-line -1))

(bind-keys
 ("s-[" . shift-left)
 ("s-]" . shift-right))
