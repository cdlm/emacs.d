(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'cask "/opt/homebrew/opt/cask/cask.el")
(cask-initialize)
(require 'use-package)
(require 'cl-lib)

(use-package pallet
  :config (pallet-mode))

(use-package bind-key)

(defvar site-lisp-dir "~/.emacs.d/lisp")
(add-to-list 'load-path site-lisp-dir)

(use-package paren-face
  ;; dimmer parentheses
  :config (global-paren-face-mode))

(use-package vitamined-mode-line
  :config (vitamined/setup))

(use-package color-theme-sanityinc-solarized
  :if (display-graphic-p)
  :config (progn
            (color-theme-sanityinc-solarized 'dark)
            (color-theme-sanityinc-solarized--with-colors
             'dark
             (cl-flet ((box (c) `(:box (:line-width 3 :color ,c :style nil)))
                       (spec (s) (mapcar (lambda (fs) `(,(car fs) ((,class ,@(cdr fs)))))
                                         s)))
               (apply 'custom-theme-set-faces
                      (cons 'sanityinc-solarized-dark
                            (spec
                             `((cursor (:background "#df4")) ; I like it to flash
                               (parenthesis (:foreground ,faintest))
                               (popup-face (:foreground ,faint :background ,contrast-background))
                               (popup-menu-mouse-face (:foreground ,background :inherit (popup-face)))
                               (popup-menu-selection-face (:foreground ,contrast-background :background ,cyan))

                               (mode-line (,@(box contrast-background) :foreground ,normal :background ,contrast-background))
                               (mode-line-inactive (,@(box alt-background) :foreground ,strong :background ,alt-background :inherit (mode-line)))
                               (mode-line-highlight (:underline (:style line)))
                               (mode-line-emphasis (:weight bold :inherit t))
                               (mode-line-buffer-id (,@(box green) :foreground ,base3 :background ,green))

                               (vitamined-mode-line-directory-face (:foreground ,background))
                               (vitamined-mode-line-readonly-face (,@(box red) :foreground ,contrast-background :background ,red))
                               (vitamined-mode-line-modified-face (,@(box orange) :foreground ,contrast-background :background ,orange))
                               (vitamined-mode-line-narrowed-face (,@(box cyan) :foreground ,contrast-background :background ,cyan))
                               (vitamined-mode-line-position-face (:foreground ,cyan))
                               (vitamined-mode-line-mode-face (:foreground ,cyan))
                               (vitamined-mode-line-minor-mode-face (:foreground ,normal))
                               (vitamined-mode-line-process-face (:foreground ,violet))
                               ))))))))

(use-package anzu
  ;; number of search matches
  :diminish anzu-mode
  :config (global-anzu-mode))

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
  :diminish auto-complete-mode
  :config (progn
            (use-package auto-complete-exuberant-ctags
              :config (ac-exuberant-ctags-setup))
            (use-package ac-ispell
              :config (ac-ispell-setup))
            (use-package ac-html
              :defer t
              :init (progn
                        (add-hook 'html-mode-hook 'ac-html-enable)
                        (add-hook 'haml-mode-hook 'ac-haml-enable)))
            (use-package auto-complete-c-headers)
            (use-package auto-complete-chunk)
            (use-package auto-complete-clang)))

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
  :config (progn
            (use-package magit-push-remote ; should be integrated in magit
              :config (add-hook 'magit-mode-hook 'magit-push-remote-mode))
            (use-package magit-svn
              :diminish magit-svn-mode
              :config (add-hook 'magit-mode-hook 'turn-on-magit-svn))
            (use-package magit-gitflow
              :config (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))))

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
  :idle (progn
          (use-package flycheck-ledger)
          (use-package flycheck-cask
            :config (add-hook 'flycheck-mode-hook 'flycheck-cask-setup))
          (use-package flycheck-haskell
            :config (add-hook 'flycheck-mode-hook 'flycheck-haskell-setup))
          (use-package flycheck-rust
            :config (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))
          (global-flycheck-mode)))

(use-package flyspell
  :config (progn
            (use-package auto-dictionary
              :config (add-hook 'flyspell-mode-hook 'auto-dictionary-mode))
            (use-package flyspell-lazy
              :config (flyspell-lazy-mode))))

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
  :mode ("\\.\\(tex\\|sty\\|cls\\)\\'" . TeX-latex-mode)
  :config (progn
            (use-package auto-complete-auctex)
            (use-package auctex-latexmk
              :config (auctex-latexmk-setup))
            (add-hook 'LaTeX-mode-hook
                      (lambda ()
                        (visual-line-mode)
                        (flyspell-mode)
                        (ac-ispell-ac-setup)))))

(use-package pillar
  :mode ("\\.\\(pier\\|pillar\\)\\'" . pillar-mode)
  :config (flyspell-mode))

(use-package markdown-mode
  :mode "\\.\\(md\\|markdown\\)\\'"
  :config (progn
            (use-package pandoc-mode)
            (add-hook 'markdown-mode-hook
                      (lambda ()
                        (conditionally-turn-on-pandoc)
                        (visual-line-mode)
                        (flyspell-mode)
                        (ac-ispell-ac-setup)))))

(use-package lua-mode)

(use-package ruby-mode
  :mode (("\\.\\(gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|thor\\)\\'" . ruby-mode)
         ("Gemfile\\(\\.lock\\)?\\|\\(Cap\\|Guard\\|[rR]ake\\|Vagrant\\)file\\'" . ruby-mode))
  :config (progn
            (use-package robe :diminish robe-mode)
            (use-package yard-mode :diminish yard-mode)
            (use-package ruby-block :diminish ruby-block-mode)
            (use-package ruby-interpolation :diminish ruby-interpolation-mode)
            (setq robe-highlight-capf-candidates nil)
            (add-hook 'ruby-mode-hook
                      (lambda ()
                        (abbrev-mode -1) ; buggy with enh-ruby-mode, used to indent after `end`
                        (electric-spacing-mode)
                        (ruby-block-mode)
                        (ruby-interpolation-mode)
                        (yard-mode)
                        (robe-mode)))
            (use-package ac-inf-ruby
              :config (progn
                        (add-to-list 'ac-modes 'inf-ruby-mode)
                        (add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable)))))

(use-package rbenv
  :config (global-rbenv-mode))

(use-package feature-mode
  ;; cucumber feature files
  :mode "\\.feature\\'")

(use-package lisp-mode
  :config (progn
            (add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode)) ; why is this not default?
            (dolist (hook '(lisp-mode-hook
                            lisp-interaction-mode-hook
                            emacs-lisp-mode-hook))
              (add-hook hook 'highlight-parentheses-mode))))

(use-package haskell-mode
  :mode "\\.l?hs\\'")

(use-package geiser                   ; list/scheme/racket interaction
  :config (use-package ac-geiser
            :config (progn
                      (add-hook 'geiser-mode-hook 'ac-geiser-setup)
                      (add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
                      (add-to-list 'ac-modes 'geiser-repl-mode))))

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

(use-package unfill)

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

(use-package org
  :config (progn
            (use-package org-habit)
            (use-package org-journal
              :config (bind-key* "C-c C-j" 'org-journal-new-entry))
            (use-package org-ac
              :config (org-ac/config-default))
            (bind-key* "C-c C-a" 'org-agenda)
            (bind-keys :map org-mode-map
                       ("s-C-<up>" . org-move-subtree-up)
                       ("s-C-<down>" . org-move-subtree-down))))

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
 ("s-]" . shift-right)
 ("S-s-<left>"  . windmove-left)
 ("S-s-<right>" . windmove-right)
 ("S-s-<up>"    . windmove-up)
 ("S-s-<down>"  . windmove-down))
