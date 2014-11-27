(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-default-options (quote ("a4paper" "11pt" "twoside")))
 '(LaTeX-style-list
   (quote
    (("article")
     ("beamer")
     ("book")
     ("letter")
     ("memoir")
     ("minimal")
     ("report")
     ("scrartcl")
     ("scrbook")
     ("scrlttr2")
     ("scrreprt")
     ("slides"))))
 '(LaTeX-verbatim-environments (quote ("verbatim" "verbatim*" "lstlisting")))
 '(TeX-PDF-mode t)
 '(TeX-auto-save t)
 '(TeX-engine (quote luatex))
 '(TeX-parse-self t)
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-list
   (quote
    (("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline
-b -g %n %o %b"))))
 '(TeX-view-program-selection (quote ((output-pdf "Skim") (output-html "xdg-open"))))
 '(auto-insert-directory "~/.emacs.d/autoinsert/")
 '(auto-insert-query nil)
 '(auto-save-list-file-prefix "~/.emacs.d/var/autosaves/")
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/var/backups"))))
 '(blink-cursor-interval 0.4)
 '(color-theme-sanityinc-solarized-rgb-is-srgb t)
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(default-frame-alist (quote ((height . 50) (width . 100))))
 '(delete-by-moving-to-trash t)
 '(drag-stuff-global-mode t)
 '(electric-indent-mode t)
 '(flycheck-disabled-checkers (quote (rust)))
 '(flycheck-indication-mode (quote right-fringe))
 '(flycheck-temp-prefix ".flycheck")
 '(font-latex-fontify-sectioning (quote color))
 '(geiser-default-implementation (quote racket))
 '(geiser-mode-smart-tab-p t)
 '(global-paren-face-mode t)
 '(ido-auto-merge-work-directories-length -1)
 '(ido-cannot-complete-command (quote ido-completion-help))
 '(ido-create-new-buffer (quote always))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-ignore-files
   (quote
    ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\`\\.DS_Store")))
 '(ido-vertical-define-keys (quote C-n-C-p-up-down-left-right))
 '(indent-tabs-mode nil)
 '(inf-ruby-default-implementation "pry")
 '(inhibit-startup-screen t)
 '(iswitchb-mode nil)
 '(lisp-lambda-list-keyword-alignment t)
 '(markdown-indent-on-enter nil)
 '(menu-bar-mode t)
 '(ns-alternate-modifier (quote none))
 '(ns-right-command-modifier (quote meta))
 '(org-agenda-file-regexp "\\`\\([^.].*\\|\\d\\d\\d\\d-\\d\\d-\\d\\d\\'\\)\\.org")
 '(org-agenda-files (quote ("~/Documents/tasks.org" "~/Documents/journal/")))
 '(org-habit-completed-glyph 8226)
 '(org-habit-graph-column 40)
 '(org-habit-show-all-today t)
 '(org-hide-leading-stars t)
 '(org-journal-date-format "%A, %Y-%m-%d")
 '(org-journal-file-format "%Y-%m-%d.org")
 '(org-journal-time-format "%R  ")
 '(org-log-done (quote time))
 '(org-modules
   (quote
    (org-bibtex org-docview org-habit org-info org-mouse org-man)))
 '(org-support-shift-select t)
 '(org-tags-column -95)
 '(org-todo-keywords (quote ((sequence "TODO(t)" "|" "DONE(d!)"))))
 '(package-archives
   (quote
    (("marmalade" . "http://marmalade-repo.org/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/"))))
 '(powerline-buffer-size-suffix nil)
 '(powerline-default-separator nil)
 '(rbenv-modeline-function (quote rbenv--modeline-plain))
 '(rbenv-show-active-ruby-in-modeline t)
 '(recentf-auto-cleanup 300)
 '(recentf-exclude (quote ("~$" "\\\\.log$")))
 '(recentf-mode t)
 '(recentf-save-file "~/.emacs.d/var/recentf")
 '(ruby-block-highlight-face (quote paren-face-match))
 '(ruby-block-highlight-toggle (quote overlay))
 '(scroll-bar-mode nil)
 '(show-paren-style (quote parenthesis))
 '(show-smartparens-global-mode nil)
 '(size-indication-mode t)
 '(smartparens-global-mode t)
 '(smooth-scroll-margin 7)
 '(solarized-use-more-italic t)
 '(solarized-use-variable-pitch nil)
 '(sp-show-pair-from-inside t)
 '(tab-always-indent nil)
 '(tool-bar-mode nil)
 '(trash-directory "~/.Trash/emacs")
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(view-read-only t)
 '(whitespace-display-mappings
   (quote
    ((space-mark 160
                 [183])
     (newline-mark 13
                   [172])
     (tab-mark 9
               [8594 9]))))
 '(whitespace-space (quote default))
 '(writeroom-disable-fringe nil)
 '(writeroom-global-effects
   (quote
    (writeroom-toggle-fullscreen writeroom-toggle-alpha writeroom-toggle-menu-bar-lines writeroom-toggle-tool-bar-lines writeroom-toggle-vertical-scroll-bars)))
 '(writeroom-width 90))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :family "Ubuntu Mono"))))
 '(cursor ((t (:background "#df4"))))
 '(enh-ruby-op-face ((t (:inherit font-lock-keyword-face))) t)
 '(fixed-pitch ((t (:family "Ubuntu Mono"))))
 '(flycheck-color-mode-line-error-face ((t (:inherit flycheck-fringe-error))))
 '(flycheck-color-mode-line-info-face ((t (:inherit flycheck-fringe-info))))
 '(flycheck-color-mode-line-warning-face ((t (:inherit flycheck-fringe-warning))))
 '(flycheck-fringe-error ((t (:inherit error :foreground "#dc322f" :inverse-video nil))))
 '(flycheck-fringe-info ((t (:inherit success :foreground "#2aa198" :inverse-video nil))))
 '(flycheck-fringe-warning ((t (:inherit warning :foreground "#b58900" :inverse-video nil))))
 '(font-latex-sectioning-0-face ((t (:inherit font-latex-sectioning-1-face :height 1.1))) t)
 '(font-latex-sectioning-5-face ((t (:weight bold))) t)
 '(font-latex-slide-title-face ((t (:inherit font-lock-type-face :weight bold :height 1.2))) t)
 '(fringe ((t (:background "#0a2832" :foreground "#154a55"))))
 '(markdown-header-delimiter-face ((t (:inherit font-lock-function-name-face :weight normal))) t)
 '(mode-line ((t (:background "#e9e2cb" :inverse-video t :box (:line-width 1 :color "#52676f") :weight normal))))
 '(ns-working-text-face ((t (:inherit cursor))))
 '(org-tag ((t (:slant italic))))
 '(outline-1 ((t (:foreground "#cb4b16"))))
 '(outline-2 ((t (:foreground "#b58900"))))
 '(outline-3 ((t (:foreground "#2aa198"))))
 '(outline-4 ((t (:foreground "#268bd2"))))
 '(outline-5 ((t (:foreground "#6c71c4"))))
 '(outline-6 ((t nil)))
 '(outline-8 ((t nil)))
 '(rbenv-active-ruby-face ((t nil)))
 '(region ((t (:background "#154a55" :inverse-video nil))))
 '(show-paren-match ((t (:foreground "#df4" :inverse-video nil :weight extra-bold :background nil))))
 '(show-paren-mismatch ((t (:inherit show-paren-match :foreground "#c61b6e"))))
 '(sp-show-pair-match-face ((t (:inherit show-paren-match))) t)
 '(tex-verbatim ((t nil)) t)
 '(trailing-whitespace ((t (:background "#422" :underline nil)))))
