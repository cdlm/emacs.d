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
 '(drag-stuff-global-mode t)
 '(electric-indent-mode t)
 '(flycheck-disabled-checkers (quote (rust)))
 '(flycheck-indication-mode (quote right-fringe))
 '(flycheck-temp-prefix ".flycheck")
 '(font-latex-fontify-sectioning (quote color))
 '(global-paren-face-mode t)
 '(ido-cannot-complete-command (quote ido-completion-help))
 '(ido-create-new-buffer (quote always))
 '(ido-vertical-define-keys (quote C-n-C-p-up-down-left-right))
 '(indent-tabs-mode nil)
 '(inf-ruby-default-implementation "pry")
 '(inhibit-startup-screen t)
 '(iswitchb-mode nil)
 '(markdown-indent-on-enter nil)
 '(menu-bar-mode t)
 '(ns-alternate-modifier (quote none))
 '(ns-right-command-modifier (quote meta))
 '(org-hide-leading-stars t)
 '(org-support-shift-select t)
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
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
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
 '(rbenv-active-ruby-face ((t nil)))
 '(region ((t (:background "#154a55" :inverse-video nil))))
 '(show-paren-match ((t (:foreground "#df4" :inverse-video nil :weight extra-bold :background nil))))
 '(show-paren-mismatch ((t (:inherit show-paren-match :foreground "#c61b6e"))))
 '(sp-show-pair-match-face ((t (:inherit show-paren-match))) t)
 '(tex-verbatim ((t nil)) t)
 '(trailing-whitespace ((t (:background "#422" :underline nil)))))
