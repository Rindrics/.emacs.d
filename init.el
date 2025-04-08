;; (require 'profiler)
;; (profiler-start 'cpu)

;; Temporally Maximize GC
(setq gc-cons-threshold most-positive-fixnum)

(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
	  (expand-file-name
	   (file-name-directory (or load-file-name byte-compile-current-file))))))

;; Initialize package manager for compile time
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
		       ("melpa" . "https://melpa.org/packages/")
		       ("gnu"   . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  ;; Leaf keywords
  (leaf leaf-keywords
    :doc "Use leaf as a package manager"
    :url "https://github.com/conao3/leaf.el"
    :ensure t
    :init
    (leaf hydra :ensure t)
    (leaf major-mode-hydra
      :doc "Use pretty-hydra to define template easily"
      :url "https://github.com/jerrypnz/major-mode-hydra.el"
      :ensure t
      :require pretty-hydra)
    :config
    (leaf-keywords-init)))

;; Compile
(eval-and-compile
  (leaf *byte-compile
    :custom
    (byte-compile-warnings  . '(not free-vars docstrings lexical unresolved constants))
    (warning-suppress-types . '(comp))
    (debug-on-error         . nil)))

;; Package Manager
(leaf package-utils
  :doc "Interactive package manager"
  :url "https://github.com/Silex/package-utils"
  :ensure t)

;; -------------------------------------
;; Generic Configurations
;; -------------------------------------

;; See http://rindrics.com/emacs/keep-tidy/
(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :custom
  `((custom-file . ,(locate-user-emacs-file "custom.el"))))
(leaf no-littering
  :doc "Keep .emacs.d/ clean"
  :url "https://github.com/emacscollective/no-littering"
  :ensure t)

;; See http://rindrics.com/emacs/backup/
(leaf backup
  :custom
  (make-backup-files . nil)
  (auto-save-default . nil))

;; See http://rindrics.com/emacs/magit/
(leaf magit
  :doc "Complete text-based user interface to Git"
  :url "https://magit.vc/"
  :ensure t
  :init
  (setq magit-auto-revert-mode nil))

(leaf *hydra-git
  :bind
  ("M-g" . *hydra-git/body)
  :pretty-hydra
  ((:title " Git" :color blue :quit-key "q" :foreign-keys warn :separator "╌")
   ("Basic"
    (("w" magit-checkout "checkout")
     ("s" magit-status "status")
     ("b" magit-branch "branch")
     ("F" magit-pull "pull")
     ("f" magit-fetch "fetch")
     ("A" magit-apply "apply")
     ("c" magit-commit "commit")
     ("P" magit-push "push"))
    ""
    (("d" magit-diff "diff")
     ("l" magit-log "log")
     ("r" magit-rebase "rebase")
     ("z" magit-stash "stash")
     ("!" magit-run "run shell command")
     ("y" magit-show-refs "references")))))

(leaf *recentf
  :doc "Record open files history"
  :global-minor-mode recentf-mode
  :bind
  (("C-x C-r" . recentf-open))
  :custom
  (recentf-max-saved-items . 20000)
  (recentf-max-menu-items  . 20000)
  (recentf-auto-cleanup    . 'never)
  (recentf-exclude . '((expand-file-name package-user-dir)
		       "recentf"
		       "COMMIT_EDITMSG"
       )))

(leaf vertico
  :doc "Completion interface"
  :ensure t
  :init (vertico-mode)
  :custom
  (vertico-cycle . t)
  (vertico-count . 18))

(leaf vertico-directory
  :doc "Extensions for file directory navigation"
  :after vertico
  :ensure nil
  :preface
  (defun vertico-directory-enter-or-dired ()
    "Enter directory or open it in dired if it's the current selection."
    (interactive)
    (let* ((current (vertico--candidate))
	   (cand (minibuffer-completion-contents)))
      (if (or (string-suffix-p "/" current)
	      (and (not (string-suffix-p "/" cand))
		   (file-directory-p current)))
	  (progn
	    (vertico-exit)
	    (dired current))
	(vertico-directory-enter))))
  :init
  (with-eval-after-load 'vertico
    (define-key vertico-map (kbd "RET") #'vertico-directory-enter-or-dired)
    (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
    (define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)))

(leaf orderless
  :doc "Completion style for regexp matching"
  :ensure t
  :custom
  (completion-styles . '(orderless))
  (completion-category-defaults . nil))

;; Font Size checker
;;
;; |∞≤≥∏∑∫ ×±⊆⊇|
;; |αβγδεζ ηθικλu|
;; |abcdef ghijkl|
;; |ABCDEF GHIJKL|
;; |'";:-+ =/\~`?|
;; |日本語 の美観|
;; |あいう えおか|
;; |アイウ エオカ|
;; |ｱｲｳｴｵｶ ｷｸｹｺｻｼ|
;;
;; | hoge                 | hogeghoe | age               |
;; |----------------------+----------+-------------------|
;; | 今日もいい天気ですね | お、     | 等幅になった :+1: |


(leaf font
 :doc "Font settings with precise Unicode ranges"
 :if window-system
 :preface
 (defun set-japanese-font (family)
   (set-fontset-font (frame-parameter nil 'font) 'japanese-jisx0208 (font-spec :family family))
   (set-fontset-font (frame-parameter nil 'font) 'japanese-jisx0212 (font-spec :family family))
   (set-fontset-font (frame-parameter nil 'font) 'katakana-jisx0201 (font-spec :family family)))

 (defun set-latin-and-greek-font (family)
   (set-fontset-font (frame-parameter nil 'font) '(#x0250 . #x02AF) (font-spec :family family))
   (set-fontset-font (frame-parameter nil 'font) '(#x00A0 . #x00FF) (font-spec :family family))
   (set-fontset-font (frame-parameter nil 'font) '(#x0100 . #x017F) (font-spec :family family))
   (set-fontset-font (frame-parameter nil 'font) '(#x0180 . #x024F) (font-spec :family family))
   (set-fontset-font (frame-parameter nil 'font) '(#x2018 . #x2019) (font-spec :family family))
   (set-fontset-font (frame-parameter nil 'font) '(#x2588 . #x2588) (font-spec :family family))
   (set-fontset-font (frame-parameter nil 'font) '(#x2500 . #x2500) (font-spec :family family))
   (set-fontset-font (frame-parameter nil 'font) '(#x2504 . #x257F) (font-spec :family family))
   (set-fontset-font (frame-parameter nil 'font) '(#x0370 . #x03FF) (font-spec :family family)))
 :custom
 (use-default-font-for-symbols . nil)
 (inhibit-compacting-font-caches . t)
 (jp-font-family . "SF Mono Square")
 (default-font-family . "Hack Nerd Font")
 :config
 (when (eq system-type 'darwin)
   (set-face-attribute 'default nil :family jp-font-family :height 140))
 (set-japanese-font jp-font-family)
 (set-frame-font jp-font-family nil t)
 (set-latin-and-greek-font default-font-family)
 (add-to-list 'face-font-rescale-alist (cons default-font-family 0.86))
 (add-to-list 'face-font-rescale-alist (cons jp-font-family 1.0)))

;; -----------------------------------------------------------------------------------------
;;
;; MacOS
;;
;; -----------------------------------------------------------------------------------------

(leaf exec-path-from-shell
  :doc "Share PATH from shell environment variables"
  :url "https://github.com/purcell/exec-path-from-shell"
  :ensure t
  :if (and (equal system-type 'darwin) (window-system))
  :custom
  (exec-path-from-shell-check-startup-files . nil)
  (exec-path-from-shell-variables . '("PATH" "GOPATH" "LANG"))
  :init
  (setq exec-path  (parse-colon-path (string-trim-right (shell-command-to-string "echo $PATH"))))
  (setenv "PATH"   (string-trim-right (shell-command-to-string "echo $PATH")))
  (setenv "GOPATH" (string-trim-right (shell-command-to-string "echo $GOPATH")))
  :config
  (exec-path-from-shell-initialize))

;; -----------------------------------------------------------------------------------------
;;
;; Brackets Guide
;;
;; -----------------------------------------------------------------------------------------

(leaf smartparens
  :ensure t
  :require smartparens-config
  :global-minor-mode smartparens-global-mode
  :bind
  (:smartparens-mode-map
   ("M-<DEL>" . sp-backward-unwrap-sexp)
   ("M-]"     . sp-up-sexp)
   ("M-["     . sp-down-sexp)
   ("C-("     . sp-beginning-of-sexp)
   ("C-)"     . sp-end-of-sexp)
   ("C-M-f"   . sp-forward-sexp)
   ("C-M-b"   . sp-backward-sexp)
   ("C-M-n"   . sp-next-sexp)
   ("C-M-p"   . sp-previous-sexp))
  :config
  (sp-local-pair 'org-mode "*" "*")
  (sp-local-pair 'org-mode "=" "=")
  (sp-local-pair 'org-mode "~" "~")
  (sp-local-pair 'org-mode "+" "+"))

;; -----------------------------------------------------------------------------------------
;;
;; Programming Mode
;;
;; -----------------------------------------------------------------------------------------

(leaf yasnippet
  :doc "Template system"
  :url "https://github.com/joaotavora/yasnippet"
  :ensure t
  :hook   ((after-init-hook . yas-reload-all)
           (prog-mode-hook  . yas-minor-mode))
  :custom (yas-snippet-dirs . `(,(expand-file-name "snippets" user-emacs-directory))))

(leaf lsp-bridge
  :doc "fast LSP client"
  :vc (:url "https://github.com/manateelazycat/lsp-bridge")
  :require t
  :init (global-lsp-bridge-mode)
  :custom
  (acm-enable-tabnine                 . nil)
  (acm-enable-copilot                 . t)
  (acm-enable-quick-access            . t)
  (lsp-bridge-enable-hover-diagnostic . t)
  (acm-backend-yas-candidates-number  . 5))

;; Go
(leaf go-mode
  :doc "Go development environment"
  :url "https://github.com/dominikh/go-mode.el"
  :if (executable-find "go")
  :ensure t
  :mode "\\.go\\'"
  :custom
  (gofmt-command . "goimports")
  :hook
  (go-mode-hook
   . (lambda ()
       (add-hook 'before-save-hook #'gofmt-before-save t t)))
 :bind
  (:go-mode-map
   ("C-c C-n" . go-run)
   ("C-c C-m" . consult-make-in-project-root)))
;; Python
(leaf python
  :doc "Python development environment"
  :url "https://wiki.python.org/moin/EmacsPythonMode"
  :mode ("\\.py\\'" . python-mode))

;; -----------------------------------------------------------------------------------------
;;
;; Configuration Language
;;
;; -----------------------------------------------------------------------------------------

(leaf yaml-mode
  :doc "Major mode for editing files in the YAML data serialization format"
  :url "https://github.com/yoshiki/yaml-mode"
  :ensure t
  :mode ("\\.yaml\\'" "\\.yml\\'" "\\.rule\\'") ; .rule is a file for Prometheus alert manager
  :defvar yaml-file-threshold
  :custom
  (lsp-yaml-schemas . '(:file:///Users/aigarash/zap-standalone/all.json "/*.yaml"))
  (yaml-file-threshold . 100000)
  :hook
  (yaml-mode-hook
   . (lambda ()
       (if (< (buffer-size (current-buffer)) yaml-file-threshold)
           (progn
             (if (window-system)
                 (highlight-indent-guides-mode))
             (yas-minor-mode)
             (lsp-deferred)))))
  :custom-face
  (font-lock-variable-name-face . '((t (:foreground "violet")))))

(leaf doom-themes
  :doc "Megapack of themes"
  :url "https://github.com/doomemacs/themes"
  :ensure t
  :require (doom-dracula-theme doom-themes-ext-org)
  :init (setq custom-safe-themes t)
  :defer-config
  (let ((display-table (or standard-display-table (make-display-table))))
    (set-display-table-slot display-table 'vertical-border (make-glyph-code ?│))
    (setq standard-display-table display-table))
  :config
  (load-theme 'doom-dracula)
  (doom-themes-org-config)
  :custom-face
  (hl-line         . '((t (:background "#3B4252" :extend t ))))
  (vertical-border . '((t (:background "#282a36" :foreground "#1E2029")))))

;; -----------------------------------------------------------------------------------------
;;
;; Plain text
;;
;; -----------------------------------------------------------------------------------------
(setq sentence-end "[。？！.!?][]\"')}]*\\($\\|\t\\| \\)[ \t\n]*")
(leaf markdown-mode
  :doc "Major mode for editing Markdown-formatted text"
  :url "https://github.com/jrblevin/markdown-mode"
  :ensure t
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'"       . markdown-mode))
  :bind
  (("M-<up>" . markdown-move-subtree-up)
   ("M-<down>" . markdown-move-subtree-down)
   ("S-M-<right>" . markdown-demote-subtree)
   ("S-M-<left>" . markdown-promote-subtree)))

(setq org-startup-indented t)
(leaf org-theme
  :doc "Theme for org-mode"
  :custom
  (org-todo-keyword-faces
   . '(("WAIT" . (:foreground "#6272a4" :weight bold :width condensed))
       ("NEXT" . (:foreground "#f1fa8c" :weight bold :width condensed))))
  :custom-face
  ;; general
  (org-level-1         . '((t (:inherit outline-1 :weight normal :height 1.2))))
  (org-level-2         . '((t (:inherit outline-2 :weight normal))))
  (org-level-3         . '((t (:inherit outline-3 :weight normal))))
  (org-level-4         . '((t (:inherit outline-4 :weight normal))))
  (org-level-5         . '((t (:inherit outline-5 :weight normal))))
  (org-level-6         . '((t (:inherit outline-6 :weight normal))))
  (org-link            . '((t (:foreground "#bd93f9" :underline t :weight normal))))
  (org-list-dt         . '((t (:foreground "#bd93f9"))))
  (org-checkbox-statistics-todo . '((t (:foreground "#6272a4" :weight normal :height 0.9))))
  (org-checkbox-statistics-done . '((t (:foreground "#44475a" :weight normal :height 0.9))))
  (org-document-title  . '((t (:foreground "#61bfff" :weight normal :height 1.0))))
  (org-document-info-keyword   . '((t (:foreground "#6272a4" :height 1.0))))
  (org-document-info   . '((t (:foreground "#6272a4" :height 1.0))))
  (org-meta-line       . '((t (:foreground "#44475a" :height 1.0))))
  (org-footnote        . '((t (:foreground "#76e0f3"))))
  (org-agenda-calendar-event . '((t (:foreground "#8995ba"))))
  (org-special-keyword . '((t (:foreground "#6272a4"))))
  (org-drawer          . '((t (:foreground "#44475a"))))
  (org-checkbox        . '((t (:foreground "#bd93f9"))))
  (org-tag             . '((t (:foreground "#6272a4"))))
  (org-date            . '((t (:foreground "#8995ba" :height 0.9 :weight light :width condensed))))
  (org-priority        . '((t (:foreground "#f1fa8c")))) ; #ebe087
  (org-todo            . '((t (:foreground "#ad92dd" :weight bold :width condensed))))
  (org-done            . '((t (:background "#282a36" :foreground "#3b216a" :strike-through nil :weight bold :width condensed)))))

(leaf org-modern
  :doc "To Be Modern Looks"
  :url "https://github.com/minad/org-modern"
  :ensure t
  :hook (org-mode-hook . org-modern-mode)
  :custom
  (org-modern-star           . 'replace)
  (org-modern-hide-stars     . " ")
  (org-modern-progress       . nil)
  (org-modern-tag            . nil)
  (org-modern-todo           . t)
  (org-modern-priority       . t)
  (org-modern-statistics     . nil)
  (org-modern-timestamp      . nil)
  (org-modern-block-name     . nil)
  (org-modern-block-fringe   . nil)
  (org-modern-table-vertical . 1)
  (org-modern-variable-pitch . t)
  ;; todo faces
  (org-modern-todo-faces
   . (quote (("TODO" :background "#673AB7" :foreground "#f8f8f2")
	     ("WAIT" :background "#6272a4" :foreground "#f8f8f2")
	     ("NEXT" :background "#f1fa8c" :foreground "#1E2029") ; #bd93f9
	     ("DONE" :background "#373844" :foreground "#b0b8d1"))))
  ;; use nerd font icons
  (org-modern-list           . '((?+ . "◦") (?- . "–") (?* . "•")))
  (org-modern-checkbox       . '((?X . "") (?- . "") (?\s . "")))
  (org-modern-priority       . '((?A . "") (?B . "") (?C . "")))
  ;;(org-modern-replace-stars  . "")
  (org-modern-replace-stars  . "")
  :custom-face
  (org-modern-date-active   . '((t (:background "#373844" :foreground "#f8f8f2" :height 0.75 :weight light :width condensed))))
  (org-modern-time-active   . '((t (:background "#44475a" :foreground "#f8f8f2" :height 0.75 :weight light :width condensed))))
  (org-modern-date-inactive . '((t (:background "#373844" :foreground "#b0b8d1" :height 0.75 :weight light :width condensed))))
  (org-modern-time-inactive . '((t (:background "#44475a" :foreground "#b0b8d1" :height 0.75 :weight light :width condensed))))
  (org-modern-tag           . '((t (:background "#44475a" :foreground "#b0b8d1" :height 0.75 :weight light :width condensed))))
  (org-modern-statistics    . '((t (:foreground "violet" :weight light)))))

(leaf org-bullets
  :ensure t
  :require t
  :custom (org-bullets-bullet-list . '("" "" "" "" "" "" "" "" "" ""))
  :hook (org-mode-hook . (lambda () (org-bullets-mode 1))))

;; (profiler-report)
;; (profiler-stop)

(setq gc-cons-threshold 16777216) ; 16mb
