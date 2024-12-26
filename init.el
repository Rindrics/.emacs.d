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
    (define-key vertico-map (kbd "/") #'vertico-directory-enter)
    (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
    (define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)))

(leaf orderless
  :doc "Completion style for regexp matching"
  :ensure t
  :custom
  (completion-styles . '(orderless))
  (completion-category-defaults . nil))
