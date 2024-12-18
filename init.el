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
    (leaf el-get
      :ensure t
      :custom
      (el-get-notify-type       . 'message)
      (el-get-git-shallow-clone . t))
    (leaf hydra :ensure t)
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

;; See http://rindrics.com/emacs/magit/
(leaf magit
  :doc "Complete text-based user interface to Git"
  :url "https://magit.vc/"
  :ensure t
  :init
  (setq magit-auto-revert-mode nil))
