;;; myfuncs.el --- my misc functions  -*- lexical-binding: t; -*-

;; Version: 0.0.4
;; Package-Requires: ((emacs "26.2") (org "9.3"))

;;;###autoload
(defun find-value-by-key (key response)
  (let* ((list response))
    (if (equal key (car list))
        (cdr list))))
(defun extract-ts-from-response (response)
  (car (remove-if nil (mapcar #'(lambda (x) (find-value-by-key 'ts x)) response))))

(defun work-slack-initial-post (channel text)
  (request "https://slack.com/api/chat.postMessage"
    :type "POST"
    :data (json-encode `(("channel" . ,channel) ("text" . ,text)))
    :headers `(("Content-Type" . "application/json;charset=utf-8")
               ("Accept" . "application/json")
               ("Authorization" . ,(concat "Bearer " (getenv "SLACK_TOKEN_FIXPOINT_HAYASHI"))))
    :parser 'json-read
    :encoding 'utf-8
    :success  (cl-function (lambda (&key data &allow-other-keys)
                             (when data
                               (setq response data))))))
(defun work-slack-post (channel text &optional response)
  (request "https://slack.com/api/chat.postMessage"
    :type "POST"
    :data (json-encode `(("channel" . ,channel) ("text" . ,text) ("thread_ts" . ,(extract-ts-from-response response))))
    :headers `(("Content-Type" . "application/json;charset=utf-8")
               ("Accept" . "application/json")
               ("Authorization" . ,(concat "Bearer " (getenv "SLACK_TOKEN_FIXPOINT_HAYASHI"))))
    :parser 'json-read
    :encoding 'utf-8))

(defun set-slack-status (user token text emoji)
  (interactive)
  (request "https://slack.com/api/users.profile.set"
    :type "POST"
    :data (json-encode `(("user" . ,user)
                         ("profile" . (("status_text" . ,text)
                                       ("status_emoji" . ,emoji)))))
    :headers `(("Content-Type" . "application/json;charset=utf-8")
               ("Accept" . "application/json")
               ("Authorization" . ,(concat "Bearer " token)))
    :parser 'json-read))
;;;###autoload
(defun set-slack-status-hayashike (status)
  (let* ((user (getenv "SLACK_USER_HAYASHIKE"))
         (token (getenv "SLACK_TOKEN_HAYASHIKE")))
    (cond ((string-equal status "resting")
           (set-slack-status user token "coffee-emoji" ":coffee:"))
          ((string-equal status "smiling")
           (set-slack-status user token "smile-emoji" ":smile:")))))
;;;###autoload
(defun set-slack-status-work (status)
  (let* ((user (getenv "SLACK_USER_FIXPOINT"))
         (token (getenv "SLACK_TOKEN_FIXPOINT")))
    (cond ((string-equal status "home")
           (set-slack-status user token "\u7d42\u696d\u6e08" ":kan:"))
          ((string-equal status "resting")
           (set-slack-status user token "\u4f11\u61a9\u4e2d" ":kyukeichu:"))
          ((string-equal status "working")
           (set-slack-status user token "" "")))))
;;;###autoload
(defun work-start nil
  (interactive)
  (progn
    (browse-url "https://gather.town/app/HVvF6mCk1RBlwjAz/fixpoint")
    (browse-url "https://teamspirit-32942.lightning.force.com/lightning/page/home")
    (set-slack-status-work "working")
    (work-slack-initial-post "attendance" "リモート開始")
    (work-slack-post "times-hayashi" "リモート開始")))
;;;###autoload
(defun work-end nil
  (interactive)
  (progn
    (set-slack-status-work "home")
    (work-slack-post "attendance" "終了" response)
    (work-slack-post "times-hayashi" "終了")))
;;;###autoload
(defun work-break nil
  (interactive)
  (progn
    (set-slack-status-work "resting")
    (work-slack-post "attendance" "休憩" response)
    (work-slack-post "times-hayashi" "休憩" response)))
;;;###autoload
(defun work-restart nil
  (interactive)
  (progn
    (set-slack-status-work "working")
    (work-slack-post "attendance" "再開" response)
    (work-slack-post "times-hayashi" "再開" response)))
;;;###autoload
;;;###autoload
(defun end-routine ()
  (progn (org-reset-checkbox-state-subtree)(save-buffer)(magit-status-setup-buffer)))
;;;###autoload
(defun view-routine-writing ()
  (interactive)
  (org-tags-view t "writing-blog//TODO|NEXT"))
;;;###autoload
(defun view-sprint-tasks ()
  (interactive)
  (org-tags-view t "PROJ-@home-mail-tel-private//TODO"))
;;;###autoload
(defun view-mail-todos ()
  (interactive)
  (org-tags-view t "mail//TODO"))
;;;###autoload
(defun view-todays-tasks ()
  (interactive)
  (org-tags-view t "TODO=\"NEXT\""))
;;;###autoload
(defun view-try ()
  (interactive)
  (org-tags-view nil "+try"))
;;;###autoload
(defun view-misc-tasks ()
  (interactive)
  (org-tags-view t "-PROJ-private-@home-TRY-read//TODO"))
;;;###autoload
(defun view-definition-of-complete ()
  (interactive)
  (org-tags-view t "PROJ//DOC"))
;;;###autoload
(defun kmacro-save (symbol)
  (interactive "SName for last kbd macro: ")
  (name-last-kbd-macro symbol)
  (with-current-buffer (find-file-noselect kmacro-save-file)
    (goto-char (point-max))
    (insert-kbd-macro symbol)
    (basic-save-buffer)))
;;;###autoload
(defun view-google-calendar ()
  (interactive)
  (browse-url "https://calendar.google.com/calendar/r"))

;;;###autoload
(defun create-tags (dir-name)
     "Create tags file."
     (interactive "DDirectory: ")
     (eshell-command
      (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))

;;;###autoload
(defun git-word-diff-add-or-del (add-or-del branch filename)
   (let* ((cmd-git (concat "git diff --word-diff=porcelain HEAD origin/" branch " -- " filename))
           (cmd-grep   (concat " | grep -e '^" add-or-del "[^" add-or-del "]' | wc -w | xargs"))
           (return (string-to-number (shell-command-to-string (concat cmd-git cmd-grep)))))
       (if (integerp return)
            return
         "not integer")))
;;;###autoload
(defun git-word-diff (branch filename)
  (let* ((added (git-word-diff-add-or-del "+" branch filename))
         (deleted (git-word-diff-add-or-del "-" branch filename)))
    (- added deleted)))
;;;###autoload
(defun git-word-diff-absolute (branch filename)
  (let* ((added (git-word-diff-add-or-del "+" branch filename))
         (deleted (git-word-diff-add-or-del "-" branch filename)))
    (+ added deleted)))
;;;###autoload
(defun my-insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point, takes ARGS.

  Prefixed with \\[universal-argument], expand the file name to
  its fully canocalized path.  See `expand-file-name'.

  Prefixed with \\[negative-argument], use relative path to file
  name from current directory, `default-directory'.  See
  `file-relative-name'.

  The default with no prefix is to insert the file name exactly as
  it appears in the minibuffer prompt."
  ;; Based on insert-file in Emacs -- ashawley 20080926
  (interactive "*fInsert file name: \nP")
  (cond ((eq '- args)
         (insert (expand-file-name filename)))
        ((not (null args))
         (insert filename))
        (t
         (insert (file-relative-name filename)))))

;;;###autoload
(global-set-key (kbd "s-c")
                (defun org-toggle-checkbox-and-nextline ()
                  (interactive)
                  (progn (org-toggle-checkbox)(next-line))))

;;;###autoload
(defun my-break-between-braces ()
  (interactive)
  (if
      (and (eq ?} (char-after (point)))
           (eq ?{(char-before (point))))
      (progn
        (c-context-open-line)
        (c-context-line-break))
      (newline-and-indent)))

(provide 'myfuncs)

;;; myfuncs.el ends here
