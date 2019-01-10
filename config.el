;;; config.el --- description -*- lexical-binding: t; -*-

(setq-default
      user-mail-address "tahir@tahirbutt.com"
      user-full-name    "Tahir H. Butt"
      doom-font (font-spec :family "Iosevka" :size 14)
      doom-serif-font (font-spec :family "Iosevka Slab" :size 14)
      doom-unicode-font (font-spec :family "Iosevka")
      doom-big-font (font-spec :family "Iosevka" :size 28)
      doom-variable-pitch-font (font-spec :family "Input Sans Condensed")
      doom-theme (if (display-graphic-p) 'doom-Iosvkem nil))

(add-to-list 'default-frame-alist
             '(ns-transparent-titlebar . t))

(add-to-list 'default-frame-alist
             '(ns-appearance . dark))

(after! org
  (setq org-directory (expand-file-name "~/Dropbox/org/")
        org-agenda-files (list org-directory)
        org-ellipsis " ▼ ")

  (setq org-capture-templates
        '(("t" "Todo" entry
           (file+headline "todo.org" "Inbox")
           "* TODO %?\n %i\n %a" :prepend t :kill-buffer t)

          ("j" "Journal" entry
           (file+olp+datetree "journal.org" "Inbox")
           "* %?\nEntered on %U\n %i\n %a" :prepend t :kill-buffer t)

          ("n" "Notes" entry
           (file+headline "notes.org" "Inbox")
           "* %u %?\n %i" :prepend t :kill-buffer t))))

(after! tramp-sh
  (setq tramp-default-method "ssh"
        tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no"
        ;; tramp-debug-buffer t
        ;; tramp-verbose 10
        )
  (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash")))

(provide 'config)
;;; config.el ends here
