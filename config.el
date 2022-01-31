;;; config.el --- description -*- lexical-binding: t; -*-

(fset 'battery-update #'ignore)

(setq-default
      user-mail-address "tahir@tahirbutt.com"
      user-full-name    "Tahir H. Butt"
      doom-theme 'modus-vivendi
      doom-font (font-spec :family "Iosevka SS08" :size 14)
      doom-serif-font (font-spec :family "Iosevka Slab" :size 14)
      doom-unicode-font (font-spec :family "Iosevka Slab")
      doom-big-font (font-spec :family "Iosevka SS08" :size 20)
      doom-variable-pitch-font (font-spec :family "Input Sans Condensed")
      +doom-dashboard-banner-padding '(0 . 0)
      +doom-dashboard-banner-file "gnu-head.png"
      +doom-dashboard-banner-dir "~/.doom.d/assets/"
      +doom-dashboard-functions
      '(doom-dashboard-widget-banner
        doom-dashboard-widget-shortmenu
        doom-dashboard-widget-loaded
        doom-dashboard-widget-footer))

(when IS-MAC
   (setq ns-use-thin-smoothing t)    ; thinner strokes for font-smoothing
   (setq dired-use-ls-dired nil)
   ;; macOS natural title bars
   (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
   (add-to-list 'default-frame-alist '(ns-appearance . dark))
   (add-hook 'window-setup-hook #'toggle-frame-fullscreen))

(after! notmuch
  (setq +notmuch-sync-backend `mbsync)
  (setq +notmuch-mail-folder "~/.mail/tahirbutt")
  (setq sendmail-program "~/.local/bin/msmtp-enqueue.sh"))

(map! :localleader
  (:map org-tree-slide-mode-map
    "e" #'org-tree-slide-slide-in-effect-toggle
    "p" #'org-tree-slide-move-previous-tree
    "n" #'org-tree-slide-move-next-tree
    "c" #'org-tree-slide-content
    "q" #'org-tree-slide-mode))

(setq python-shell-interpreter "python3")

;; org-mode
(setq org-directory (expand-file-name "~/org/"))
(setq +org-capture-cookbook (expand-file-name "~/org/cookbook.org"))

(after! org
  (setq org-agenda-files (list org-directory)
        org-ellipsis " ▼ ")
  (setq org-capture-templates
          '(("t" "Personal todo" entry
            (file+headline +org-capture-todo-file "Inbox")
            "* TODO %?\n%i\n%a" :prepend t)
            ("n" "Personal notes" entry
            (file+headline +org-capture-notes-file "Inbox")
            "* %u %?\n%i\n%a" :prepend t)
            ("j" "Journal" entry
             (file+olp+datetree +org-capture-journal-file)
             "* %U %?\n%i\n%a" :prepend t)
            ("c" "Cookbook" entry (file (expand-file-name "~/org/cookbook.org"))
             "%(org-chef-get-recipe-from-url)" :empty-lines 1)
            ("m" "Manual Cookbook" entry (file (expand-file-name "~/org/cookbook.org"))
             "* %^{Recipe title: }\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n   %?\n** Directions\n\n")
            ("d" "Centralized templates for Datadog")
            ("dt" "Datadog todo" entry
            (file+headline "datadog.org" "Tasks")
            "* TODO %?\n %i\n %a"
            :prepend nil)
            ("dn" "Datadog note" entry
            (file+headline "datadog.org" "Notes")
            "* %U %?\n %i\n %a"
            :prepend t))))

(use-package! org-noter
  :defer t
  :after org-mode
  :config
  (map! :map (pdf-view-mode-map nov-mode-map)
     :n "i" #'org-noter-insert-note
     :n "K" #'org-noter-kill-session)
  :config
  (setq org-noter-auto-save-last-location t
        org-noter-always-create-frame nil
        org-noter-insert-note-no-questions t))

(after! tramp-sh
  (setq tramp-default-method "ssh"
        tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no"
        ;; tramp-debug-buffer t
        ;; tramp-verbose 10
        )
  (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash")))

(after! circe
  (set-irc-server! "irc.libera.net"
    `(:tls t
      :port 6697
      :nick "majorgreys"
      :sasl-username ,(+pass-get-user "irc/libera.net")
      :sasl-password (lambda (&rest _) (+pass-get-secret "irc/libera.net"))
      :channels ("#emacs"))))

(provide 'config)
