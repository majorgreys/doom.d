;;; config.el --- description -*- lexical-binding: t; -*-

(fset 'battery-update #'ignore)

(setq-default
      user-mail-address "tahir@tahirbutt.com"
      user-full-name    "Tahir H. Butt"
      doom-font (font-spec :family "Iosevka Term SS08" :size 14)
      doom-serif-font (font-spec :family "Iosevka Term Slab" :size 14)
      doom-unicode-font (font-spec :family "Iosevka Term Slab")
      doom-big-font (font-spec :family "Iosevka Term SS08" :size 28)
      doom-variable-pitch-font (font-spec :family "Input Sans Condensed")
      doom-theme (if (display-graphic-p) 'doom-city-lights nil)
      +doom-dashboard-banner-padding '(0 . 0)
      +doom-dashboard-banner-file "vim.png"
      +doom-dashboard-banner-dir "~/.doom.d/assets/"
      +doom-dashboard-functions
      '(doom-dashboard-widget-banner
        doom-dashboard-widget-shortmenu
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

(after! org
  (setq org-directory (expand-file-name "~/Dropbox/org/")
        org-agenda-files (list org-directory)
        org-ellipsis " ▼ "))

(after! tramp-sh
  (setq tramp-default-method "ssh"
        tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no"
        ;; tramp-debug-buffer t
        ;; tramp-verbose 10
        )
  (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash")))

(provide 'config)
