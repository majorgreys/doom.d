;;; config.el --- description -*- lexical-binding: t; -*-

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

(after! org
  (setq org-directory (expand-file-name "~/Dropbox/org/")
        org-agenda-files (list org-directory)
        org-ellipsis " ▼ ")

  (map! :map org-mode-map
        :localleader
        (:prefix ("p" . "Push/Pull")
          :desc "pull"  "l" (lambda! (org-trello-sync-buffer t))
          :desc "push"  "p" #'org-trello-sync-buffer))

  (setq org-capture-templates
        '(("t" "Todo" entry
           (file+headline "todo.org" "Inbox")
           "* TODO %?\n %i\n %a" :prepend t :kill-buffer t)

          ("j" "Journal" entry
           (file+olp+datetree "journal.org" "Journal")
           "* %?\nEntered on %U\n %i\n %a" :prepend t :kill-buffer t)

          ("n" "Notes" entry
           (file+headline "notes.org" "Inbox")
           "* %u %?\n %i" :prepend t :kill-buffer t))))

(def-package! lsp-python-ms
  :after lsp
  :config
  (setq lsp-python-ms-dir
        (expand-file-name "~/src/python-language-server/output/bin/Release/"))
  (setq lsp-python-ms-executable
        "~/src/python-language-server/output/bin/Release/osx-x64/publish/Microsoft.Python.LanguageServer"))

(after! tramp-sh
  (setq tramp-default-method "ssh"
        tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no"
        ;; tramp-debug-buffer t
        ;; tramp-verbose 10
        )
  (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash")))

(provide 'config)
;;; config.el ends here
