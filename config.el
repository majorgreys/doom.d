;;; config.el --- description -*- lexical-binding: t; -*-

(setq-default
      user-mail-address "tahir@tahirbutt.com"
      user-full-name    "Tahir H. Butt"
      doom-font (font-spec :family "Input Mono Narrow" :size 16 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Input Sans Narrow" :size 16 :weight 'normal)
      doom-unicode-font (font-spec :family "Sarasa Mono SC" :size 12 :weight 'normal)
      doom-big-font (font-spec :family "Input Mono Narrow" :size 22 :weight 'semi-light)
      ovp-font "Iosevka Term"
      doom-theme 'doom-city-lights)

(add-to-list 'default-frame-alist
             '(ns-transparent-titlebar . t))

(add-to-list 'default-frame-alist
             '(ns-appearance . dark))

;; lang/org
(setq org-directory (expand-file-name "~/Dropbox/org/")
      org-agenda-files (list org-directory)
      org-ellipsis " ▼ "
      org-bullets-bullet-list '("#"))

(after! ox-pandoc
  (setq org-pandoc-options
        '((standalone . t)
          (mathjax . t)))
  (setq org-pandoc-options-for-latex-pdf
        '((pdf-engine . "xelatex"))))

(after! org
  (setq +org-babel-mode-alist
        '(("ipython" . ipython)
          ("bash" . shell)
          ("plantuml" . t)
          ("sh" . shell)))

  (setq org-plantuml-jar-path
      (expand-file-name "/opt/plantuml/plantuml.jar"))

  (setq org-capture-templates
        '(("t" "Todo" entry
           (file+headline "todo.org" "Inbox")
           "* TODO %?\n %i\n %a" :prepend t :kill-buffer t)

          ("j" "Journal" entry
           (file+olp+datetree "journal.org" "Inbox")
           "* %?\nEntered on %U\n %i\n %a" :prepend t :kill-buffer t)

          ("n" "Notes" entry
           (file+headline "notes.org" "Inbox")
           "* %u %?\n %i" :prepend t :kill-buffer t)

          ("dn" "Dissertation: Journal" entry
           (file+olp+datetree "dissertation.org" "Journal")
           "* %? :note:\n %i" :prepend t :kill-buffer t)

          ("dt" "Dissertation: Todo" entry
           (file+headline "dissertation.org" "Todo")
           "* TODO %?\n %i\n %a" :prepend t :kill-buffer t)

          ("ddn" "Datadog: Journal" entry
           (file+olp+datetree "datadog.org" "Journal")
           "* %? :note:\n %i" :prepend t :kill-buffer t)

          ("ddt" "Datadog: Todo" entry
           (file+headline "datadog.org" "Todo")
           "* TODO %?\n %i\n %a" :prepend t :kill-buffer t))))

(after! elfeed-show
  (map! (:map elfeed-show-mode-map
          [remap kill-this-buffer]      "q"
          [remap kill-buffer]           "q"
          :nm "q"   #'+rss/delete-pane
          :nm "o"   #'ace-link-elfeed
          :nm "RET" #'org-ref-add-bibtex-entry-from-elfeed-entry
          :nm "n"   #'elfeed-show-next
          :nm "p"   #'elfeed-show-prev
          :nm "+"   #'elfeed-show-tag
          :nm "-"   #'elfeed-show-untag
          :nm "s"   #'elfeed-show-new-live-search
          :nm "y"   #'elfeed-show-yank)))

(after! tramp-sh
  (setq tramp-default-method "ssh"
        tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no"
        ;; tramp-debug-buffer t
        ;; tramp-verbose 10
        )
  (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash")))

(provide 'config)
;;; config.el ends here
