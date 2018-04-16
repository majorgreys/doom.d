;;; config.el --- description -*- lexical-binding: t; -*-

(setq user-mail-address "tahir@tahirbutt.com"
      user-full-name    "Tahir H. Butt"

      doom-themes-enable-bold t
      doom-themes-enable-italic t
      doom-font (font-spec :family "IBM Plex Mono" :size 16)
      doom-variable-pitch-font (font-spec :family "IBM Plex Sans" :size 14)
      doom-unicode-font (font-spec :family "Source Code Pro" :size 16)
      doom-big-font (font-spec :family "IBM Plex Mono" :size 20)
      doom-line-numbers-style nil
      ivy-posframe-font (font-spec :family "Input Mono Narrow" :size 16)
      ivy-height 12
      +rss-elfeed-files '("elfeed.org")
      ;; shr-use-fonts nil ;; for elfeed variable fonts

      org-ellipsis " + "

      +write-text-scale 1.5
      doom-theme 'doom-nord
      )


(after! ox-pandoc
  (setq org-pandoc-options
        '((standalone . t)
          (mathjax . t)))
  (setq org-pandoc-options-for-latex-pdf
        '((pdf-engine . "xelatex"))))

(setq +org-dir (expand-file-name "~/Dropbox/org/"))

(after! org
  (setq org-capture-templates
        '(("t" "Todo" entry
           (file+headline +org-default-todo-file "Inbox")
           "* TODO %?\n %i\n %a" :prepend t :kill-buffer t)

          ("j" "Journal" entry
           (file+olp+datetree "journal.org" "Inbox")
           "* %?\nEntered on %U\n %i\n %a" :prepend t :kill-buffer t)

          ("n" "Notes" entry
           (file+headline +org-default-notes-file "Inbox")
           "* %u %?\n %i" :prepend t :kill-buffer t))))

(def-package! ace-link
  :commands (ace-link ace-link-eww ace-link-elfeed))

(def-package! org-zotxt
  :commands org-zotxt-mode
  :init (add-hook 'org-mode-hook #'org-zotxt-mode)
  :config
  (setq org-zotxt-default-search-method :everything)
  (setq org-zotxt-link-description-style :betterbibtexkey))

(after! shr
  (require 'shr-tag-pre-highlight)
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight)))

(after! magithub
  (setq magithub-clone-default-directory (expand-file-name "~/GitHub/")))

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

(provide 'config)
;;; config.el ends here
