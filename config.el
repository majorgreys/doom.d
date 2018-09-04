;;; config.el --- description -*- lexical-binding: t; -*-

(setq-default
      user-mail-address "tahir@tahirbutt.com"
      user-full-name    "Tahir H. Butt"
      doom-font (font-spec :family "Input Mono Narrow" :size 14 :weight 'ultralight)
      doom-variable-pitch-font (font-spec :family "Input Sans Narrow" :size 14 :weight 'normal)
      doom-unicode-font (font-spec :family "Sarasa Mono SC" :size 12 :weight 'normal)
      doom-big-font (font-spec :family "Input Mono Narrow" :size 22 :weight 'ultralight)
      ovp-font "Iosevka Term"
      doom-theme 'doom-city-lights)

(add-to-list 'default-frame-alist
             '(ns-transparent-titlebar . t))

(add-to-list 'default-frame-alist
             '(ns-appearance . dark))

;; load heavy packages all sneaky breeky like
(defun auto-require-packages (packages)
  (let ((gc-cons-threshold doom-gc-cons-upper-limit)
        file-name-handler-alist)
    (let* ((reqs (cl-remove-if #'featurep packages))
           (req (pop reqs)))
      (when req
        (require req)
        (when reqs
          (run-with-idle-timer 1 nil #'auto-require-packages reqs))))))

(run-with-idle-timer 1 nil #'auto-require-packages
                     '(calendar find-func format-spec org-macs org-compat
                       org-faces org-entities org-list org-pcomplete org-src
                       org-footnote org-macro ob org org-clock org-agenda
                       org-capture with-editor git-commit package magit))


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

(def-package! ob-ipython
  :after (org ob)
  :config
  (setq ob-ipython-resources-dir ".ob-ipython-resrc/")
  (setq ob-ipython-command "/opt/anaconda/bin/jupyter"))

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
           (file+headline +org-default-todo-file "Inbox")
           "* TODO %?\n %i\n %a" :prepend t :kill-buffer t)

          ("j" "Journal" entry
           (file+olp+datetree "journal.org" "Inbox")
           "* %?\nEntered on %U\n %i\n %a" :prepend t :kill-buffer t)

          ("n" "Notes" entry
           (file+headline +org-default-notes-file "Inbox")
           "* %u %?\n %i" :prepend t :kill-buffer t)

          ("ld" "Liwwa: Daily Scrum" entry
           (file+olp+datetree "liwwa.org" "Journal")
           "* Logged at %U :scrum:\nSince last scrum\n- %?\nNext:\n-" :prepend t :kill-buffer t)

          ("ln" "Liwwa: Note" entry
           (file+olp+datetree "liwwa.org" "Journal")
           "* %? :note:\n %i" :prepend t :kill-buffer t)

          ("lt" "Liwwa: Todo" entry
           (file+headline "liwwa.org" "Todo")
           "* TODO %?\n %i\n %a" :prepend t :kill-buffer t)
          ))

  (autoload 'magit--display-buffer-fullframe "magit-mode")
  (advice-remove #'org-capture-place-template #'+popup*suppress-delete-other-windows)
  (set-popup-rule! "^CAPTURE.*\\.org$" :actions '(magit--display-buffer-fullframe) :quit nil))

(def-package! ace-link
  :commands (ace-link ace-link-eww ace-link-elfeed))

(def-package! zotxt
  :config
  (setq zotxt-default-bibliography-style "chicago-note-bibliography"))

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

(def-package! conda
  :commands (conda-env-activate-for-buffer)
  :config
  (setq conda-anaconda-home "~/.conda/")
  (conda-env-autoactivate-mode -1)
  (add-hook 'python-mode-hook #'conda-env-activate-for-buffer)
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell)
    ;; Version management with pyenv
  (defun +python|add-version-to-modeline ()
    "Add version string to the major mode in the modeline."
    (setq mode-name
          (if conda-env-current-name
              (format "Py:conda:%s" conda-env-current-name)
            "Python")))
  (add-hook 'conda-postactivate-hook #'+python|add-version-to-modeline)
  (add-hook 'conda-postdeactivate-hook #'+python|add-version-to-modeline))

(add-to-list 'auto-mode-alist '("\\.jinja$" . web-mode))

(def-package! py-autopep8
  :config
  (add-hook 'python-mode-hook #'py-autopep8-enable-on-save))

(def-package! python
  :config
  (add-hook 'before-save-hook #'py-isort-before-save))

(def-package! auto-virtualenvwrapper
  :config
  (add-hook 'python-mode-hook #'auto-virtualenvwrapper-activate))

(after! ein
  (setq ein:jupyter-default-notebook-directory "~/"))

(after! tramp-sh
  (setq tramp-default-method "ssh"
        tramp-default-proxies-alist
        '(("liwwa-vm" nil "/ssh:liwwa-vm:"))
        tramp-debug-buffer t
        tramp-verbose 10))

(provide 'config)
;;; config.el ends here
