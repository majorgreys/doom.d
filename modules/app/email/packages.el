;; -*- no-byte-compile: t; -*-
;;; app/email/package.el -*- lexical-binding: t; -*-

(package! notmuch)
(package! org-mime)
(package! vdirel)
(when (featurep! :completion ivy)
  (package! counsel-notmuch))
(when (featurep! :completion helm)
  (package! helm-notmuch))
