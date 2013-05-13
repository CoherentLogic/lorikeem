
;//LORIKEEM
(add-to-list 'load-path "~/.emacs.d")
(autoload 'mumps-mode "lorikeem" "MUMPS mode" t)
(setq auto-mode-alist (append '(("\\.m$" . mumps-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.ewd$" . xml-mode)) auto-mode-alist))
;LORIKEEM//