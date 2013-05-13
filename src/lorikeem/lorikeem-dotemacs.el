
;;//LORIKEEM START
(add-to-list 'load-path "~/.emacs.d/plugins/lorikeem")
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas/global-mode 1)
(autoload 'mumps-mode "lorikeem" "MUMPS mode" t)
(setq auto-mode-alist (append '(("\\.m$" . mumps-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.ewd$" . xml-mode)) auto-mode-alist))
;;LORIKEEM END//