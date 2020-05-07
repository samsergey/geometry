;; add lines to .emacs
;; (load "~/work/webinar/webfigs/livescript-mode.el")
;; (load "~/work/webinar/webfigs/emacs-settings.el")


(add-to-list 'auto-mode-alist '("\\.ls\\'" . livescript-mode))
(add-hook 'livescript-mode-hook `hasklig-mode)
(add-hook 'livescript-mode-hook 'livescript-unicode)
(add-hook 'livescript-mode-hook
	  (lambda ()
	    (local-set-key "\C-c\C-v" 'livescript-compile-file)))
(add-hook 'livescript-mode-hook
	  (lambda ()
	    (local-set-key "\C-c\C-c" 'livescript-compile-buffer)))
(add-hook 'livescript-mode-hook 'livescript-cos-mode)
