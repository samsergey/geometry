(require 'package)
(add-to-list 'package-archives
  '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(company-ghc-show-info t)
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
    ("01e067188b0b53325fc0a1c6e06643d7e52bc16b6653de2926a480861ad5aa78" "1b27e3b3fce73b72725f3f7f040fd03081b576b1ce8bbdfcb0212920aec190ad" "158013ec40a6e2844dbda340dbabda6e179a53e0aea04a4d383d69c329fba6e6" "e30f381d0e460e5b643118bcd10995e1ba3161a3d45411ef8dfe34879c9ae333" "9b1c580339183a8661a84f5864a6c363260c80136bd20ac9f00d7e1d662e936a" "d21135150e22e58f8c656ec04530872831baebf5a1c3688030d119c114233c24" "73a13a70fd111a6cd47f3d4be2260b1e4b717dbf635a9caee6442c949fad41cd" "cf284fac2a56d242ace50b6d2c438fcc6b4090137f1631e32bedf19495124600" "228c0559991fb3af427a6fa4f3a3ad51f905e20f481c697c6ca978c5683ebf43" "66aea5b7326cf4117d63c6694822deeca10a03b98135aaaddb40af99430ea237" "de0b7245463d92cba3362ec9fe0142f54d2bf929f971a8cdf33c0bf995250bcf" "107420ec61832543bd769e5d6764bedb41ec3d2f8cd4d53a407f7aa74b493dac" "6db9acac88c82f69296751e6c6d808736d6ff251dcb34a1381be86efc14fef54" "64ca5a1381fa96cb86fd6c6b4d75b66dc9c4e0fc1288ee7d914ab8d2638e23a9" "946e871c780b159c4bb9f580537e5d2f7dba1411143194447604ecbaf01bd90c" "b181ea0cc32303da7f9227361bb051bbb6c3105bb4f386ca22a06db319b08882" "af717ca36fe8b44909c984669ee0de8dd8c43df656be67a50a1cf89ee41bde9a" "3fa07dd06f4aff80df2d820084db9ecbc007541ce7f15474f1d956c846a3238f" "962dacd99e5a99801ca7257f25be7be0cebc333ad07be97efd6ff59755e6148f" default)))
 '(default-input-method "TeX")
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote text-mode))
 '(initial-scratch-message nil)
 '(js2-highlight-level 3)
 '(js2-strict-missing-semi-warning nil)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (imenu-list js2-highlight-vars js2-mode mode-icons julia-repl ocodo-svg-modelines beacon popwin magit smooth-scroll airline-themes spaceline shm intero multiple-cursors markup-faces markdown-mode company-ghc)))
 '(save-place-mode t nil (saveplace))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode t)
 '(visual-line-mode t t))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(require 'semantic/sb)

;; NO JUNK
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `((".*" . ,temporary-file-directory)))

(fset 'yes-or-no-p 'y-or-n-p)

;Make the mouse jump away when you type over it.
(mouse-avoidance-mode 'cat-and-mouse)


;; Параметры скроллинга
;;(setq scroll-margin 3)
;;(setq scroll-step 1)
;;(setq scroll-preserve-screen-position nil)
;;(require 'smooth-scroll)
;;(smooth-scroll-mode t)


;;================================
;; File
;;================================
;; maintains a menu for visiting files that were operated on recently.
;; When enabled a new "Open Recent" submenu is displayed in the "Files" menu.
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 60)
(global-set-key "\C-x\C-r" 'recentf-open-files)

;;;---------------------------------------------------------------
;;; использование клавиш-команд при включённой русской раскладке
;;;---------------------------------------------------------------
(defun reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
        (modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (quail-get-translation
                      (cadr map) (char-to-string to) 1)))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key local-function-key-map
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))

(reverse-input-method 'russian-computer)
;;;---------------------------------------------------------------

(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     (local-set-key "\C-c\C-c" 'eval-buffer)))

(add-hook 'html-mode-hook
	  '(lambda ()
	     (local-set-key "\C-cc" (lambda()(interactive)
				      (insert "<code>")
				      (save-excursion
					(insert "</code>"))
				      ))
	     (local-set-key "\C-cl" (lambda()(interactive)
				      (insert "<l></l>")))
	     (local-set-key "\C-cd" (lambda()(interactive)
				      (insert "<def>")
				      (save-excursion
					(insert "</def>"))
				      ))
	     (local-set-key "\C-cC" (lambda()(interactive)
				      (insert "<pre><hs>")
				      (save-excursion
					(insert "</hs></pre>"))
				      ))
	     (local-set-key "\C-ct" (lambda()(interactive)
				      (insert "<task>")
				      (save-excursion
					(insert "</task>"))
				      ))
	     (local-set-key "\C-ce" (lambda()(interactive)
				      (insert "<eq>")
				      (save-excursion
					(insert "</eq>"))
				      ))))

(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(global-set-key "\M-;" 'toggle-comment-on-line)
(global-set-key "\M-:" 'comment-or-uncomment-region)
(global-set-key "\C-d" 'delete-backward-char)
(global-set-key "\C-cv" 'visual-line-mode)


;; If you don't have MELPA in your package archives:
;(require 'package)
;(add-to-list
;  'package-archives
;  '("melpa" . "http://melpa.org/packages/") t)
;(package-initialize)
;(package-refresh-contents)

;; Install Intero
;(package-install 'intero)
;(add-hook 'haskell-mode-hook 'intero-mode)


;(load "~/.emacs.d/tex-mode-settings.el")
;(load "~/.emacs.d/haskell-mode-settings.el")
(load "~/.emacs.d/multiple-cursors-settings.el")


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "CMU Typewriter Text" :foundry "unknown" :slant normal :weight normal :height 158 :width normal))))
 '(comint-highlight-prompt ((t (:foreground "orange"))))
 '(compilation-error ((t (:foreground "red" :weight semi-bold))))
 '(cursor ((t (:background "dark orange"))))
 '(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground "gray"))))
 '(font-lock-string-face ((t (:foreground "gold"))))
 '(fricas-algebra ((t (:foreground "chartreuse"))))
 '(fricas-message ((t (:foreground "palevioletred"))))
 '(fricas-type-time ((t (:foreground "blue"))))
 '(fricas-undefined ((t (:foreground "orange red"))))
 '(haskell-constructor-face ((t (:inherit font-lock-type-face :weight normal))))
 '(haskell-interactive-face-result ((t (:foreground "yellow"))))
 '(haskell-operator-face ((t (:foreground "DarkKhaki" :weight normal))))
 '(js2-function-call ((t (:inherit default))))
 '(js2-object-property ((t (:inherit default :foreground "tan"))))
 '(outline-1 ((t (:foreground "red" :underline t :height 1.5))))
 '(outline-2 ((t (:inherit font-lock-variable-name-face :height 1.2)))))

(put 'downcase-region 'disabled nil)

(require 'airline-themes)
(load-theme 'airline-papercolor)
(setq powerline-utf-8-separator-left        #xe0b0
      powerline-utf-8-separator-right       #xe0b2
      airline-utf-glyph-separator-left      #xe0b0
      airline-utf-glyph-separator-right     #xe0b2
      airline-utf-glyph-subseparator-left   #xe0b1
      airline-utf-glyph-subseparator-right  #xe0b3
      airline-utf-glyph-branch              #xe0a0
      airline-utf-glyph-readonly            #xe0a2
      airline-utf-glyph-linenumber          #xe0a1)

(defun turn-off-mouse (&optional frame)
  (interactive)
  (let ((inhibit-message t) (default-directory "~"))
    (shell-command "synclient TouchpadOff=1")))

(defun turn-on-mouse (&optional frame)
  (interactive)
  (let ((inhibit-message t) (default-directory "~"))
    (shell-command "synclient TouchpadOff=0")))

;(global-set-key "\C-M-m" 'tun-on-mouse)
;(global-set-key "\C-down-mouse-2" 'tun-off-mouse)

;(turn-on-mouse)
;(add-hook 'focus-in-hook #'turn-off-mouse)
;(add-hook 'focus-out-hook #'turn-on-mouse)
;(add-hook 'delete-frame-functions #'turn-on-mouse)
;(add-hook 'kill-emacs-hook #'turn-on-mouse)
;(add-hook 'suspend-hook #'turn-on-mouse)

(beacon-mode 1)

(auto-image-file-mode 1)
(ocodo-svg-modelines-init)
(smt/set-theme 'ocodo-minimal-dark-smt)

(put 'upcase-region 'disabled nil)

(add-hook 'haskell-mode-hook
	  (lambda ()
	    (local-set-key "\C-c\C-c" 'load-and-switch)))

(add-hook 'inf-haskell-mode-hook
	  (lambda ()
	    (local-set-key "\C-cc" '(lambda () (interactive) (other-window 1)))))


(global-set-key (kbd "<f5>") 'load-and-switch)
(global-set-key (kbd "<f4>") '(lambda () (interactive) (other-window 1)))
;(add-hook 'haskell-mode-hook 'ghc-core-mode)
;(add-hook 'haskell-mode-hook 'inferior-haskell-mode)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;(load "~/.emacs.d/flp-practice.el")

(defun load-and-switch ()
  (interactive)
  (inferior-haskell-load-file)
  (other-window 1))

(load "~/.emacs.d/hasklig-mode-master/hasklig-mode.el")
(set-face-attribute 'default nil
                    :family "Hasklig"
                    :height 125
                    :weight 'normal
                    :width 'normal)
;;(add-hook 'haskell-mode-hook `hasklig-mode)
;;(add-hook 'inferior-haskell-mode-hook `hasklig-mode)

(mode-icons-mode)

(add-to-list 'auto-mode-alist '("\\.input\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.spad\\'" . haskell-mode))
;;(add-hook 'fricas-mode-hook `hasklig-mode)
(font-lock-add-keywords 'haskell-mode
			'(("for\\b" . font-lock-keyword-face)
			  ("with " . font-lock-keyword-face)
			  ("add " . font-lock-keyword-face)))

(load "~/.emacs.d/livescript-modeb.el")
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

;; Globally prettify symbols
;(global-prettify-symbols-mode 1)
;(setq prettify-symbols-alist '(("lambda" . 955)
;                               ("`bind`" . 226B)))
