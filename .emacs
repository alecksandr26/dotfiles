(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(magit flycheck company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Load the theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'monokai t)

;; Remove all the unneeded UI Elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Activate the number column
;;(global-display-line-numbers-mode 1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Load the font
(set-frame-parameter nil 'font "Source Code Pro SemiBold-10")

;; Identation
(setq-default indent-tabs-mode t)
(setq-default tab-width 8)

(defvaralias 'c-basic-offset 'tab-width) ; Identation for C

;; start package.el with emacs
(require 'package)

;; add MELPA to repository list
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; initialize package.el
(package-initialize)

;; Load comany
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Load flycheck
(add-hook 'after-init-hook 'global-flycheck-mode)

;; My shortcuts
(global-set-key [f5] 'compile)          ; To compile the code
(global-set-key (kbd "C-c C-f") 'display-buffer-other-frame) ; To open a new frame

(message ".emacs loaded correctly")
