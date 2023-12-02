(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company-c-headers multiple-cursors cmake-mode rjsx-mode rust-mode crystal-mode iedit magit company)))
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


;; Remove backup files
(setq make-backup-files nil)

;; Activate the number column
;;(global-display-line-numbers-mode 1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Load the font
(set-frame-parameter nil 'font "Source Code Pro SemiBold-10")

;; Identation
(setq-default indent-tabs-mode t)
(setq-default tab-width 8)
(setq backward-delete-char-untabify-method 'hungry)

(defvaralias 'c-basic-offset 'tab-width) ; Identation for C

;; start package.el with emacs
(require 'package)

;; add MELPA to repository list
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; initialize package.el
(package-initialize)

;; Load comany
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Load flycheck
;; (add-hook 'after-init-hook 'global-flycheck-mode)
;; (flycheck-mode 0)

;; Tor un iedit
(global-set-key (kbd "C-c ;") 'iedit-mode)

;; My shortcuts
(defvar previous-shell-command nil
  "Variable to store the previous shell command.")

(defun async-shell-command-with-previous ()
  "Run `async-shell-command` with the previously used command, if any."
  (interactive)
  (let ((command (read-shell-command "Shell command: " previous-shell-command)))
    (setq previous-shell-command command)
    (async-shell-command command)))

(global-set-key [f5] 'compile)
(global-set-key [f6] 'async-shell-command-with-previous)
(global-set-key (kbd "C-c C-f") 'display-buffer-other-frame) ; To open a new frame

;; For crystal
;; (require 'flycheck-crystal)
;; (add-hook 'crystal-mode-hook 'flycheck-mode)

;; For running rust
(require 'rust-mode)
;; (require 'flycheck-rust)
;; (add-hook 'rust-mode-hook 'flycheck-mode)
;; (with-eval-after-load 'rust-mode
;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))


;; ;; To have iedit in fortran
;; (add-hook 'f90-mode-hook
;;           (lambda ()
;;             ;; Use default global binding for M-f and M-b.
;;             (local-set-key (kbd "C-c ;") 'iedit-mode)))

;; (add-hook 'fortran-mode-hook
;;           (lambda ()
;;             ;; Use default global binding for M-f and M-b.
;;             (local-set-key (kbd "C-c ;") 'iedit-mode)))


;; ;; Run the aucte
;; (add-hook 'latex-mode-hook
;;           (lambda ()
;;             (display-line-numbers-mode)
;;             (flycheck-mode -1)))

;; ;; TO have multiple cursors
;; (require 'multiple-cursors)

;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


(message ".emacs loaded correctly")
(put 'downcase-region 'disabled nil)
