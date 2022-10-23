(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(buffers-menu-show-status nil)
 '(cursor-type 'bar)
 '(menu-bar-mode nil)
 '(menu-prompting nil)
 '(package-selected-packages
   '(fixmee neotree sql-indent auctex company-irony elpy company-c-headers flycheck magit vterm cython-mode multi-vterm projectile iedit yasnippet-snippets yasnippet auto-complete-c-headers auto-complete company))
 '(scroll-bar-mode nil)
 '(tab-width 4)
 '(tool-bar-mode nil))

;; To hide the started screen of emacs
(setq inhibit-startup-screen t)

(defun fontify-frame (frame)
  (set-frame-parameter frame 'font "Source Code Pro SemiBold-10"))
;; Fontify current frame
(fontify-frame nil)
;; Fontify any future frames
(push 'fontify-frame after-make-frame-functions)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; To make more slower the mouse scroll
(setq mouse-wheel-progressive-speed nil)

;; To manipulate the idetnation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4) ; Assuming you want your tabs to be four spaces wide

(defvaralias 'c-basic-offset 'tab-width)
(setq c-guess-make-basic-offset t)
(setq c-guess-guessed-basic-offset-verbose nil)
(setq js-indent-level 4)
(setq sgml-basic-offset 4)
(setq python-indent-guess-indent-offset t)
(setq python-indent-guess-indent-offset-verbose nil)

;; Desactivate the backup files
(setq make-backup-files nil)
(setq auto-save-default nil) ; stop creating #autosave# files
(put 'erase-buffer 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Load the monokai theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'monokai t)

;; To autocomplete brackets
(electric-pair-mode)

;; start package.el with emacs
(require 'package)
;; add MELPA to repository list
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; initialize package.el
(package-initialize)


;; Adding vterm
(add-to-list 'load-path "~/.emacs.d/emacs-libvterm")
(require 'vterm)

;; To run the iedit
(define-key global-map (kbd "C-c ;") 'iedit-mode)

;; To run flycheck
(add-hook 'after-init-hook 'global-flycheck-mode)

;; To start auto completeheaders
;; (require 'auto-complete)

;; Add the directories path to the auto complete c headers
;; (require 'auto-complete-c-headers)
;;  (add-to-list 'ac-sources 'ac-source-c-headers)
;;  (setq achead:include-directories
;;   (append '("/usr/include/c++/4.8"
;;             "/usr/include/x86_64-linux-gnu/c++/4.8"
;;             "/usr/include/c++/4.8/backward"
;;             "/usr/lib/gcc/x86_64-linux-gnu/4.8/include"
;;             "/usr/lib/gcc/x86_64-linux-gnu/4.8/include-fixed"
;;             "/usr/include/x86_64-linux-gnu")
;;             achead:include-directories))


;; Create function autocomplete c headers
;; (defun my:ac-c-headers-init ()
;;   (require 'auto-complete-c-headers)
;;   (add-to-list 'ac-sources 'ac-source-c-headers))

;; (add-hook 'c++-mode-hook 'my:ac-c-headers-init)
;; (add-hook 'c-mode-hook 'my:ac-c-headers-ini)

(require 'company)
(require 'company-c-headers)

;; To active company
(add-hook 'after-init-hook 'global-company-mode)

;; To active compnay c headers
(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-c-headers-path-system "/usr/include/")
(add-to-list 'company-c-headers-path-system "/usr/include/c++/12.2.0/")
(add-to-list 'company-c-headers-path-system "/usr/i686-w64-mingw32/include/")


;; Snippets
(require 'yasnippet)
(yas-global-mode 1)
(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(global-set-key (kbd "M-z") 'tab-indent-or-complete)

;; Enable elpy
(elpy-enable)

;; Enable Flycheck with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; To make a shortcut
(global-set-key [f5] 'compile)

;; To eneable the neotree
(add-to-list 'load-path "/some/path/neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; To start fixmee
(require 'fixmee)
(require 'button-lock)
(global-fixmee-mode 1)
