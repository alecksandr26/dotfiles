;; -------------------------------------------------------------------------------------------
;; Init default configs

;; Do not show the startup screen.
(setq inhibit-startup-message t)

;; Disable tool bar, menu bar, scroll bar.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)


;; Config your backupfiles:
;; https://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
;; https://emacs.stackexchange.com/questions/33/put-all-backups-into-one-backup-folder
(setq backup-directory-alist '(("." . "~/.emacs.d/emacs-backup")))

;; Create the directory '.emacs-saves' for emacs saves
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/emacs-saves/" t)))


;; My code style is linux kernel coding style
;; https://www.kernel.org/doc/html/v4.10/process/coding-style.html
;; https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines

(setq-default tab-width 8)

;; To make tbe backspace delete the tabs not convert the tab into spaces etc..
(setq backward-delete-char-untabify-method 'nil)

;; Enable electric-pair-mode for automatic insertion of matching parentheses
(electric-pair-mode 1)


;; Enable the debug
;; To solve this problem of: wrong type argument: stringp, nil
;; https://stackoverflow.com/questions/5413959/wrong-type-argument-stringp-nil
;; (setq debug-on-error t)


;; Emacs as Deamon
;; https://www.emacswiki.org/emacs/EmacsAsDaemon
;; systemctl --user enable --now emacs
;; systemctl --user start --now emacs
;; check: https://unix.stackexchange.com/questions/56871/emacs-daemon-crashing-after-closing-emacsclient-c
(setq default-frame-alist '((font . "Source Code Pro SemiBold-10")))
(add-to-list 'default-frame-alist
             '(vertical-scroll-bars . nil))

;; Checking where Im running emacs
(if (daemonp)
    (message "Loading in the daemon!")
  (message "Loading in regular Emacs!")
  )

;; -------------------------------------------------------------------------------------------
;; Theme, Font, Trans, etc ... configs

;; Display the number lines
(global-display-line-numbers-mode 1)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Theme torte: https://stackoverflow.com/questions/14811454/vim-torte-colorscheme-for-emacs
;; (load-theme 'torte t)

;; monokai theme
;; https://github.com/oneKelvinSmith/monokai-emacs
;; (load-theme 'monokai t)


;; https://github.com/sjrmanning/darkokai/tree/5820aeddfc8c869ba840cc534eba776936656a66
;; (load-theme 'darkokai t)

;; https://github.com/dawidof/emacs-monokai-theme/tree/f342b6afc31f929be0626eca2d696ee9fab78011
;; (load-theme 'monokai-alt t)

;; https://github.com/belak/emacs-monokai-pro-theme/tree/d56fa38a9ed3b1d8e4f8401cb4c3f08073f3ba26
(load-theme 'monokai-pro t)
(load-theme 'monokai-pro-classic t)

;; Set the font
(set-frame-parameter nil 'font "Source Code Pro Semibold-10")

;; To make emacs transparent
(set-frame-parameter nil 'alpha-background 90)
(add-to-list 'default-frame-alist '(alpha-background . 90))

;; Ensure new frames created by the emacsclient also inherit the transparency setting
(defun set-frame-transparency (frame)
  (set-frame-parameter frame 'alpha-background 90))

(add-hook 'after-make-frame-functions 'set-frame-transparency)


;; -------------------------------------------------------------------------------------------
;; Key bindings config

;; To compile code or to run commands
(global-set-key [f5] 'compile)

;; Shell command to be saved
(defvar previous-shell-command nil
  "Variable to store the previous shell command.")

(defun async-shell-command-with-previous ()
  "Run `async-shell-command` with the previously used command, if any."
  (interactive)
  (let ((command (read-shell-command "Shell command: " previous-shell-command)))
    (setq previous-shell-command command)
    (async-shell-command command)))

;; To run commands with input and output streams
(global-set-key [f6] 'async-shell-command-with-previous)

;; To open a new frame of emacs
(global-set-key (kbd "C-c C-f") 'display-buffer-other-frame)


;; dired
;; To add a new file to the current listed direcotry 
(defun my-dired-add-file-here ()
  "Add a new file in the current directory in Dired mode."
  (interactive)
  (let ((dir (dired-current-directory)))
    (find-file (expand-file-name (read-string "New file name: " nil nil "new_file") dir))))

(global-set-key (kbd "C-+") 'my-dired-add-file-here)



;; -------------------------------------------------------------------------------------------
;; Native Packages config

;; ido - To autocomplete the buffers names
;; https://www.gnu.org/software/emacs/manual/html_mono/ido.html#Overview
(require 'ido)
(ido-mode t)


;; org mode
;; My org mode configuration
(require 'org)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.4))


;; -------------------------------------------------------------------------------------------
;; Initialized the package manager

(require 'package)

(package-initialize)

(add-to-list 'package-archives
        '("melpa-stable" . "http://stable.melpa.org/packages/"))
    
(add-to-list 'package-archives
        '("melpa" . "http://melpa.org/packages/"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(jenkinsfile-mode feature-mode react-snippets rjsx-mode company-auctex elpy cpputils-cmake cmake-mode company-c-headers fixmee multi-vterm vterm yasnippet-snippets flycheck magit-todos magit seq company smex use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; M-x package-install -> use-package
(require 'use-package)

(setq package-install-upgrade-built-in t)


;; -------------------------------------------------------------------------------------------
;; General package configs

;; smex
;; https://github.com/nonsequitur/smex/
(use-package smex
  :ensure t
  :init
  ;; Can be omitted. This might cause a (minimal) delay when Smex is auto-initialized on its first run.
  (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
	 ;; This is your old M-x.
         ("C-c C-c M-x" . execute-extended-command)))


;; company
;; https://company-mode.github.io/
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  
  ;; Remove company-capf becuase it doesn't works
  ;; (setq company-backends (delete 'company-semantic (delete 'company-capf company-backends)))
  )



;; magit
;; https://github.com/magit/magit/tree/2ed93504778c9ec2b8f56665cbdeae348146fbf7
;; Getting Started: https://magit.vc/manual/magit/Getting-Started.html
(use-package magit
  :ensure t)

(defun my-smerge-mode-hook ()
  (local-set-key (kbd "C-c k") 'smerge-keep-current)) ; Change "C-c k" to your desired keybinding

(add-hook 'smerge-mode-hook 'my-smerge-mode-hook)


;; magit-todos
;; IMPORTANT: install ripgrep
;; https://github.com/alphapapa/magit-todos/tree/debb77b3589f2d83c8b43706edc1f8f90bf1ad91
(use-package magit-todos
  :ensure t
  :after magit
  :config (magit-todos-mode 1))

;; flycheck
;; https://github.com/flycheck/flycheck/tree/7ee95638c64821e9092a40af12b1095aa5604fa5
(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode)
  :config
  ;; Add custom include path for C++
  (add-hook 'c++-mode-hook
            (lambda ()
              ;; (setq flycheck-clang-include-path
              ;;       (list (expand-file-name "/usr/lib/qt/")
	      ;; 		   (expand-file-name "/usr/include/qt/QtWidgets")
	      ;; 		   (expand-file-name "/usr/include/qt/QtGui")
	      ;; 		   (expand-file-name "/usr/include/qt/QtCore")
	      ;; 		   ))
	      
	      ;; Enable syntax-only checks, so you need install 'sudo pacman -S cppcheck'
              ;; (setq flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))
              ;; (flycheck-select-checker 'c/c++-cppcheck)
	      
	      ;; Set include paths for cppcheck
              ;; (setq flycheck-cppcheck-include-path '("/usr/include" "/usr/lib/qt/include"))
	      
              ;; Add suppressions
              ;;(setq flycheck-cppcheck-suppressions '())
	      
	      )))


;; projectile
;; https://docs.projectile.mx/projectile/index.html
;; usage: https://docs.projectile.mx/projectile/usage.html
(use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (setq projectile-project-search-path '("~/Documents/projects/"))
  :config
  (projectile-mode +1))

;; multiple-cursors
;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :ensure t
  ;; When you want to add multiple cursors not based on continuous lines,
  ;; but based on keywords in the buffer, use:
  ;; `C-g` To remove all the cursos
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))


;; yasnippet
;; Code snippets to a lot of languages 
;; https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

;; yasnippet-snippets
;; https://github.com/AndreaCrotti/yasnippet-snippets
(use-package yasnippet-snippets
  :ensure t)


;; ggtags, alternative to rtags
;; https://elpa.gnu.org/packages/ggtags.html
;; Download global here: https://www.gnu.org/software/global/
;; Or if you are in debian
;; apt install global exuberant-ctags
(use-package ggtags
  :ensure t
  :hook ((c-mode-common . (lambda ()
                            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                              (ggtags-mode 1))))))


;; iedit configuration
;; https://github.com/victorhge/iedit/tree/dd5d75b38ee0c52ad81245a8e5c932d3f5c4772d
(use-package iedit
  :ensure t
  :bind ("C-c ;" . iedit-mode)
  )

;; vterm
;; https://github.com/akermu/emacs-libvterm
;; usage; 'C-c t' To change the modes between the copy and paste
(use-package vterm
  :ensure t)

;; multi-vterm
;; https://github.com/suonlight/multi-vterm
(use-package multi-vterm
  :ensure t)

;; fixme
;; C-c V - To see all the current fixme tags
;; https://github.com/rolandwalker/fixmee
(use-package fixmee
  :ensure t
  :hook (after-init . global-fixmee-mode))
(use-package button-lock
  :ensure t)


;; hl-todo
;; https://github.com/tarsius/hl-todo/tree/7146bbcab5248f3fb9d09acb981b8e63f0c73413
(use-package hl-todo
  :ensure t
  :hook (after-init . global-hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#e4db73")
          ("FIXME"  . "#f82570")
          ("DEBUG"  . "#ae81ff")
          ("GOTCHA" . "#fc961f")
          ("STUB"   . "#66d9ee"))))


;; -------------------------------------------------------------------------------------------
;; My C/C++ Packges and Configs

;; company-c-headers
;; https://github.com/randomphrase/company-c-headers
(use-package company-c-headers
  :ensure t
  :after (company)
  :config
  (add-to-list 'company-backends 'company-c-headers)
  (add-to-list 'company-c-headers-path-system "/usr/include/")
  (add-to-list 'company-c-headers-path-system "/usr/include/c++/12/")
  ;; (add-to-list 'company-c-headers-path-system "/usr/include/c++/14.2.1/")
  ;; (add-to-list 'company-c-headers-path-system "/usr/include/qt/")
  ;; (add-to-list 'company-c-headers-path-system "/usr/include/qt/*/")
  )


;; rtags
;; Install manually: https://github.com/Andersbakken/rtags#tldr-quickstart
;; Usage: https://github.com/Andersbakken/rtags/wiki/Usage
;; (require 'rtags)
;; (add-hook 'after-init-hook 'rtags-start-process-unless-running)
;; Adding the keybingds
;; (rtags-enable-standard-keybindings)


;; cmake-ide
;; https://github.com/atilaneves/cmake-ide
;; Starts the server 'rdm' and the 'rc -J' if a 'compile_commands.json' exist
;; (use-package cmake-ide
;;   :ensure t
;;   :init (cmake-ide-setup))


;; cmake-mode
;; https://melpa.org/#/cmake-mode
;; A simple highlighter for cmake files
(use-package cmake-mode
  :ensure t)


;; cpputils-cmake
;; To give support to flycheck in code projects
;; and to automatically run gdb
;; https://github.com/redguardtoo/cpputils-cmake
(use-package cpputils-cmake
  :ensure t
  :hook
  (c-mode-common . (lambda ()
                     (when (derived-mode-p 'c-mode 'c++-mode)
                       (cppcm-reload-all))))
  :config
  (global-set-key (kbd "C-c C-g")
                  '(lambda ()
                     (interactive)
                     (gud-gdb (concat "gdb --fullname " (cppcm-get-exe-path-current-buffer)))))  
  )


(defun my-c-c++-mode-hook ()
  (setq c-basic-offset 8) ; Indentation level is 4 spaces
  (setq indent-tabs-mode t) ; Use tabs instead of spaces
  )

(add-hook 'c-mode-hook 'my-c-c++-mode-hook)
(add-hook 'c++-mode-hook 'my-c-c++-mode-hook)

;; -------------------------------------------------------------------------------------------
;; My Python Packages and Configs

;; elpy
;; https://github.com/jorgenschaefer/elpy
;; An ide for python
;; Usage: https://elpy.readthedocs.io/en/latest/ide.html#
(use-package elpy
  :ensure t
  :init  
  (elpy-enable)

  ;; To give support for the envs in python to elpy, dont' work
  ;; follow this url: https://github.com/jorgenschaefer/elpy/issues/1727
  ;; (setenv "WORKON_HOME" "/home/aleck/")
  
  (setq elpy-rpc-virtualenv-path "~/.emacs.d/elpy/rpc-venv/")
  :hook (elpy-mode . (lambda () (highlight-indentation-mode -1))))



;; -------------------------------------------------------------------------------------------
;; My Bash Packages and Configs

(defun my-bash-mode-hook ()
  "Custom settings for Bash mode."
  (setq indent-tabs-mode t)  ;; Use tabs for indentation
  (setq tab-width 'tab-width))        ;; Set tab width to 4 spaces (or your preferred width)

(add-hook 'sh-mode-hook 'my-bash-mode-hook)

;; -------------------------------------------------------------------------------------------
;; My Latex Package and Configs


;; auctex
;; https://elpa.gnu.org/packages/auctex.html
(use-package auctex
  :ensure t)


;; company-auctex
;; https://github.com/alexeyr/company-auctex/tree/9400a2ec7459dde8cbf1a5d50dfee4e300ed7e18
(use-package company-auctex
  :ensure t
  :init (company-auctex-init)
  )



;; -------------------------------------------------------------------------------------------
;; My Clisp Package and Configs

;; Install clisp and sbcl

;; slime
;; https://melpa.org/#/slime
;; https://github.com/slime/slime
;; Manual: https://slime.common-lisp.dev/doc/html/

;; (use-package slime
;;   :ensure t
;;   :init
;;   (setq inferior-lisp-program "sbcl")
;;   )


;; slime-company
;; https://github.com/anwyn/slime-company

;; (use-package slime-company
;;   :ensure t
;;   :init
;;   (slime-setup '(slime-fancy slime-company))
;;   )

;; -------------------------------------------------------------------------------------------
;; My Java Package and Configs


;; To keep 4 spaces identation
;; (defun my-java-mode-hook ()
;;   "Custom Java mode hook."
;;   (setq-local c-basic-offset 4))

;; (add-hook 'java-mode-hook 'my-java-mode-hook)

;; -------------------------------------------------------------------------------------------
;; My forth Package and Configs

;; Install forth with
;; yay -S gforth

;; forth-mode
;; https://github.com/larsbrinkhoff/forth-mode
;; To start an interactive Forth session, type M-x run-forth.
;; (use-package forth-mode
;;   :ensure t)
;; (use-package forth-block-mode
;;   :ensure 
;; (use-package forth-interaction-mode
;;   :ensure t)

;; -------------------------------------------------------------------------------------------
;; My csharp-mode Package and Configs
;; (use-package csharp-mode
;;   :ensure t)



;; -------------------------------------------------------------------------------------------
;; My Javascript Package and Configs

;; To set the identation of 
;; (setq js-indent-level 2)

;; Install and configure web-mode for .tsx files
(use-package web-mode
  :ensure t
  :mode ("\\.tsx?$" . web-mode)  ; Use web-mode for .ts, .tsx files
  :config
  (add-hook 'web-mode-hook #'yas-minor-mode)  ; Enable yasnippet in web-mode
  (setq web-mode-enable-auto-quoting nil)     ; Disable auto-quoting in JSX/TSX
  :custom
  (web-mode-markup-indent-offset 2)  ; Set indentation for markup
  (web-mode-css-indent-offset 2)     ; Set indentation for CSS
  (web-mode-code-indent-offset 2))   ; Set indentation for code

(use-package rjsx-mode
  :ensure t
  :mode ("\\.jsx?$" . rjsx-mode)  ; Automatically use rjsx-mode for .js and .jsx files
  :config
  (add-hook 'rjsx-mode-hook #'yas-minor-mode)  ; Enable yasnippet in rjsx-mode
  (add-hook 'rjsx-mode-hook
            (lambda ()
              (setq js-indent-level 2))))  ; Set JS indentation level to 2 for rjsx-mode


(use-package react-snippets
  :ensure t
  :after (yasnippet) ; Ensure yasnippet is loaded first
  :config
  (add-hook 'rjsx-mode-hook #'yas-minor-mode)
  ) ; Enable yasnippet in rjsx-mode


(use-package react-snippets
  :ensure t
  :after (yasnippet) ; Ensure yasnippet is loaded first
  :config
  (add-hook 'rjsx-mode-hook #'yas-minor-mode)
  ) ; Enable yasnippet in rjsx-mode



;; -------------------------------------------------------------------------------------------
;; My Cucumber Package and Configs

(use-package feature-mode
  :ensure t
  :mode ("\\.feature\\'" . feature-mode)
  :config
  (setq feature-default-language "en")
  (add-hook 'feature-mode-hook
            (lambda ()
	      ;; Use spaces instead of tabs
              (setq tab-width 2)
              (setq indent-tabs-mode nil)
              )))






;; -------------------------------------------------------------------------------------------
;; My Jenkins Package and Configs

(use-package jenkinsfile-mode
  :ensure t
  :mode ("Jenkinsfile\\'" . jenkinsfile-mode))

(message ".emacs loaded correctly")
