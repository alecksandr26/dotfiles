;; -------------------------------------------------------------------------------------------
;; Init default configs

;; Do not show the startup screen.
(setq inhibit-startup-message t)

;; Disable tool bar, menu bar, scroll bar.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Remove backup files
;; Config your backupfiles:
;; https://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
;; https://emacs.stackexchange.com/questions/33/put-all-backups-into-one-backup-folder
(setq backup-directory-alist '(("." . "~/.emacs-backup")))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs-saves/" t)))

;; Add the column numbers
(setq column-number-mode t)

;; Display the number lines
(global-display-line-numbers-mode 1)

;; To compile code or to run commands
(global-set-key [f5] 'compile)

(defvar previous-shell-command nil
  "Variable to store the previous shell command.")

(defun async-shell-command-with-previous ()
  "Run `async-shell-command` with the previously used command, if any."
  (interactive)
  (let ((command (read-shell-command "Shell command: " previous-shell-command)))
    (setq previous-shell-command command)
    (async-shell-command command)))

(global-set-key [f6] 'async-shell-command-with-previous)

;; To open a new frame of emacs
(global-set-key (kbd "C-c C-f") 'display-buffer-other-frame)

;; Load my theme manually
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; https://github.com/oneKelvinSmith/monokai-emacs
(load-theme 'monokai t)
;; https://github.com/sjrmanning/darkokai/tree/5820aeddfc8c869ba840cc534eba776936656a66
;; (load-theme 'darkokai t)
;; https://github.com/dawidof/emacs-monokai-theme/tree/f342b6afc31f929be0626eca2d696ee9fab78011
;; (load-theme 'monokai-alt t)
;; https://github.com/belak/emacs-monokai-pro-theme/tree/d56fa38a9ed3b1d8e4f8401cb4c3f08073f3ba26
(load-theme 'monokai-pro t)
(load-theme 'monokai-pro-classic t)

;; Set the font
(set-frame-parameter nil 'font "Source Code Pro SemiBold-10")

;; My code style is linux kernel coding style
;; https://www.kernel.org/doc/html/v4.10/process/coding-style.html
;; https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines

(setq-default indent-tabs-mode t)
(setq-default tab-width 8)
(defvaralias 'c-basic-offset 'tab-width)
(setq backward-delete-char-untabify-method 'nil)

;; dired
;; To add a new file to the current listed direcotry 
(defun my-dired-add-file-here ()
  "Add a new file in the current directory in Dired mode."
  (interactive)
  (let ((dir (dired-current-directory)))
    (find-file (expand-file-name (read-string "New file name: " nil nil "new_file") dir))))

(global-set-key (kbd "C-+") 'my-dired-add-file-here)

;; ido
;; https://www.gnu.org/software/emacs/manual/html_mono/ido.html#Overview
(require 'ido)
(ido-mode t)

;; Enable electric-pair-mode for automatic insertion of matching parentheses
(electric-pair-mode 1)

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
    (message "Loading in regular Emacs!"))


;; org mode
;; My org mode configuration
(require 'org)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.4))

;; -------------------------------------------------------------------------------------------
;; Initialized the package manager
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("bf72d370fd0c2f47c632cd7314b1b9b7e0c79900c1947d0794ac2ecebb5ed584" default))
 '(package-selected-packages
   '(company-c-headers forth-mode slime-company slime company-auctex auctex yasnippet-snippets multi-vterm vterm smex elpy cmake-mode cmake-ide rtags projectile hl-todo flycheck company fixmee magit multiple-cursors iedit)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; -------------------------------------------------------------------------------------------
;; General package configs

;; company
;; https://company-mode.github.io/
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
;; Remove company-capf becuase it doesn't works
(setq company-backends (delete 'company-semantic (delete 'company-capf company-backends)))

;; magit
;; https://github.com/magit/magit/tree/2ed93504778c9ec2b8f56665cbdeae348146fbf7
;; Getting Started: https://magit.vc/manual/magit/Getting-Started.html

;; magit-todos IS BROKEN... : (,  'try again in the future'
;; https://github.com/alphapapa/magit-todos/tree/debb77b3589f2d83c8b43706edc1f8f90bf1ad91

;; ;; flycheck
;; ;; https://github.com/flycheck/flycheck/tree/7ee95638c64821e9092a40af12b1095aa5604fa5
(require 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)

;; projectile
;; https://docs.projectile.mx/projectile/index.html
;; usage: https://docs.projectile.mx/projectile/usage.html
(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-project-search-path '("~/Documents/projects/"))
(projectile-mode +1)

;; ;; multiple-cursors
;; ;; https://github.com/magnars/multiple-cursors.el
(require 'multiple-cursors)

;; ;; When you want to add multiple cursors not based on continuous lines,
;; ;; but based on keywords in the buffer, use:
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; ;; yasnippet
;; ;; https://github.com/joaotavora/yasnippet
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

;; yasnippet-snippets
;; https://github.com/AndreaCrotti/yasnippet-snippets

;; ggtags, alternative to rtags
;; https://elpa.gnu.org/packages/ggtags.html
;; Download global here: https://www.gnu.org/software/global/
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

;; iedit configuration
;; https://github.com/victorhge/iedit/tree/dd5d75b38ee0c52ad81245a8e5c932d3f5c4772d
(require 'iedit)
(global-set-key (kbd "C-c ;") 'iedit-mode)

;; vterm
;; https://github.com/akermu/emacs-libvterm
;; usage; 'C-c t' To change the modes between the copy and paste
(require 'vterm)

;; multi-vterm
;; https://github.com/suonlight/multi-vterm
(require 'multi-vterm)

;; smex
;; https://github.com/nonsequitur/smex/
(require 'smex)
;; Can be omitted. This might cause a (minimal) delay when Smex is auto-initialized on its first run.
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; fixme
;; https://github.com/rolandwalker/fixmee
(require 'fixmee)
(require 'button-lock)
(add-hook 'after-init-hook 'global-fixmee-mode)

;; hl-todo
;; https://github.com/tarsius/hl-todo/tree/7146bbcab5248f3fb9d09acb981b8e63f0c73413
(require 'hl-todo)
(setq hl-todo-keyword-faces
      '(("TODO"   . "#e4db73")
        ("FIXME"  . "#f82570")
        ("DEBUG"  . "#ae81ff")
        ("GOTCHA" . "#fc961f")
        ("STUB"   . "#66d9ee")))
(add-hook 'after-init-hook 'global-hl-todo-mode)

;; -------------------------------------------------------------------------------------------
;; My C/C++ Packges and Configs

;; company-c-headers
;; https://github.com/randomphrase/company-c-headers
(require 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-c-headers-path-system "/usr/include/")
(add-to-list 'company-c-headers-path-system "/usr/include/c++/13.2.1/")

;; rtags
;; Install manually: https://github.com/Andersbakken/rtags#tldr-quickstart
;; Usage: https://github.com/Andersbakken/rtags/wiki/Usage
(require 'rtags)
(add-hook 'after-init-hook 'rtags-start-process-unless-running)
;; Adding the keybingds
(rtags-enable-standard-keybindings)

;; cmake-ide
;; https://github.com/atilaneves/cmake-ide
;; Starts the server 'rdm' and the 'rc -J' if a 'compile_commands.json' exist
(cmake-ide-setup)

;; cmake-mode
;; https://melpa.org/#/cmake-mode
;; A simple highlighter for cmake files
(require 'cmake-mode)

;; -------------------------------------------------------------------------------------------
;; My Python Packages and Configs

;; elpy
;; https://github.com/jorgenschaefer/elpy
;; An ide for python
(require 'elpy)
(elpy-enable)
(add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))

;; To enable flycheck for python, install pylint
;; https://pylint.org/
;; guide: https://pylint.readthedocs.io/en/latest/user_guide/usage/output.html
;; arch: pacman -S python-pylint  # if you live in the future

(defun my-turn-off-flymake-mode ()
  "Turn off Flymake mode."
  (flymake-mode -1))

;; To set the indentation style for multi-line function calls
(add-hook 'find-file-hook #'my-turn-off-flymake-mode)

;; For The identation in function arguments
;; https://superuser.com/questions/537366/in-emacs-how-do-i-align-closing-parentheses-with-the-start-of-the-opening-line
;; Set the indentation style for multi-line function calls in Python
(add-hook 'python-mode-hook
          (lambda ()
            (setq python-indent-offset 4) ; Set Python indentation to 4 spaces
            (setq tab-width 4) ; Set tab width to 4 spaces
            (setq python-indent-guess-indent-offset nil) ; Disable guessing indentation offset
            (setq python-indent-def-block-scale 1) ; Set the default indentation scale for blocks
            (setq python-indent-def-block-scale-default 1) ; Set the default indentation scale for blocks to 1
            (setq python-indent-first-label-offset -4))) ; Set the indentation offset for the first label

;; -------------------------------------------------------------------------------------------
;; My Latex Packages and Configs

;; auctex
;; https://elpa.gnu.org/packages/auctex.html
(require 'auctex)

;; company-auctex
;; https://github.com/alexeyr/company-auctex/tree/9400a2ec7459dde8cbf1a5d50dfee4e300ed7e18
(require 'company-auctex)
(company-auctex-init)

;; -------------------------------------------------------------------------------------------
;; My Clisp Packages and Configs

;; Install clisp and sbcl

;; slime
;; https://melpa.org/#/slime
;; https://github.com/slime/slime
(require 'slime)
(setq inferior-lisp-program "sbcl")

;; slime-company
;; https://github.com/anwyn/slime-company
(slime-setup '(slime-fancy slime-company))

;; -------------------------------------------------------------------------------------------
;; Other languages

;; forth-mode
;; https://github.com/larsbrinkhoff/forth-mode
;; To start an interactive Forth session, type M-x run-forth.

;; Install forth with
;; yay -S gforth
(require 'forth-mode)
(require 'forth-block-mode)
(require 'forth-interaction-mode)


;; csharp-mode
;; To do chapr in linux you need to install mono: https://wiki.archlinux.org/title/mono
;; sudo pacman -S mono
;; (require 'csharp-mode)


(message ".emacs loaded correctly")
