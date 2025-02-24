;; -------------------------------------------------------------------------------------------
;; Init default configs

;; Do not show the startup screen.
(setq inhibit-startup-message t)

;; Disable tool bar, menu bar, scroll bar and truncate lines.
;; (tool-bar-mode -1)
(menu-bar-mode -1)
;; (scroll-bar-mode -1)
(scroll-bar-mode 1)
(setq-default truncate-lines t)

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
	     ;; Not disable the bar
             '(vertical-scroll-bars . 1)
	     )

;; Checking where Im running emacs
(if (daemonp)
    (message "Loading in the daemon!")
  (message "Loading in regular Emacs!")
  )


;; -------------------------------------------------------------------------------------------
;; Theme, Font, Trans, etc ... configs

;; Display the number lines
(global-display-line-numbers-mode 1)

;; Set the font
(set-frame-parameter nil 'font "Source Code Pro Semibold-10")

;; To make emacs transparent
(set-frame-parameter nil 'alpha-background 90)
(add-to-list 'default-frame-alist '(alpha-background . 90))

;; Ensure new frames created by the emacsclient also inherit the transparency setting
(defun set-frame-transparency (frame)
  (set-frame-parameter frame 'alpha-background 90))

(add-hook 'after-make-frame-functions 'set-frame-transparency)


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
;; (load-theme 'monokai-pro t)
;; (load-theme 'monokai-pro-classic t)



;; hide and show, to fold code
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Hideshow.html
(add-hook 'prog-mode-hook #'hs-minor-mode)

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
;; To set sizes for the images
(setq org-image-actual-width nil)

(require 'ob-latex)

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.4))


;; -------------------------------------------------------------------------------------------
;; Initialized the package manager

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package for easier package management
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(setq package-install-upgrade-built-in t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(languagetool pyvenv python-mode super-save cmake-ide dap-mode lsp-ui lsp-mode button-lockxs yasnippet-snippets yasnippet multiple-cursors projectile jenkinsfile-mode flycheck seq magit-todos magit company smex use-package))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


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

;; hl-todo
;; https://github.com/tarsius/hl-todo/tree/7146bbcab5248f3fb9d09acb981b8e63f0c73413
(use-package hl-todo
  :ensure t
  :hook (after-init . global-hl-todo-mode)
  :config
  ;; (setq hl-todo-keyword-faces
  ;;       '(("TODO"   . "#e4db73")
  ;;         ("FIXME"  . "#f82570")
  ;;         ("DEBUG"  . "#ae81ff")
  ;;         ("GOTCHA" . "#fc961f")
  ;;         ("STUB"   . "#66d9ee")))
  (setq hl-todo-keyword-faces
	'(("TODO"   . "#707070")  ;; Soft gray
          ("FIXME"  . "#d9534f")  ;; Muted red
          ("DEBUG"  . "#6c757d")  ;; Neutral gray
          ("GOTCHA" . "#f0ad4e")  ;; Light amber
          ("STUB"   . "#5bc0de")  ;; Cool blue
          ("NOTE"   . "#5a6268"))) ;; Muted dark gray
  )


;; The packaged theme
;; (use-package monokai-theme
;;   :ensure t
;;   :config
;;   (load-theme 'monokai t))

;; which-key
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; treemacs: A vertial list of files
;; https://github.com/Alexander-Miller/treemacs
(use-package treemacs
  :ensure t
  :config
  (global-set-key (kbd "C-c t t") 'treemacs) ;; Open Treemacs with C-c t t
  (global-set-key (kbd "C-c t d") 'treemacs-select-directory)) ;; Open Treemacs in a specific directory

;; super-save: Save automatically the files when switching between buffer
;; Install and enable super-save
(use-package super-save
  :ensure t
  :config
  (super-save-mode 1)
  (setq super-save-auto-save-when-idle t)) ;; Save when idle

;; languagetool: Spell checker
(use-package languagetool
  :ensure t
  :defer t
  :commands (languagetool-check
             languagetool-clear-suggestions
             languagetool-correct-at-point
             languagetool-correct-buffer
             languagetool-set-language
             languagetool-server-mode
             languagetool-server-start
             languagetool-server-stop))

(setq languagetool-java-arguments '("-Dfile.encoding=UTF-8")
        languagetool-console-command "~/.languagetool/languagetool-commandline.jar"
        languagetool-server-command "~/.languagetool/languagetool-server.jar"
	languagetool-supported-languages '("en-US" "es")
	languagetool-language "en-US"
	)

;; Start the LanguageTool server after Emacs is initialized
(add-hook 'after-init-hook #'languagetool-server-start)

;; Stop the LanguageTool server when Emacs is exited
(add-hook 'kill-emacs-hook #'languagetool-server-stop)



;; -------------------------------------------------------------------------------------------
;; My C/C++ Packges and Configs


;; lsp-mode: The lenguage server for c/c++
;; https://github.com/emacs-lsp/lsp-mode

;; lsp-mode needs of button-lock
(use-package button-lock
  :ensure t)

(use-package lsp-mode
  :hook ((c-mode c++-mode) . lsp) ;; Enable lsp for both C and C++ modes
  :config
  (setq lsp-clients-clangd-executable "clangd") ;; Path to clangd, adjust if needed
  (setq lsp-prefer-flymake nil)) ;; Use flycheck instead of flymake


;; lsp-ui: A simple ui for the lenguage server
;; https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-doc-enable t
        lsp-ui-doc-delay 0.3))

;; dap-mode: For debugging the program
;; https://github.com/emacs-lsp/dap-mode
(use-package dap-mode
  :after lsp-mode
  :ensure t
  :config
  ;; Enable dap-ui for debugger interface
  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup)
  (require 'dap-ui)
  (dap-ui-mode 1)

  ;; Keybindings for debugging actions
  (global-set-key (kbd "C-c d d") 'dap-debug)            ;; Start debugging session
  (global-set-key (kbd "C-c d b") 'dap-breakpoint-toggle) ;; Toggle breakpoint at current line
  (global-set-key (kbd "C-c d c") 'dap-continue)         ;; Continue to next breakpoint
  (global-set-key (kbd "C-c d n") 'dap-next)             ;; Step over
  (global-set-key (kbd "C-c d i") 'dap-step-in)          ;; Step in
  (global-set-key (kbd "C-c d o") 'dap-step-out)         ;; Step out
  (global-set-key (kbd "C-c d q") 'dap-disconnect)       ;; Exit/Disconnect debugging session

  ;; Additional keybindings for UI
  (global-set-key (kbd "C-c d l") 'dap-ui-locals)        ;; Show locals view
  (global-set-key (kbd "C-c d s") 'dap-ui-sessions)      ;; Show active sessions
  (global-set-key (kbd "C-c d t") 'dap-ui-threads)       ;; Show threads view
  (global-set-key (kbd "C-c d r") 'dap-ui-repl)          ;; Open REPL
)

;; cmake-ide: To give support for cmake projects
;; https://github.com/atilaneves/cmake-ide
(use-package cmake-ide
  :config
  (cmake-ide-setup))


(defun my-c-c++-mode-hook ()
  (setq c-basic-offset tab-width) 
  (setq indent-tabs-mode t) ; Use tabs instead of spaces
  )

(add-hook 'c-mode-hook 'my-c-c++-mode-hook)
(add-hook 'c++-mode-hook 'my-c-c++-mode-hook)


;; -------------------------------------------------------------------------------------------
;; My Python Packges and Configs

;; python: This is a packaged that is an IDE
(use-package python
  :ensure t
  :custom
  (python-shell-interpreter "python3"))

;; 'run-python' command will execute the interpreter

;; Adding python to the lsp server, 
(add-hook 'python-mode-hook #'lsp)
(setq lsp-pylsp-server-command "~/.emacs.d/lsp-python-venv/bin/pylsp")


;; pyvenv: To manage the python enviroments
(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1))  ;; Automatically enable pyvenv when Emacs starts


;; init.el ends here


