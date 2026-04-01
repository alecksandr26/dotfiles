;; -------------------------------------------------------------------------------------------
;; Init default configs

;; Do not show the startup screen.
;; (setq inhibit-startup-message t)

;; Disable tool bar, menu bar, scroll bar and truncate lines.
;; (tool-bar-mode -1)
(menu-bar-mode -1)


;; (scroll-bar-mode -1)
(setq-default truncate-lines t)


;; Config your backupfiles:
;; https://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
;; https://emacs.stackexchange.com/questions/33/put-all-backups-into-one-backup-folder
(setq backup-directory-alist '(("." . "~/.emacs.d/emacs-backup")))

;; Create the directory '.emacs-saves' for emacs saves
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/emacs-saves/" t)))


;; My code style is similar to linux kernel coding style
;; https://www.kernel.org/doc/html/v4.10/process/coding-style.html
;; https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines
;; (setq-default tab-width 8)

;; To make tbe backspace delete the tabs not convert the tab into spaces etc..
;; (setq backward-delete-char-untabify-method 'nil)


;; Enable electric-pair-mode for automatic insertion of matching parentheses
(electric-pair-mode 1)


;; Activating windmove to move between the windows 
(windmove-default-keybindings) ;; Shift + arrow key


;; Emacs as Deamon
;; https://www.emacswiki.org/emacs/EmacsAsDaemon
;; systemctl --user enable --now emacs
;; systemctl --user start --now emacs
;; check: https://unix.stackexchange.com/questions/56871/emacs-daemon-crashing-after-closing-emacsclient-c
(setq default-frame-alist '((font . "Source Code Pro SemiBold-10")))

;; Checking where Im running emacs
(if (daemonp)
    (message "[CONFIG-INFO] Loading in the daemon!")
  (message "[CONFIG-INFO] Loading in regular Emacs!"))


;; -------------------------------------------------------------------------------------------
;; Theme, Font, etc ... configs

;; Display the number lines
(global-display-line-numbers-mode 1)


;; Set the font
(set-frame-parameter nil 'font "Source Code Pro Semibold-10")


;; -------------------------------------------------------------------------------------------
;; Key bindings config

;; To compile code or to run commands
(global-set-key [f5] 'compile)


;; Shell command to be saved
(defvar previous-shell-command nil
  "Variable to store the previous shell command.")

(defun my/async-shell-command-with-previous ()
  "Run `async-shell-command` with the previously used command, if any."
  (interactive)
  (let ((command (read-shell-command "Shell command: " previous-shell-command)))
    (setq previous-shell-command command)
    (async-shell-command command)))

;; To run commands with input and output streams
(global-set-key [f6] 'my/async-shell-command-with-previous)


;; dired
;; To add a new file to the current listed direcotry 
(defun my/dired-add-file-here ()
  "Add a new file in the current directory in Dired mode."
  (interactive)
  (let ((dir (dired-current-directory)))
    (find-file (expand-file-name (read-string "New file name: " nil nil "new_file") dir))))

(global-set-key (kbd "C-+") 'my/dired-add-file-here)


(defun my-rc/duplicate-line ()
  "Duplicate current line"
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(global-set-key (kbd "C-,") 'my-rc/duplicate-line)


(defun my/open-init-file ()
  "Open my init.el quickly."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; -------------------------------------------------------------------------------------------
;; Native Packages

;; ido - To autocomplete the buffers names
;; https://www.gnu.org/software/emacs/manual/html_mono/ido.html#Overview
(require 'ido)
(ido-mode t)

;; org mode 
;; My rg mode configuration for taking notes
;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-python.html
(use-package org
  :ensure nil
  :config
  ;; For listing the sessions:
  ;; M-x list-processes

  ;; Enable org-tempo (<s TAB etc.)
  (require 'org-tempo)

  ;; Enable Python in org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))

  ;; Enable latex in org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((latex . t)))

  ;; Enable shifts  selects
  (setq org-support-shift-select t)

  ;; The size of the latex fragments
  (setq org-format-latex-options
	(plist-put org-format-latex-options :scale 2.0))

  ;; No confirmation before executing
  (setq org-confirm-babel-evaluate nil)

  ;; Default Python settings
  (setq org-babel-default-header-args:python
        '((:session . "main")))

  ;; Render the latex automatically the latex
  (setq org-startup-with-latex-preview t)

  ;; For latex
  ;; <l [TAB], for latex
  ;; C-c C-x C-l, for rendering or run the command (org-toggle-latex-fragment)
  ;; NOTE!!!!: you must to have `sudo pacman -S texlive-most` installed latex

  
  ;; Custom template for Python block
  ;; <py [TAB] -  for putting a python block
  (add-to-list 'org-structure-template-alist
               '("py" . "src python :session main :results output"))

  ;; <as [TAB] -  for putting a python async block 
  (add-to-list 'org-structure-template-alist
               '("as" . "src python :session main :results output :async yes"))

  ;; <plt [TAB] - for putting a plot
  (add-to-list 'org-structure-template-alist
               '("plt" . "src python :session main :results file link :file plot.png"))

  ;; <table [TAB] -  for putting a python block
  (add-to-list 'org-structure-template-alist
               '("table" . "src python :session main :results table"))
  ;; <m [TAB] - for latex
  (add-to-list 'org-structure-template-alist
             '("m" . "latex"))

  ;; To show inline images 
  (setq org-startup-with-inline-images t)
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local electric-pair-inhibit-predicate
                          (lambda (c)
                            (if (eq c ?<)
				t
                              (electric-pair-default-inhibit c))))))
  )



;; -------------------------------------------------------------------------------------------
;; Initialized the package manager

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Install 'use-package' for easier package management
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
   '(company diff-hl iedit magit-todos multi-vterm multiple-cursors
	     projectile pyvenv rust-mode smex super-save vterm
	     yasnippet yasnippet-snippets)))
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
  (setq company-minimum-prefix-length 1))


;; magit
;; https://github.com/magit/magit/tree/2ed93504778c9ec2b8f56665cbdeae348146fbf7
;; Getting Started: https://magit.vc/manual/magit/Getting-Started.html
(use-package magit
  :ensure t)


;; magit-todos
;; IMPORTANT: install ripgrep
;; https://github.com/alphapapa/magit-todos/tree/debb77b3589f2d83c8b43706edc1f8f90bf1ad91
(use-package magit-todos
  :ensure t
  :after magit
  :config (magit-todos-mode 1))

;; diff-hl
;; Highlight git changes directly in the buffer
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :ensure t

  :hook ((prog-mode . diff-hl-mode)
         (text-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (after-load-theme . diff-hl-update-face))

  ;; FACE DEFINITIONS (moved here cleanly)
  :custom-face
  (diff-hl-insert
   ((t (:foreground "#00ff00" :background "#007700"))))
  (diff-hl-change
   ((t (:foreground "#ffff00" :background "#775500"))))
  (diff-hl-delete
   ((t (:foreground "#ff0000" :background "#770000"))))

  :config
  ;; enable everywhere
  (global-diff-hl-mode))


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

;; iedit configuration
;; https://github.com/victorhge/iedit/tree/dd5d75b38ee0c52ad81245a8e5c932d3f5c4772d
(use-package iedit
  :ensure t
  :bind ("C-c ;" . iedit-mode))

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
;; https://andreacrotti.pro/yasnippet-snippets/snippets
(use-package yasnippet-snippets
  :ensure t)

;; vterm
;; https://github.com/akermu/emacs-libvterm
;; usage; 'C-c t' To change the modes between the copy and paste
(use-package vterm
  :ensure t)

;; multi-vterm
;; https://github.com/suonlight/multi-vterm
(use-package multi-vterm
  :ensure t)

;; hl-todo
;; https://github.com/tarsius/hl-todo/tree/7146bbcab5248f3fb9d09acb981b8e63f0c73413
(use-package hl-todo
  :ensure t
  :hook (after-init . global-hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
	'(("TODO"   . "#707070")  ;; Soft gray
          ("FIXME"  . "#d9534f")  ;; Muted red
          ("DEBUG"  . "#6c757d")  ;; Neutral gray
          ("GOTCHA" . "#f0ad4e")  ;; Light amber
          ("STUB"   . "#5bc0de")  ;; Cool blue
          ("NOTE"   . "#5a6268"))) ;; Muted dark gray
  )

;; super-save: Save automatically the files when switching between buffer
;; https://github.com/bbatsov/super-save
(use-package super-save
  :ensure t
  :config
  (super-save-mode 1)
  (setq super-save-auto-save-when-idle t)) ;; Save when idle



;; -------------------------------------------------------------------------------------------
;; My Python Packges and Configs

(defun my/python-install-package (package)
  "Install a Python PACKAGE using current python interpreter."
    (interactive "sInstall Python package: ")
    (let ((python python-shell-interpreter))
      (compile (format "%s -m pip install %s" python package))))

;; python: This is a packaged that is an IDE
(use-package python
  :ensure t
  :custom
  (python-shell-interpreter "python3")
  (python-indent-offset 4)
  (python-indent-guess-indent-offset nil))


;; pyvenv: To have venv with the python IDE
;; https://github.com/jorgenschaefer/pyvenv
(use-package pyvenv
  :config
  (pyvenv-mode 1))

;; -------------------------------------------------------------------------------------------
;; My Rust Packages and Configs

;; install rust analyzer: https://github.com/rust-lang/rust-analy
;; and move it /usr/local/bin/, to have analyzer
;; you may need to install rust-src
(use-package rust-mode
  :ensure t
  :hook (rust-mode . lsp-deferred)
  :config
  (setq lsp-auto-guess-root nil))


