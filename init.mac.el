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

;; Enable pixel-precise scrolling
(pixel-scroll-precision-mode 1)
;; Don't auto-scroll back to the left when typing
(setq auto-hscroll-mode 'current-line) ;; For Emacs 28+
;; For Emacs < 28 use this instead:
;; (setq auto-hscroll-mode nil)

;; Make sure horizontal scrolling affects whole buffer
(setq scroll-preserve-screen-position t)

(add-to-list 'default-frame-alist '(horizontal-scroll-bars . bottom))


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
;; (set-frame-parameter nil 'alpha-background 90)
;; (add-to-list 'default-frame-alist '(alpha-background . 90))

;; Ensure new frames created by the emacsclient also inherit the transparency setting
;; (defun set-frame-transparency (frame)
;;   (set-frame-parameter frame 'alpha-background 90))

;; (add-hook 'after-make-frame-functions 'set-frame-transparency)


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

;; Disable the ring bell
(setq ring-bell-function 'ignore)




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



;; Enable Shift + Wheel scroll for horizontal scrolling
(defun my/scroll-left (&optional arg)
  (interactive "p")
  (scroll-left (* 15 (or arg 1))))  ;; Scroll 15 columns instead of 1

(defun my/scroll-right (&optional arg)
  (interactive "p")
  (scroll-right (* 15 (or arg 1)))) ;; Scroll 15 columns instead of 1


(global-set-key (kbd "<S-wheel-left>") #'my/scroll-right)
(global-set-key (kbd "<S-wheel-right>") #'my/scroll-left)

(setq hscroll-step 5)     ;; scroll by 5 columns when needed
(setq hscroll-margin 1)   ;; start scrolling 1 column before edge
(setq auto-hscroll-mode 'current-line) ;; only scroll when the cursor moves past edge



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


;; Activasting windmove to move between the windows 
(windmove-default-keybindings) ;; Shift + arrow keys


(require 'ansi-color)

(defun my/compilation-mode-colorize ()
  "Interpret ANSI color codes in compilation buffer."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))

(add-hook 'compilation-filter-hook 'my/compilation-mode-colorize)


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
   '(apheleia cmake-ide company consult diff-hl ellama fixmee flycheck
	      iedit jest lsp-java lsp-ui magit-todos multi-vterm
	      multiple-cursors mvn projectile pyvenv rg rjsx-mode smex
	      super-save typescript-mode yasnippet-snippets))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-change ((t (:foreground "#ffff00" :background "#775500"))))
 '(diff-hl-delete ((t (:foreground "#ff0000" :background "#770000"))))
 '(diff-hl-insert ((t (:foreground "#00ff00" :background "#007700")))))


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


;; diff-hl
;; To highlit the differences within a file in git
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
         (text-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))


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
	      ))
  (setq flycheck-java-language-server 'jdtls)
  )


;; projectile
;; https://docs.projectile.mx/projectile/index.html
;; usage: https://docs.projectile.mx/projectile/usage.html
(use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (setq projectile-project-search-path '("~/Documents/projects/"))
  :config
  (projectile-mode +1)
  (setq projectile-project-root-files-bottom-up (append '("pom.xml") projectile-project-root-files-bottom-up)))

;; rg
;; https://github.com/dajva/rg.el
;; For searching in files and that kind a stuff
;; Usage: https://rgel.readthedocs.io/en/latest/usage.html#searching
(use-package rg
  :ensure t
  :config (rg-enable-default-bindings))

;; consult
;; https://github.com/minad/consult
;; For quick and fast search
(use-package consult
  :ensure t)

(global-set-key [f4] 'consult-ripgrep)



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
  :config
  (defun run-amazon-q-multi-vterm ()
    "Run Amazon Q CLI in a dedicated multi-vterm buffer."
    (interactive)
    (let ((buffer-name "*amazon-q*"))
      ;; Create a new multi-vterm if one doesn't exist or reuse existing one
      (if (get-buffer buffer-name)
          (switch-to-buffer buffer-name)
        (progn
          (multi-vterm)
          (rename-buffer buffer-name)
          ;; Start Amazon Q CLI
          (vterm-send-string "q chat\n")))))
  ;; Optional keybinding
  (global-set-key (kbd "C-c q") 'run-amazon-q-multi-vterm)
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

;; which-key: To throw information of a shortcut
;; https://github.com/justbur/emacs-which-ke
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
;; https://github.com/bbatsov/super-save
(use-package super-save
  :ensure t
  :config
  (super-save-mode 1)
  (setq super-save-auto-save-when-idle t)) ;; Save when idle

;; languagetool: Spell checker
;; (use-package languagetool
;;   :ensure t
;;   :defer t
;;   :commands (languagetool-check
;;              languagetool-clear-suggestions
;;              languagetool-correct-at-point
;;              languagetool-correct-buffer
;;              languagetool-set-language
;;              languagetool-server-mode
;;              languagetool-server-start
;;              languagetool-server-stop))

;; (setq languagetool-java-arguments '("-Dfile.encoding=UTF-8")
;;         languagetool-console-command "~/.languagetool/languagetool-commandline.jar"
;;         languagetool-server-command "~/.languagetool/languagetool-server.jar"
;; 	languagetool-supported-languages '("en-US" "es")
;; 	languagetool-language "en-US"
;; 	)

;; Start the LanguageTool server after Emacs is initialized
;; (add-hook 'after-init-hook #'languagetool-server-start)

;; Stop the LanguageTool server when Emacs is exited
;; (add-hook 'kill-emacs-hook #'languagetool-server-stop)

;; lsp-mode: The lenguage server for c/c++, python, java
;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((c-mode c++-mode python-mode java-mode js-mode typescript-mode tsx-ts-mode js-ts-mode web-mode) . lsp-deferred)
  ;; :hook ((prog-mode . lsp-deferred)
  ;;        (lsp-mode . lsp-enable-which-key-integration))
  :config
  ;; Set the path to clangd for C/C++
  (setq lsp-clients-clangd-executable "clangd")
  ;; Use flycheck instead of flymake
  (setq lsp-prefer-flymake nil))

(with-eval-after-load 'lsp-mode
  ;; Disable verbose logging of LSP input/output
  (setq lsp-log-io nil)

  ;; Suppress warning about ambiguous project root
  (setq lsp-message-project-root-warning t)

  ;; Suppress "Unknown notification" warnings (like semgrep/rulesRefreshed)
  (setq lsp--log-unknown-message-function #'ignore))


;; lsp-ui: A simple ui for the lenguage server
;; https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-doc-enable t
        lsp-ui-doc-delay 0.3))

;; lsp-mode needs of button-lock
(use-package button-lock
  :ensure t)


;; ellama: To have a chatbut in the editor,
;; https://github.com/s-kostyaev/ellama
;; note you will need to install ollama https://ollama.com/download
(use-package ellama
  :ensure t
  :bind ("C-c e" . ellama)
  ;; send last message in chat buffer with C-c C-c
  :hook (org-ctrl-c-ctrl-c-final . ellama-chat-send-last-message)
  :init (setopt ellama-auto-scroll t)
  :config
  ;; show ellama context in header line in all buffers
  (ellama-context-header-line-global-mode +1)
  ;; show ellama session id in header line in all buffers
  (ellama-session-header-line-global-mode +1))

;; -------------------------------------------------------------------------------------------
;; My C/C++ Packges and Configs

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

;; ;; Adding python to the lsp server, 
;; (add-hook 'python-mode-hook #'lsp)
;; (setq lsp-pylsp-server-command "~/.emacs.d/lsp-python-venv/bin/pylsp")


;; pyvenv: To manage the python enviroments
(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1))  ;; Automatically enable pyvenv when Emacs starts

;; -------------------------------------------------------------------------------------------
;; My Java Packges and Configs


;; # JDK (21+ recommended) Otherwise doens't work
;; sudo pacman -S jdk-openjdk  # Arch Linux
;; # sudo apt install openjdk-21-jdk  # Debian/Ubuntu

;; # Maven
;; sudo pacman -S maven         # Arch Linux
;; # sudo apt install maven     # Debian/Ubuntu

;; yay -S jdtls  # Install eclipse language server
;; /usr/share/java/jdtls/bin/jdtls

;; # Minimal Working Config (Emacs init.el)
;; ;; LSP + Java
;; (use-package lsp-mode
;;   :ensure t
;;   :hook (java-mode . lsp))
;; (use-package lsp-java :ensure t :after lsp-mode)


(use-package lsp-java
  :ensure t
  :after lsp-mode
  :init
  ;; ====== CRITICAL FIX ======
  (setq lsp-java-java-path "/usr/lib/jvm/java-21-openjdk/bin/java")  ; Explicit Java 21
  :config
  ;; Set JDTLS server path (point to your installation)
  (setq lsp-java-server-install-dir "/usr/share/java/jdtls/"
        lsp-java-workspace-dir (expand-file-name "~/.emacs.d/java-workspace"))  ; Optional: Set workspace dir

  ;; Configure JVM args for performance
  (setq lsp-java-vmargs
        (list "-noverify"
              "-Xmx2G"
              "-XX:+UseG1GC"
              "-XX:+UseStringDeduplication"
              "--add-opens=jdk.compiler/com.sun.tools.javac.api=ALL-UNNAMED"
              (concat "-Dlombok.path=" (expand-file-name "~/.m2/repository/org/projectlombok/lombok/"))))
  
  ;; Java LSP settings
  (setq lsp-java-completion-enabled t
        lsp-java-format-enabled t
        lsp-java-import-order ["java" "javax" "org" "com"]
        lsp-java-save-actions-organize-imports t
        lsp-java-autobuild-enabled t
        lsp-java-content-provider-preferred "fernflower")

  ;; Configure JDK runtime (replace path with your JDK 17/21)
  (setq lsp-java-configuration-runtimes
        '[(:name "JavaSE-21" :path "/usr/lib/jvm/java-21-openjdk/" :default t)])

  ;; Start LSP when opening Java files
  (add-hook 'java-mode-hook #'lsp)
  
  (setq lsp-java-format-on-type-enabled nil)  ; Disable format on typing
  (setq lsp-java-format-enabled nil)         ; Disable all formatting (if needed)
  )


(use-package mvn
  :ensure t
  :after maven-test-mode
  :commands (mvn-clean mvn-compile mvn-install))

;; -------------------------------------------------------------------------------------------
;; My TypeScript/TSX/JavaScript/JSX Pakcages and Configs

;; Tree sitter for 
(when (fboundp 'treesit-available-p)
  (setq major-mode-remap-alist
        '((js-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)))

  
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

  (setq treesit-language-source-alist
        '((javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "main" "typescript/src")
          (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "main" "tsx/src")))

  (dolist (lang '(javascript typescript tsx))
    (unless (treesit-language-available-p lang)
      (treesit-install-language-grammar lang))))


(with-eval-after-load 'treesit
  (setq typescript-ts-mode-indent-offset 4))


;; Jest configuration for running tests
(use-package jest
  :ensure t
  :after (js2-mode typescript-mode)
  :hook ((js2-mode . jest-minor-mode)
         (typescript-mode . jest-minor-mode))
  :config
  ;; Set project root detection
  (setq jest-project-root-function
        (lambda ()
          (locate-dominating-file default-directory "package.json")))
  
  ;; Configure test file patterns
  (setq jest-test-file-extensions '("js" "jsx" "ts" "tsx"))
  
  ;; Key bindings for Jest commands
  (define-key jest-minor-mode-map (kbd "C-c j t") 'jest-file)
  (define-key jest-minor-mode-map (kbd "C-c j s") 'jest-function)
  (define-key jest-minor-mode-map (kbd "C-c j r") 'jest-repeat)
  (define-key jest-minor-mode-map (kbd "C-c j p") 'jest-popup))


(setq js-indent-level 4)
(setq sgml-basic-offset 4)


;; init.el ends here


